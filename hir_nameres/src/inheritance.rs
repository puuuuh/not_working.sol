use base_db::{BaseDb, Project};
use hir_def::{ContractId, ContractType, Item};
use salsa::tracked;
use smallvec::SmallVec;

use crate::scope::{body::Definition, HasScope};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, salsa::Update)]
pub enum LinearizationError {
    Cycle,
    Merge
}

fn linearization_recovery<'db>(db: &'db dyn BaseDb, value: &Result<Vec<ContractId<'db>>, LinearizationError>, _count: u32, project: Project, c: ContractId<'db>) -> salsa::CycleRecoveryAction<Result<Vec<ContractId<'db>>, LinearizationError>> {
    salsa::CycleRecoveryAction::Iterate
}

fn linearization_initial<'db>(_db: &dyn BaseDb, project: Project, c: ContractId<'db>) -> Result<Vec<ContractId<'db>>, LinearizationError> {
    Err(LinearizationError::Cycle)
}

fn merge<'db>(db: &'db dyn BaseDb, mut data: Vec<Vec<ContractId<'db>>>) -> Result<Vec<ContractId<'db>>, LinearizationError> {
    let mut res = Vec::with_capacity(10);
    while data.len() > 0 {
        let mut next = None;
        for l in data.iter().rev() {
            next = l.last().copied();
            if let Some(next) = next {
                if data.iter().all(|l| l[l.len() - 1] == next || !l[..(l.len() - 1)].contains(&next)) {
                    break;
                }
            }
            next = None;
        }

        if let Some(next) = next {
            for l in &mut data {
                l.pop_if(|x| *x == next);
            }
            data.retain(|a| !a.is_empty());
            res.push(next);
        } else {
            return Err(LinearizationError::Merge);
        }
    }

    res.reverse();

    Ok(res)
}

#[salsa::tracked(cycle_initial = linearization_initial, cycle_fn = linearization_recovery)]
pub fn linearization<'db>(db: &'db dyn BaseDb, project: Project, c: ContractId<'db>) -> Result<Vec<ContractId<'db>>, LinearizationError> {
    let mut chain = Vec::with_capacity(5);
    let parents = c.inheritance_chain(db);
    if !parents.is_empty() {
        let mut data = vec![vec![]];
        for p in parents.into_iter() {
            let path = &p.path.0;
            let scope = c.scope(db, project);
            if let Some(Definition::Item(Item::Contract(p))) = scope.lookup_path(db, path) {
                if (p.kind(db) == ContractType::Contract) {
                    data[0].push(p);
                    data.push(linearization(db, project, p)?);
                }
            }
        }
        chain = merge(db, data)?;
    }
    chain.push(c);
    Ok(chain)
}

#[cfg(test)]
mod tests {
    use base_db::{Project, TestDatabase, TestFixture};
    use hir_def::HasSourceUnit;
    use salsa::Database;
    use crate::inheritance::{linearization, LinearizationError};

    #[test]
    fn merge_test() {
        let fixture = TestFixture::parse(r"
            contract O {}

            contract A is O {}
            contract B is O {}
            contract C is O {}
            contract D is K1 {}
            contract E is O {}

            contract K1 is C,A,B {}
            contract K3 is A,D {}
            contract K2 is B,D,E {}

            contract Z is K1,K3,K2 {}
        ");
        let (db, file) = TestDatabase::from_fixture(fixture);
        let source_unit = file.source_unit(&db);
        let data = source_unit.data(&db);
        let c = *data.contracts(&db).last().unwrap();
        assert_eq!(linearization(&db, Project::new(&db, vfs::VfsPath::from_virtual("".to_owned())), c), Err(LinearizationError::Merge));
    }

    #[test]
    fn cycle_test() {
        let fixture = TestFixture::parse(r"
            contract O {}

            contract A is O {}
            contract B is O {}
            contract C is Z {}
            contract D is O {}
            contract E is O {}

            contract K1 is C,A,B {}
            contract K3 is A,D {}
            contract K2 is B,D,E {}

            contract Z is K1,K3,K2 {}
        ");
        let (db, file) = TestDatabase::from_fixture(fixture);
        let source_unit = file.source_unit(&db);
        let data = source_unit.data(&db);
        let c = *data.contracts(&db).last().unwrap();
        assert_eq!(linearization(&db, Project::new(&db, vfs::VfsPath::from_virtual("".to_owned())), c), Err(LinearizationError::Cycle));
    }

    #[test]
    fn linearization_test() {
        let fixture = TestFixture::parse(r"
            contract O {}

            contract A is O {}
            contract B is O {}
            contract C is O {}
            contract D is O {}
            contract E is O {}

            contract K1 is C,A,B {}
            contract K3 is A,D {}
            contract K2 is B,D,E {}

            contract Z is K1,K3,K2 {}
        ");
        let (db, file) = TestDatabase::from_fixture(fixture);
        let source_unit = file.source_unit(&db);
        let data = source_unit.data(&db);
        let c = *data.contracts(&db).last().unwrap();

        db.attach(|_| {
            for c in linearization(&db, Project::new(&db, vfs::VfsPath::from_virtual("".to_owned())), c).unwrap() {
                dbg!(c.name(&db));
            }
        });
    }
}