use base_db::{BaseDb, Project};
use hir_def::{lower_file, ContractId, ContractType, Item};
use salsa::tracked;
use smallvec::SmallVec;
use tracing::warn;

use crate::scope::{HasScope, body::Definition};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, salsa::Update)]
pub enum LinearizationError {
    Cycle,
    Merge,
}

fn linearization_recovery<'db>(
    db: &'db dyn BaseDb,
    value: &Result<Vec<ContractId<'db>>, LinearizationError>,
    _count: u32,
    project: Project,
    c: ContractId<'db>,
) -> salsa::CycleRecoveryAction<Result<Vec<ContractId<'db>>, LinearizationError>> {
    salsa::CycleRecoveryAction::Iterate
}

fn linearization_initial<'db>(
    _db: &dyn BaseDb,
    project: Project,
    c: ContractId<'db>,
) -> Result<Vec<ContractId<'db>>, LinearizationError> {
    Err(LinearizationError::Cycle)
}

fn merge<'db>(
    db: &'db dyn BaseDb,
    mut data: Vec<Vec<ContractId<'db>>>,
) -> Result<Vec<ContractId<'db>>, LinearizationError> {
    let mut res = Vec::with_capacity(10);
    while data.len() > 0 {
        let mut next = None;
        for l in data.iter().rev() {
            next = l.last().copied();
            if let Some(next) = next {
                if data
                    .iter()
                    .all(|l| l[l.len() - 1] == next || !l[..(l.len() - 1)].contains(&next))
                {
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
fn inheritance_chain_cycle<'db>(
    db: &'db dyn BaseDb,
    project: Project,
    c: ContractId<'db>,
) -> Result<Vec<ContractId<'db>>, LinearizationError> {
    cov_mark::hit!(hir_nameres_inheritance_chain);
    let mut chain = Vec::with_capacity(5);
    let parents = c.inheritance_chain(db);
    if !parents.is_empty() {
        let mut data = vec![vec![]];
        for p in parents.into_iter() {
            let path = &p.path.0;
            let scope = c.origin(db).map(|c| c.scope(db, project))
                .unwrap_or_else(|| { lower_file(db, c.file(db)).scope(db, project) });
            if let Some(Definition::Item(Item::Contract(p))) = scope.lookup_path(db, path) {
                data[0].push(p);
                data.push(inheritance_chain_cycle(db, project, p)?);
            }
        }
        chain = merge(db, data)?;
    }
    chain.push(c);
    Ok(chain)
}

#[salsa::tracked]
pub fn inheritance_chain<'db>(
    db: &'db dyn BaseDb,
    project: Project,
    c: ContractId<'db>,
) -> Vec<ContractId<'db>> {
    match inheritance_chain_cycle(db, project, c) {
        Ok(mut data) => {
            data.reverse();
            data
        },
        Err(e) => {
            vec![c]
        },
    }
}

#[cfg(test)]
mod tests {
    use crate::inheritance::{LinearizationError, inheritance_chain};
    use base_db::{Project, TestDatabase, TestFixture};
    use hir_def::lower_file;
    use salsa::{Database, Setter};
    use tracing::{level_filters::LevelFilter, warn};

    #[test]
    fn example_test() {
        let fixture = TestFixture::parse(
            r"
            main.sol
            contract O {}

            contract A is O {}
            contract B is O {}
            contract C is O {}
            contract D is O {}
            contract E is O {}

            contract K1 is B,A,C {}
            contract K3 is D,A {}
            contract K2 is E,D,B {}

            contract Z is K2,K3,K1 {}
        ",
        );
        cov_mark::check_count!(hir_nameres_inheritance_chain, 10);
        let (mut db, file) = TestDatabase::from_fixture(fixture);
        let project = Project::new(&db, vfs::VfsPath::from_virtual("".to_owned()));
        let source_unit = lower_file(&db, file);
        let data = source_unit.data(&db);
        let c = *data.contracts(&db).last().unwrap();
        assert_eq!(
            inheritance_chain(&db, project, c),
            vec![
                data.contract(&db, "Z"),
                data.contract(&db, "K1"),
                data.contract(&db, "C"),
                data.contract(&db, "K3"),
                data.contract(&db, "A"),
                data.contract(&db, "K2"),
                data.contract(&db, "B"),
                data.contract(&db, "D"),
                data.contract(&db, "E"),
                data.contract(&db, "O"),
            ]
        );
    }


    /// Checks if inheritance chain dont recompute after small unrelated changes
    #[test]
    fn invalidation_test() {
        tracing_subscriber::fmt().with_max_level(LevelFilter::DEBUG).init();
        let fixture = TestFixture::parse(
            r"
            main.sol
            contract O {}

            contract A is O {}
            contract B is O {}
            contract C is O {}
            contract D is O {}
            contract E is O {}

            contract K1 is B,A,C {}
            contract K3 is D,A {}
            contract K2 is E,D,B {}

            contract Z is K2,K3,K1 {}
        ",
        );
        let (mut db, file) = TestDatabase::from_fixture(fixture);
        let project = Project::new(&db, vfs::VfsPath::from_virtual("".to_owned()));
        {
            cov_mark::check_count!(hir_nameres_inheritance_chain, 10);
            let source_unit = lower_file(&db, file);
            let data = source_unit.data(&db);
            let c = *data.contracts(&db).last().unwrap();
            inheritance_chain(&db, project, c);
        }
        file.set_content(&mut db).to(r"
            contract O {}
            contract A is O {}
            contract B is O {}
            contract C is O {}
            contract D is O {}
            contract E is O {}
            contract K1 is B,A,C {}
            contract K3 is D,A {
                function tmp() {
                }
            }
            contract K2 is E,D,B {}
            contract Z is K2,K3,K1 {}
        ".into());
        {
            cov_mark::check_count!(hir_nameres_inheritance_chain, 0);
            let source_unit = lower_file(&db, file);
            let data = source_unit.data(&db);
            for c in data.contracts(&db) {
                inheritance_chain(&db, project, *c);
            }
        }
    }

    #[test]
    fn merge_err_test() {
        let fixture = TestFixture::parse(
            r"
            main.sol
            contract O {}

            contract A is O {}
            contract B is O {}
            contract C is O {}
            contract D is O {}
            contract E is O {}

            contract K1 is B,A,C {}
            contract K3 is D,C,A {}
            contract K2 is E,D,B {}

            contract Z is K2,K3,K1 {}
        ",
        );
        let (mut db, file) = TestDatabase::from_fixture(fixture);
        let project = Project::new(&db, vfs::VfsPath::from_virtual("".to_owned()));
        let source_unit = lower_file(&db, file);
        let data = source_unit.data(&db);
        let c = *data.contracts(&db).last().unwrap();
        assert_eq!(
            inheritance_chain(&db, project, c), 
            vec![
                c
            ]
        );
    }

    #[test]
    fn cycle_test() {
        let fixture = TestFixture::parse(
            r"
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
        ",
        );
        let (db, file) = TestDatabase::from_fixture(fixture);
        let source_unit = lower_file(&db, file);
        let data = source_unit.data(&db);
        let c = *data.contracts(&db).last().unwrap();
        assert_eq!(
            inheritance_chain(&db, Project::new(&db, vfs::VfsPath::from_virtual("".to_owned())), c),
            vec![c]
        );
    }
}
