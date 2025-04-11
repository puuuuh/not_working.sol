//! `AstIdMap` allows to create stable IDs for "large" syntax nodes like items
//! and macro calls.
//!
//! Specifically, it enumerates all items in a file and uses position of a an
//! item as an ID. That way, id's don't change unless the set of items itself
//! changes.

use rowan::ast::AstNode;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::{
    any::type_name,
    fmt,
    hash::{Hash, Hasher},
    marker::PhantomData,
};
use syntax::{ast, SolidityLang, SyntaxNode, SyntaxNodePtr};

pub type ErasedFileAstId = SyntaxNodePtr;

/// `AstId` points to an AST node in a specific file.
pub struct FileAstId<N: AstIdNode> {
    raw: ErasedFileAstId,
    covariant: PhantomData<fn() -> N>,
}

impl<N: AstIdNode> Clone for FileAstId<N> {
    fn clone(&self) -> FileAstId<N> {
        *self
    }
}
impl<N: AstIdNode> Copy for FileAstId<N> {}

impl<N: AstIdNode> PartialEq for FileAstId<N> {
    fn eq(&self, other: &Self) -> bool {
        self.raw == other.raw
    }
}
impl<N: AstIdNode> Eq for FileAstId<N> {}
impl<N: AstIdNode> Hash for FileAstId<N> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.raw.hash(hasher);
    }
}

impl<N: AstIdNode> fmt::Debug for FileAstId<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "FileAstId::<{}>({:?})", type_name::<N>(), self.raw.kind())
    }
}

impl<N: AstIdNode> FileAstId<N> {
    // Can't make this a From implementation because of coherence
    pub fn upcast<M: AstIdNode>(self) -> FileAstId<M>
    where
        N: Into<M>,
    {
        FileAstId { raw: self.raw, covariant: PhantomData }
    }

    pub fn erase(self) -> ErasedFileAstId {
        self.raw
    }
}

pub trait AstIdNode: AstNode<Language = SolidityLang> {}

macro_rules! register_ast_id_node {
    (impl AstIdNode for $($ident:ident),+ ) => {
        $(
            impl AstIdNode for ast::nodes::$ident {}
        )+
        fn should_alloc_id(kind: syntax::SyntaxKind) -> bool {
            $(
                ast::nodes::$ident::can_cast(kind)
            )||+
        }
    };
}
register_ast_id_node! {
    impl AstIdNode for
    Item,
    Import,
    Pragma,
    Using,
    Contract,
    ConstructorDefinition,
    FunctionDefinition,
    ContractItem,
    ModifierDefinition,

    StateVariableDeclaration,
    EnumDefinition,
        EnumMember,
    StructDefinition,
        StructMember,
    UserDefinedValueTypeDefinition,
    EventDefinition,
    ErrorDefinition,
    ParameterList,

    VariableDeclaration,
    Block
}

/// Maps items' `SyntaxNode`s to `ErasedFileAstId`s and back.
#[derive(Default)]
pub struct AstIdMap {
    ids: Vec<SyntaxNodePtr>,
    map: HashMap<SyntaxNodePtr, usize>,
}

impl fmt::Debug for AstIdMap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("AstIdMap").field("map", &self.ids).finish()
    }
}

impl PartialEq for AstIdMap {
    fn eq(&self, other: &Self) -> bool {
        self.ids == other.ids
    }
}
impl Eq for AstIdMap {}

impl AstIdMap {
    pub fn from_source(node: &SyntaxNode) -> AstIdMap {
        assert!(node.parent().is_none());
        let mut res = AstIdMap::default();

        // make sure to allocate the root node
        if !should_alloc_id(node.kind()) {
            res.ids.push(SyntaxNodePtr::new(node));
        }
        // By walking the tree in breadth-first order we make sure that parents
        // get lower ids then children. That is, adding a new child does not
        // change parent's id. This means that, say, adding a new function to a
        // trait does not change ids of top-level items, which helps caching.
        bdfs(node, |it| {
            if should_alloc_id(it.kind()) {
                res.ids.push(SyntaxNodePtr::new(&it));
                TreeOrder::BreadthFirst
            } else {
                TreeOrder::DepthFirst
            }
        });
        res.map = HashMap::with_capacity(res.ids.len());
        for (idx, ptr) in res.ids.iter().enumerate() {
            match res.map.entry(*ptr) {
                Entry::Occupied(_) => unreachable!(),
                Entry::Vacant(entry) => {
                    entry.insert(idx);
                }
            }
        }
        res.ids.shrink_to_fit();
        res
    }

    /// The [`AstId`] of the root node
    pub fn root(&self) -> SyntaxNodePtr {
        self.ids[0]
    }

    pub fn ast_id<N: AstIdNode>(&self, item: &N) -> FileAstId<N> {
        let raw = self.get_erased(SyntaxNodePtr::new(item.syntax()));
        FileAstId { raw, covariant: PhantomData }
    }

    pub fn ast_id_for_ptr<N: AstIdNode>(&self, ptr: SyntaxNodePtr) -> FileAstId<N> {
        match self.map.get(&ptr) {
            Some(&raw) => FileAstId { raw: self.ids[raw], covariant: PhantomData },
            None => panic!("Can't find {:?} in AstIdMap:\n{:?}", ptr, self.ids,),
        }
    }

    pub fn get<N: AstIdNode>(&self, id: FileAstId<N>) -> SyntaxNodePtr {
        SyntaxNodePtr::from(self.ids[self.map[&id.raw]])
    }

    pub fn get_erased(&self, id: ErasedFileAstId) -> SyntaxNodePtr {
        self.ids[self.map[&id]]
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum TreeOrder {
    BreadthFirst,
    DepthFirst,
}

/// Walks the subtree in bdfs order, calling `f` for each node. What is bdfs
/// order? It is a mix of breadth-first and depth first orders. Nodes for which
/// `f` returns [`TreeOrder::BreadthFirst`] are visited breadth-first, all the other nodes are explored
/// [`TreeOrder::DepthFirst`].
///
/// In other words, the size of the bfs queue is bound by the number of "true"
/// nodes.
fn bdfs(node: &SyntaxNode, mut f: impl FnMut(SyntaxNode) -> TreeOrder) {
    let mut curr_layer = vec![node.clone()];
    let mut next_layer = vec![];
    while !curr_layer.is_empty() {
        curr_layer.drain(..).for_each(|node| {
            let mut preorder = node.preorder();
            while let Some(event) = preorder.next() {
                match event {
                    syntax::WalkEvent::Enter(node) => {
                        if f(node.clone()) == TreeOrder::BreadthFirst {
                            next_layer.extend(node.children());
                            preorder.skip_subtree();
                        }
                    }
                    syntax::WalkEvent::Leave(_) => {}
                }
            }
        });
        std::mem::swap(&mut curr_layer, &mut next_layer);
    }
}
