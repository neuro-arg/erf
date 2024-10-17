// an ir that offers a very high level representation of types but low level control flow
// it's an intermediate step before lowering into a compilation target

use std::collections::BTreeMap;

#[derive(Debug)]
pub struct Program {
    label: Option<String>,
    funcs: Vec<Func>,
}

impl Program {
    pub fn new(label: Option<&str>) -> Self {
        Self {
            label: label.map(From::from),
            funcs: vec![],
        }
    }
    pub fn alloc_func(&mut self, label: Option<&str>, typ: Type) -> &mut Func {
        self.funcs.push(Func {
            label: label.map(From::from),
            basic_blocks: vec![],
            index: self.funcs.len(),
            typ,
        });
        self.funcs.last_mut().unwrap()
    }
}

#[derive(Debug)]
pub struct Func {
    label: Option<String>,
    basic_blocks: Vec<BasicBlock>,
    index: usize,
    typ: Type,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VarRef(BlockRef, LocalVarRef);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LocalVarRef(usize);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockRef(usize);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FuncRef(usize);

impl Func {
    pub fn reference(&self) -> FuncRef {
        FuncRef(self.index)
    }
    fn var(&self, var: VarRef) -> &Var {
        &self.basic_blocks[var.0 .0].vars[var.1 .0]
    }
    pub fn alloc_blocks<const N: usize>(&mut self) -> &mut [BasicBlock; N] {
        let i = self.basic_blocks.len();
        let mut j = i;
        self.basic_blocks.resize_with(i + N, || {
            let ret = BasicBlock {
                index: j,
                ops: vec![],
                vars: vec![],
            };
            j += 1;
            ret
        });
        (&mut self.basic_blocks[i..]).try_into().unwrap()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    // 0-tuple, used as the empty closure type
    Void,
    Bool,
    Int {
        signed: bool,
        bits: u8,
    },
    Float {
        bits: u8,
    },
    Product(Vec<Type>),
    Sum(Vec<Type>),
    /// Each function is a separate type, much like in Rust.
    /// This is because GPUs don't have function pointers, and GPUs are the main target.
    /// Closure is the type of the captured data that must be passed as the first arguments.
    Func {
        func: FuncRef,
        closure: Vec<Type>,
    },
}

#[derive(Clone, Debug)]
struct Var {
    label: Option<String>,
    typ: Type,
}

#[derive(Debug)]
pub struct BasicBlock {
    index: usize,
    ops: Vec<OpCode>,
    vars: Vec<Var>,
}

impl BasicBlock {
    pub fn reference(&self) -> BlockRef {
        BlockRef(self.index)
    }
    pub fn alloc_var(&mut self, label: Option<&str>, typ: Type) -> VarRef {
        self.vars.push(Var {
            label: label.map(From::from),
            typ,
        });
        VarRef(self.reference(), LocalVarRef(self.vars.len() - 1))
    }
}

/// A chain sequence of 0 or more field accesses
#[derive(Clone, Debug, PartialEq)]
pub struct Field {
    pub var: VarRef,
    pub field_accesses: Vec<usize>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum PhiSource {
    Bool(bool),
    Int(i128),
    Float(f64),
    Product(Vec<PhiSource>),
    Func(FuncRef),
    Var(VarRef),
}

#[derive(Clone, Debug, PartialEq)]
pub enum AssignSource {
    Bool(bool),
    Int(i128),
    Float(f64),
    Product(Vec<AssignSource>),
    Func(FuncRef),
    Var(Field),
}

#[derive(Clone, Debug, PartialEq)]
pub enum OpCode {
    // BLOCK TERMINATORS
    /// Return from the function
    Return,
    /// Jump to a different block (it must come after this one)
    Jump { target: BlockRef },
    // NON-TERMINATORS
    /// Conditionally jump to a different block (it must come after this one)
    CondJump { target: BlockRef, cond: VarRef },
    /// Conditionally jump to a different block (it must come before this one)
    CondBackJump { target: BlockRef, cond: VarRef },
    /// Call a function
    Call {
        /// Assignment target
        target: Option<LocalVarRef>,
        /// Function, potentially with closure
        function: VarRef,
        /// Arg count must equal function arity minus closure length
        args: Vec<VarRef>,
    },
    /// <target> := match previous_block {
    ///     $(<block> => <var>)*
    /// }
    ///
    /// Sources must cover all possible predecessors
    Phi {
        target: LocalVarRef,
        sources: BTreeMap<BlockRef, PhiSource>,
    },
    /// <target> := <source>
    Assign {
        target: LocalVarRef,
        source: AssignSource,
    },
    /// match var {
    ///     $(<x> => goto <x>,)
    /// }
    ///
    /// Var's type must be union, union len must match target count
    Case { var: VarRef, targets: Vec<BlockRef> },
}
