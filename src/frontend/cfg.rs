use crate::frontend::common::*;
use crate::frontend::common::{BinOp, Cmp, FloatBinOp, IntBinOp};
use crate::frontend::ctx::{Ctx, VarId};
use crate::frontend::typecheck::Type;
use cranelift_entity::{entity_impl, PrimaryMap};
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RepType {
    Word,
    Float,
}

impl fmt::Display for RepType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RepType::Word => "W".fmt(f),
            RepType::Float => "F".fmt(f),
        }
    }
}

impl From<&Type> for RepType {
    fn from(ty: &Type) -> RepType {
        match ty {
            Type::Var(_) => panic!("Type variable in RepType::from"),
            Type::Float => RepType::Float,
            _ => RepType::Word,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockIdx(u32);
entity_impl!(BlockIdx, "b");

// Functions
#[derive(Debug)]
pub struct Fun {
    pub name: VarId,
    pub args: Vec<VarId>,
    pub blocks: PrimaryMap<BlockIdx, BlockData>,
    pub return_type: RepType,
}

#[derive(Debug)]
pub enum BlockData {
    NA,
    Block(Block),
}

impl BlockData {
    #[allow(non_snake_case)]
    pub fn is_NA(&self) -> bool {
        match self {
            BlockData::NA => true,
            BlockData::Block(_) => false,
        }
    }

    pub fn get_block(&self) -> Option<&Block> {
        match self {
            BlockData::NA => None,
            BlockData::Block(block) => Some(block),
        }
    }
}

// Basic blocks
#[derive(Debug)]
pub struct Block {
    pub idx: BlockIdx,
    pub comment: Option<String>,
    pub stmts: Vec<Stmt>,
    pub exit: Exit,
}

#[derive(Debug)]
pub enum Stmt {
    Asgn(Asgn),
    Expr(Expr),
}

// Assignments
#[derive(Debug)]
pub struct Asgn {
    pub lhs: VarId,
    pub rhs: Expr,
}

// Assignment right-hand sides
#[derive(Debug)]
pub enum Expr {
    Atom(Atom),
    IBinOp(BinOp<IntBinOp>),
    FBinOp(BinOp<FloatBinOp>),
    Neg(VarId),
    FNeg(VarId),
    App(VarId, Vec<VarId>, RepType),
    // Tuple allocation
    Tuple { len: usize },
    // Tuple field read
    TupleGet(VarId, usize),
    // Tuple field write
    TuplePut(VarId, usize, VarId),
    // Array allocation
    ArrayAlloc { len: VarId },
    // Array field read
    ArrayGet(VarId, VarId),
    // Array field write
    ArrayPut(VarId, VarId, VarId),
}

#[derive(Debug, PartialEq)]
pub enum Atom {
    Unit,
    Int(i64),
    Float(f64),
    Var(VarId),
}

// Exit nodes of basic blocks
#[derive(Debug, PartialEq, Eq)]
pub enum Exit {
    Return(VarId),
    Branch {
        v1: VarId,
        v2: VarId,
        cond: Cmp,
        then_block: BlockIdx,
        else_block: BlockIdx,
    },
    Jump(BlockIdx),
}

fn print_comma_sep<A>(
    ctx: &Ctx,
    stuffs: &mut dyn Iterator<Item = &A>,
    show_stuff: fn(&A, ctx: &Ctx, w: &mut dyn fmt::Write) -> fmt::Result,
    w: &mut dyn fmt::Write,
) -> Result<(), fmt::Error> {
    let mut add_comma = false;
    for stuff in stuffs {
        if add_comma {
            w.write_str(", ")?;
        } else {
            add_comma = true;
        }
        show_stuff(stuff, ctx, w)?;
    }
    Ok(())
}

impl Fun {
    pub fn pp(&self, ctx: &Ctx, w: &mut dyn fmt::Write) -> fmt::Result {
        let Fun {
            name,
            args,
            blocks,
            return_type,
        } = self;

        w.write_str("function ")?;
        pp_id(ctx, *name, w)?;
        w.write_str("(")?;
        print_comma_sep(ctx, &mut args.iter(), pp_id_ref, w)?;
        writeln!(w, ") -> {}", return_type)?;

        for block in blocks.values() {
            match block {
                BlockData::NA => {}
                BlockData::Block(block) => {
                    block.pp(ctx, w)?;
                }
            }
        }
        writeln!(w)
    }
}

impl Block {
    pub fn pp(&self, ctx: &Ctx, w: &mut dyn fmt::Write) -> Result<(), fmt::Error> {
        let Block {
            idx,
            comment,
            stmts,
            exit,
        } = self;
        write!(w, "{}:", idx)?;
        match comment {
            None => {
                writeln!(w)?;
            }
            Some(comment) => {
                writeln!(w, " // {}", comment)?;
            }
        }
        for asgn in stmts {
            w.write_str("    ")?;
            asgn.pp(ctx, w)?;
            writeln!(w)?;
        }
        w.write_str("    ")?;
        exit.pp(ctx, w)?;
        writeln!(w)
    }
}

impl Exit {
    pub fn pp(&self, ctx: &Ctx, w: &mut dyn fmt::Write) -> fmt::Result {
        use Exit::*;
        match self {
            Return(var) => {
                w.write_str("return ")?;
                pp_id(ctx, *var, w)
            }
            Branch {
                v1,
                v2,
                cond,
                then_block,
                else_block,
            } => {
                w.write_str("if ")?;
                pp_id(ctx, *v1, w)?;
                write!(w, " {} ", cond)?;
                pp_id(ctx, *v2, w)?;
                write!(w, " then {} else {}", then_block, else_block)
            }
            Jump(lbl) => write!(w, "jump {}", lbl),
        }
    }
}

impl Stmt {
    pub fn pp(&self, ctx: &Ctx, w: &mut dyn fmt::Write) -> fmt::Result {
        match self {
            Stmt::Asgn(asgn) => asgn.pp(ctx, w),
            Stmt::Expr(expr) => expr.pp(ctx, w),
        }
    }
}

impl Asgn {
    pub fn pp(&self, ctx: &Ctx, w: &mut dyn fmt::Write) -> fmt::Result {
        let Asgn { lhs, rhs } = self;
        pp_id(ctx, *lhs, w)?;
        w.write_str(": ")?;
        match ctx.var_type_(*lhs) {
            Some(var_type) => {
                var_type.pp(w)?;
            }
            None => {
                w.write_str("{{?}}")?;
            }
        }
        w.write_str(" = ")?;
        rhs.pp(ctx, w)
    }
}

impl Expr {
    pub fn pp(&self, ctx: &Ctx, w: &mut dyn fmt::Write) -> fmt::Result {
        use Expr::*;
        match self {
            Atom(atom) => atom.pp(ctx, w),
            IBinOp(BinOp { op, arg1, arg2 }) => {
                pp_id(ctx, *arg1, w)?;
                let op_str = match op {
                    IntBinOp::Add => " + ",
                    IntBinOp::Sub => " - ",
                    // IntBinOp::Mul => " * ",
                    // IntBinOp::Div => " / ",
                };
                write!(w, "{}", op_str)?;
                pp_id(ctx, *arg2, w)
            }
            FBinOp(BinOp { op, arg1, arg2 }) => {
                pp_id(ctx, *arg1, w)?;
                let op_str = match op {
                    FloatBinOp::Add => " +. ",
                    FloatBinOp::Sub => " -. ",
                    FloatBinOp::Mul => " *. ",
                    FloatBinOp::Div => " /. ",
                };
                write!(w, "{}", op_str)?;
                pp_id(ctx, *arg2, w)
            }
            Neg(var) => {
                w.write_str("-")?;
                pp_id(ctx, *var, w)
            }
            FNeg(var) => {
                w.write_str("-.")?;
                pp_id(ctx, *var, w)
            }
            App(fun, args, _) => {
                pp_id(ctx, *fun, w)?;
                w.write_str("(")?;
                print_comma_sep(ctx, &mut args.iter(), pp_id_ref, w)?;
                w.write_str(")")
            }
            Tuple { len } => write!(w, "alloc_tuple(len={})", len),
            TuplePut(tuple, idx, val) => {
                pp_id(ctx, *tuple, w)?;
                write!(w, ".{{{}}} <- ", idx)?;
                pp_id(ctx, *val, w)
            }
            TupleGet(tuple, idx) => {
                pp_id(ctx, *tuple, w)?;
                write!(w, ".{}", idx)
            }
            ArrayAlloc { len } => {
                w.write_str("alloc_array(len=")?;
                pp_id(ctx, *len, w)?;
                w.write_str(")")
            }
            ArrayGet(array, idx) => {
                pp_id(ctx, *array, w)?;
                w.write_str(".(")?;
                pp_id(ctx, *idx, w)?;
                w.write_str(")")
            }
            ArrayPut(array, idx, val) => {
                pp_id(ctx, *array, w)?;
                w.write_str(".(")?;
                pp_id(ctx, *idx, w)?;
                w.write_str(") <- ")?;
                pp_id(ctx, *val, w)
            }
        }
    }
}

impl Atom {
    pub fn pp(&self, ctx: &Ctx, w: &mut dyn fmt::Write) -> fmt::Result {
        use Atom::*;
        match self {
            Unit => w.write_str("()"),
            Int(i) => write!(w, "{}", i),
            // Use debug format in floats, otherwise "1.0" is printed as "1"
            Float(f) => write!(w, "{:?}", f),
            Var(var) => pp_id(ctx, *var, w),
        }
    }
}

fn pp_id(ctx: &Ctx, id: VarId, w: &mut dyn fmt::Write) -> fmt::Result {
    write!(w, "{}", ctx.get_var(id))
}

fn pp_id_ref(id: &VarId, ctx: &Ctx, w: &mut dyn fmt::Write) -> fmt::Result {
    write!(w, "{}", ctx.get_var(*id))
}
