package sutd.compiler.simp.syntax

import sutd.compiler.simp.syntax.SrcLoc.*

object AST {
    type Var = String
    
    enum Stmt {
        case Nop(src: SrcLoc)
        case Assign(src: SrcLoc, x: Var, e: Exp)
        case Ret(src: SrcLoc, e: Exp)
        case IfElse(src: SrcLoc, cond: Exp, thn: Stmt, els: Stmt)
        case While(src: SrcLoc, cond: Exp, body: Stmt)
        case Seq(src: SrcLoc, stmts: List[Stmt])
    }
    
    enum Exp {
        case IntConst(src: SrcLoc, v: Int)
        case BoolConst(src: SrcLoc, v: Boolean)
        case Var(src: SrcLoc, x: String)
        case Plus(src: SrcLoc, e1: Exp, e2: Exp)
        case Minus(src: SrcLoc, e1: Exp, e2: Exp)
        case Mult(src: SrcLoc, e1: Exp, e2: Exp)
        case DEqual(src: SrcLoc, e1: Exp, e2: Exp)
        case LThan(src: SrcLoc, e1: Exp, e2: Exp)
    }
    
    // Helper functions to extract source locations
    import Stmt.*
    import Exp.*
    
    def stmtSrcLoc(s: Stmt): SrcLoc = s match {
        case Nop(src) => src
        case Assign(src, _, _) => src
        case Ret(src, _) => src
        case IfElse(src, _, _, _) => src
        case While(src, _, _) => src
        case Seq(src, _) => src
    }
    
    def expSrcLoc(e: Exp): SrcLoc = e match {
        case IntConst(src, _) => src
        case BoolConst(src, _) => src
        case Var(src, _) => src
        case Plus(src, _, _) => src
        case Minus(src, _, _) => src
        case Mult(src, _, _) => src
        case DEqual(src, _, _) => src
        case LThan(src, _, _) => src
    }
}
