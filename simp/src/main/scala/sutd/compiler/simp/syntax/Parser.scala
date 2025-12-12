package sutd.compiler.simp.syntax

import sutd.compiler.simp.syntax.Lexer.*
import sutd.compiler.simp.syntax.SrcLoc.*
import sutd.compiler.simp.monad.Monad.*
import sutd.compiler.simp.syntax.Parsec.*

object Parser {
    import LToken.*
    import AST.*
    import AST.Stmt.*
    import AST.Exp.*
    
    case class PEnv(tokens: List[LToken])
    
    given penvParserEnv: ParserEnv[PEnv, LToken] = new ParserEnv[PEnv, LToken] {
        override def getTokens(env: PEnv): List[LToken] = env.tokens
        override def setTokens(ts: List[LToken])(env: PEnv): PEnv = PEnv(ts)
        override def getCol(env: PEnv): Int = env.tokens.headOption.map(t => srcLoc(t).col).getOrElse(0)
        override def getLine(env: PEnv): Int = env.tokens.headOption.map(t => srcLoc(t).line).getOrElse(0)
        override def setLine(l: Int)(env: PEnv): PEnv = env
        override def setCol(c: Int)(env: PEnv): PEnv = env
    }
    
    // Terminal parsers with source location extraction
    def p_IntTok: Parser[PEnv, (SrcLoc, Int)] = 
        sat((tok: LToken) => tok match {
            case IntTok(_, _) => true
            case _ => false
        }).flatMap {
            case IntTok(src, v) => pure((src, v))
            case _ => fail("Expected IntTok")
        }
    
    def p_IdTok: Parser[PEnv, (SrcLoc, String)] = 
        sat((tok: LToken) => tok match {
            case IdTok(_, _) => true
            case _ => false
        }).flatMap {
            case IdTok(src, v) => pure((src, v))
            case _ => fail("Expected IdTok")
        }
    
    def p_TrueKW: Parser[PEnv, SrcLoc] = 
        sat((tok: LToken) => tok match {
            case TrueKW(_) => true
            case _ => false
        }).flatMap {
            case TrueKW(src) => pure(src)
            case _ => fail("Expected true")
        }
    
    def p_FalseKW: Parser[PEnv, SrcLoc] = 
        sat((tok: LToken) => tok match {
            case FalseKW(_) => true
            case _ => false
        }).flatMap {
            case FalseKW(src) => pure(src)
            case _ => fail("Expected false")
        }
    
    // Expression parsers
    def p_Const: Parser[PEnv, Exp] = 
        p_IntTok.flatMap { case (src, v) => pure(IntConst(src, v)) }
        .orElse(p_TrueKW.flatMap(src => pure(BoolConst(src, true))))
        .orElse(p_FalseKW.flatMap(src => pure(BoolConst(src, false))))
    
    def p_Var: Parser[PEnv, Exp] = 
        p_IdTok.flatMap { case (src, x) => pure(Var(src, x)) }
    
    // Statement parsers
    def p_Assign: Parser[PEnv, Stmt] = for {
        (src, x) <- p_IdTok
        _ <- sat((tok: LToken) => tok match { case EqSign(_) => true; case _ => false })
        e <- p_Exp
        _ <- sat((tok: LToken) => tok match { case SemiColon(_) => true; case _ => false })
    } yield Assign(src, x, e)
    
    def p_Return: Parser[PEnv, Stmt] = for {
        src <- sat((tok: LToken) => tok match { 
            case RetKW(_) => true; case _ => false 
        }).flatMap {
            case RetKW(s) => pure(s)
            case _ => fail("Expected return")
        }
        e <- p_Exp
        _ <- sat((tok: LToken) => tok match { case SemiColon(_) => true; case _ => false })
    } yield Ret(src, e)
    
    def p_IfElse: Parser[PEnv, Stmt] = for {
        src <- sat((tok: LToken) => tok match { 
            case IfKW(_) => true; case _ => false 
        }).flatMap {
            case IfKW(s) => pure(s)
            case _ => fail("Expected if")
        }
        _ <- sat((tok: LToken) => tok match { case LParen(_) => true; case _ => false })
        cond <- p_Exp
        _ <- sat((tok: LToken) => tok match { case RParen(_) => true; case _ => false })
        thn <- p_Stmt
        _ <- sat((tok: LToken) => tok match { case ElseKW(_) => true; case _ => false })
        els <- p_Stmt
    } yield IfElse(src, cond, thn, els)
    
    def p_While: Parser[PEnv, Stmt] = for {
        src <- sat((tok: LToken) => tok match { 
            case WhileKW(_) => true; case _ => false 
        }).flatMap {
            case WhileKW(s) => pure(s)
            case _ => fail("Expected while")
        }
        _ <- sat((tok: LToken) => tok match { case LParen(_) => true; case _ => false })
        cond <- p_Exp
        _ <- sat((tok: LToken) => tok match { case RParen(_) => true; case _ => false })
        body <- p_Stmt
    } yield While(src, cond, body)
    
    // Add similar updates for p_Exp, p_Term, p_Factor with source locations
}
