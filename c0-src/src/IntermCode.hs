module IntermCode where

import AST

import Control.Monad.State

import           Data.Map(Map)
import qualified Data.Map as Map


------- Data/Types da Linguagem Intermédia------------------------------

type Temp = String
type Label = String

data Inst = MOVE Temp Temp              -- temp1 := temp2
          | MOVEI Temp Int              -- temp1 := int
          | MOVEC Temp Ident [Temp] Int     -- temp1 := id(temps...)
          | OP BinOp Temp Temp Temp     -- temp1 := temp2 op temp3
          | OPI BinOp Temp Temp Int     -- temp1 := temp2 op int
          | LABEL Label
          | JUMP Label
          | COND Temp RelOp Temp Label Label
          | CALL Ident [Temp] Int
          | RETURN Temp
          deriving Show

data BinOp = OpPlus | OpMinus | OpMult | OpDiv deriving Show
data RelOp = Lt | Gt | Eq | LtEq | GtEq | NEq deriving Show

-----------------------------------------------------------------------

type Func = (Ident, [Temp], [Inst])
type Table = Map Ident Temp
type Count = (Int, Int)


-------------------------
--Temps and Labels

newTemp :: State Count Temp
newTemp = do (temps, labels) <- get
             put (temps+1, labels)
             return ("t" ++ show temps)

popTemp :: Int-> State Count ()
popTemp n = do (temps, labels) <- get
               put (temps-n,labels)
               return  ()


resetTemp :: State Count ()
resetTemp = do (temps, labels) <- get
               put (0, labels)
               return  ()

newLabel :: State Count Label
newLabel = do (temps, labels) <- get
              put (temps, labels+1)
              return ("L" ++ show labels)



----------------------------
--Function

transProgram :: Program -> Table -> State Count [Func]
transProgram (x:prog) table =
     do  (table', fn) <- transFunDecl x table
         pg <- transProgram prog table
         return (fn : pg)
transProgram [] table = return []

transFunDecl :: FunDecl -> Table -> State Count (Table, Func)
transFunDecl (FunDecl _ id dc stmts) table =
     do (table', temps) <- transDeclsF dc table
        (table'', stmts) <- transStmts stmts table'
        return (table'',(id, temps, stmts))

transDeclsF :: [DeclF] -> Table -> State Count (Table, [Temp])
transDeclsF [] table    = return (table, [])
transDeclsF ((DeclF typ ident):xs) table =
     do t <- newTemp
        let table' = Map.insert ident t table
        (table'', temps) <- transDeclsF xs table'
        return (table'', (t:temps))





-------------------------------
--Statements
transStmts :: [Stmt] -> Table -> State Count (Table, [Inst])
transStmts [] table = return (table, [])
transStmts (x:xs) table =
            do  (table', code1) <-  (transStmt x table)
                (table'', code2) <- (transStmts xs table')
                return (table'', (code1 ++ code2))


transStmt :: Stmt -> Table -> State Count (Table, [Inst])
transStmt (Assign ident expr) table = case Map.lookup ident table of
                                    Nothing   -> error "undefinied variable"
                                    Just dest -> do code1 <- transExpr expr table dest
                                                    return (table, code1)
transStmt (If expr stms) table =
     do  l1    <- newLabel
         l2    <- newLabel
         code1 <- transCond expr table l1 l2
         (_, code2) <- transStmts stms table
         return (table, (code1 ++ [LABEL l1] ++ code2 ++ [LABEL l2]))
transStmt (IfElse expr stms1 stms2) table =
     do  l1    <- newLabel
         l2    <- newLabel
         l3    <- newLabel
         code1 <- transCond expr table l1 l2
         (_, code2) <- transStmts stms1 table
         (_', code3) <- transStmts stms2 table
         return (table, (code1 ++ [LABEL l1] ++ code2 ++ [JUMP l3] ++ [LABEL l2] ++ code3 ++ [LABEL l3]))
transStmt (While expr stms) table =
     do  l1    <- newLabel
         l2    <- newLabel
         l3    <- newLabel
         code1 <- transCond expr table l2 l3
         (_, code2) <- transStmts stms table
         return (table, ([LABEL l1] ++ code1 ++ [LABEL l2] ++ code2 ++ [JUMP l1, LABEL l3]))
transStmt (For stmt1 e1 stmt2 stmts) table =
     do  l1    <- newLabel
         l2    <- newLabel
         l3    <- newLabel
         (table', code1) <- transStmt stmt1 table
         code2 <- transCond e1 table' l2 l3
         (_, code3) <- transStmt stmt2 table'
         (_, code4) <- transStmts stmts table
         return (table, (code1 ++ [LABEL l1] ++ code2 ++ code3 ++ [LABEL l2] ++ code4 ++ [JUMP l1, LABEL l3]))
transStmt (AssignArr id n exp) table =
     do  t0  <- newTemp
         t1  <- newTemp
         code2 <- transExpr exp table t1
         popTemp(2)
         return (table, ([MOVEI t0 n, OPI OpMult t0 t0 4] ++ code2 ++ [MOVE t0 t1]))
transStmt (Decls _ decls) table =
     do  (table', code1) <- transDecls  decls table
         return (table', code1)
transStmt (Return e1) table =
     do t0 <- newTemp
        code1 <- transExpr e1 table t0
        popTemp(1)
        return (table, (code1 ++ [RETURN t0]))

transStmt (CompExp (Function id exps)) table =
     do  (code, temps) <- transExprs exps table
         let ntemps = Map.size table
         return (table, (code ++ [CALL id temps ntemps]))




----------------------------
--Declarations

transDecls :: [Decl] -> Table -> State Count (Table, [Inst])
transDecls [] table = return (table, [])
transDecls (x:xs) table = do (table', code1) <- transDecl x table
                             (table'', code2) <- transDecls xs table'
                             return (table'', code1 ++ code2)


transDecl :: Decl -> Table -> State Count (Table, [Inst])
transDecl (Decl ident)      table =  do t <- newTemp
                                        let table' = Map.insert ident t table
                                        return (table',[MOVEI t 0])
transDecl (Init ident expr) table = do t <- newTemp
                                       let table' = Map.insert ident t table
                                       code1 <- transExpr expr table' t
                                       return (table',code1)
                                       





------------------------------------------
--Expressions

transExprs :: [Exp] -> Table -> State Count ([Inst],[Temp])
transExprs [] _ = return ([],[])
transExprs (x:xs) table =
     do  t1           <- newTemp
         ex           <- transExpr x table t1
         (recI, recT) <- transExprs xs table
         popTemp(1)
         return (ex ++ recI, t1 : recT)


transExpr :: Exp -> Table -> Temp -> State Count [Inst]
transExpr (BoolValue False) _ dest = return [MOVEI dest 0]
transExpr (BoolValue True) _ dest = return [MOVEI dest 1]
transExpr (Num n) _ dest = return [MOVEI dest n]
transExpr (Var x) table dest = case Map.lookup x table of
                            Nothing -> error "undefinied variable"
                            Just n -> do
                                        return [MOVE dest n]
transExpr (Add e1 e2) table dest =
    do  t1    <- newTemp
        t2    <- newTemp
        code1 <- transExpr e1 table t1
        code2 <- transExpr e2 table t2
        popTemp(2)
        return (code1 ++ code2 ++ [OP OpPlus dest t1 t2])
transExpr (Minus e1 e2) table dest =
    do  t1    <- newTemp
        t2    <- newTemp
        code1 <- transExpr e1 table t1
        code2 <- transExpr e2 table t2
        popTemp(2)
        return (code1 ++ code2 ++ [OP OpMinus dest t1 t2])
transExpr (Mult e1 e2) table dest =
    do  t1    <- newTemp
        t2    <- newTemp
        code1 <- transExpr e1 table t1
        code2 <- transExpr e2 table t2
        popTemp(2)
        return (code1 ++ code2 ++ [OP OpMult dest t1 t2])
transExpr (Div e1 e2) table dest =
    do  t1    <- newTemp
        t2    <- newTemp
        code1 <- transExpr e1 table t1
        code2 <- transExpr e2 table t2
        popTemp(2)
        return (code1 ++ code2 ++ [OP OpDiv dest t1 t2])
transExpr (Mod e1 e2) table dest =
    do  t1    <- newTemp
        t2    <- newTemp
        t3    <- newTemp
        t4    <- newTemp
        code1 <- transExpr e1 table t1
        code2 <- transExpr e2 table t2
        popTemp(4)
        return (code1 ++ code2 ++ [OP OpDiv t3 t1 t2, OP OpMult t4 t3 t2, OP OpMinus dest t1 t4])
transExpr (LessThan e1 e2) table dest =
    do  l1    <- newLabel
        l2    <- newLabel
        code1 <- transCond (LessThan e1 e2) table l1 l2
        return ([MOVEI dest 0] ++ code1 ++ [LABEL l1, MOVEI dest 0] ++ [LABEL l2])
transExpr (LessEqThan e1 e2) table dest =
    do  l1    <- newLabel
        l2    <- newLabel
        code1 <- transCond (LessEqThan e1 e2) table l1 l2
        return ([MOVEI dest 0] ++ code1 ++ [LABEL l1, MOVEI dest 0] ++ [LABEL l2])
transExpr (GreatThan e1 e2) table dest =
    do  l1    <- newLabel
        l2    <- newLabel
        code1 <- transCond (GreatThan e1 e2) table l1 l2
        return ([MOVEI dest 0] ++ code1 ++ [LABEL l1, MOVEI dest 0] ++ [LABEL l2])
transExpr (GreatEqThan e1 e2) table dest =
    do  l1    <- newLabel
        l2    <- newLabel
        code1 <- transCond (GreatEqThan e1 e2) table l1 l2
        return ([MOVEI dest 0] ++ code1 ++ [LABEL l1, MOVEI dest 0] ++ [LABEL l2])
transExpr (Equal e1 e2) table dest =
    do  l1    <- newLabel
        l2    <- newLabel
        code1 <- transCond (Equal e1 e2) table l1 l2
        return ([MOVEI dest 0] ++ code1 ++ [LABEL l1, MOVEI dest 0] ++ [LABEL l2])
transExpr (NotEqual e1 e2) table dest =
    do  l1    <- newLabel
        l2    <- newLabel
        code1 <- transCond (NotEqual e1 e2) table l1 l2
        return ([MOVEI dest 0] ++ code1 ++ [LABEL l1, MOVEI dest 0] ++ [LABEL l2])
transExpr (ArrayInd id exp) table dest =
    do  (code1, addr) <- transIndex (ArrayInd id exp) table
        return (code1 ++ [MOVE dest addr])
transExpr (Function id exps) table dest =
    do  (code, temps) <- transExprs exps table
        let ntemps = Map.size table
        return (code ++ [MOVEC dest id temps ntemps])



-----------------------
--array index
transIndex :: Exp -> Table -> State Count ([Inst], Temp)
transIndex (ArrayInd id e1) table =
    do base  <- case Map.lookup id table of
               Nothing -> error "undefinied variable"
               Just n -> return n
       addr  <- newTemp
       code1 <- transExpr e1 table addr
       return (code1 ++ [OPI OpMult addr addr 4, OP OpPlus addr addr base], addr)





--------------------------------
--Conditions

transCond :: Exp -> Table -> Label -> Label -> State Count [Inst]
transCond (BoolNot exp) table lt lf = transCond exp table lf lt
transCond (LessEqThan exp1 exp2) table lt lf =
     do  t1    <- newTemp
         t2    <- newTemp
         code1 <- transExpr exp1 table t1
         code2 <- transExpr exp2 table t2
         popTemp(2)
         return (code1 ++ code2 ++ [COND t1 LtEq t2 lt lf])
transCond (LessThan exp1 exp2) table lt lf =
     do  t1    <- newTemp
         t2    <- newTemp
         code1 <- transExpr exp1 table t1
         code2 <- transExpr exp2 table t2
         popTemp(2)
         return (code1 ++ code2 ++ [COND t1 Lt t2 lt lf])
transCond (GreatThan exp1 exp2) table lt lf =
     do  t1    <- newTemp
         t2    <- newTemp
         code1 <- transExpr exp1 table t1
         code2 <- transExpr exp2 table t2
         popTemp(2)
         return (code1 ++ code2 ++ [COND t1 Gt t2 lt lf])
transCond (GreatEqThan exp1 exp2) table lt lf =
     do  t1    <- newTemp
         t2    <- newTemp
         code1 <- transExpr exp1 table t1
         code2 <- transExpr exp2 table t2
         popTemp(2)
         return (code1 ++ code2 ++ [COND t1 GtEq t2 lt lf])
transCond (Equal exp1 exp2) table lt lf =
     do  t1    <- newTemp
         t2    <- newTemp
         code1 <- transExpr exp1 table t1
         code2 <- transExpr exp2 table t2
         popTemp(2)
         return (code1 ++ code2 ++ [COND t1 Eq t2 lt lf])
transCond (NotEqual exp1 exp2) table lt lf =
     do  t1    <- newTemp
         t2    <- newTemp
         code1 <- transExpr exp1 table t1
         code2 <- transExpr exp2 table t2
         popTemp(2)
         return (code1 ++ code2 ++ [COND t1 NEq t2 lt lf])
transCond (BoolOr exp1 exp2) table lt lf =
     do  l1    <- newLabel
         code1 <- transCond exp1 table l1 lf
         code2 <- transCond exp2 table lt lf
         return (code1 ++ [LABEL l1] ++ code2)
transCond (BoolAnd exp1 exp2) table lt lf =
     do  l1    <- newLabel
         code1 <- transCond exp1 table lt l1
         code2 <- transCond exp2 table lt lf
         return (code1 ++ [LABEL l1] ++ code2)

transCond (BoolValue True) _ lt _ = return []
transCond (BoolValue False) _ _ lf = return []
