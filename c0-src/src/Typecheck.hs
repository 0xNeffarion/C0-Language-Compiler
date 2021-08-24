
module Typecheck where

import           AST

import           Data.Map(Map)
import qualified Data.Map as Map
import Control.Exception

type TypeEnv = Map Ident Type

--Function-----------------------------
----Program = [FunDecl]
checkProgram :: TypeEnv -> Program -> TypeEnv
checkProgram env (x:xs) = checkProgram env' xs
                        where env' = checkFunDecl env x
checkProgram env [] = env

----------------------------------------------------------------------------------------------------

checkFunDecl :: TypeEnv -> FunDecl -> TypeEnv
checkFunDecl env (FunDecl t iden params block) =  if (checkBlock env'' t block)==True then env'
                                                  else error "CheckBlock Error"
                                                  where env'   = Map.insert iden (TypeFun (checkFParams params) t) env
                                                        env''  = declParams env' params



----------------------------------------------------------------------------------------------------
declParams :: TypeEnv -> [DeclF] -> TypeEnv
declParams env ((DeclF t ident):xs) = declParams env' xs
                                    where env' = Map.insert ident t env
declParams env [] = env

checkFParams :: [DeclF] -> [Type]
checkFParams (x:xs) = (checkFParam x):(checkFParams xs)
checkFParams [] = []

checkFParam :: DeclF -> Type
checkFParam (DeclF t iden) = t

----------------------------------------------------------------------------------------------------

checkBlock :: TypeEnv -> Type -> [Stmt] -> Bool
--forces haskell to not be lazy
checkBlock env ftype (x:xs) = if(env' == Map.empty) then False
                        else checkBlock env' ftype xs
                        where env' = checkStm env ftype x
checkBlock env _ [] = True

checkStm :: TypeEnv -> Type -> Stmt -> TypeEnv
checkStm env ftype (Decls t decL) = checkDecls env t decL
checkStm env ftype (Assign ident expr) = let t1 = checkExpr env (Var ident)
                                             t2 = checkExpr env expr
                                         in if t1==t2 then env
                                                  else error "TYPE ERROR"
---------------------------------------- Code Flow Statements  ----------------------------------
checkStm env ftype (If cond stms)
   = let t0 = checkExpr env cond
   in if t0 == TypeBool && (checkBlock env ftype stms) then env
        else error "type error: if condition"
checkStm env ftype (IfElse cond stms1 stms2)
   = let t0 = checkExpr env cond
   in if t0 == TypeBool && (checkBlock env ftype stms1) && (checkBlock env ftype stms2) then env
        else error "type error: ifelse condition must be bool"
checkStm env ftype (While expr stms)
   = let t0 = checkExpr env expr
   in if t0 == TypeBool && checkBlock env ftype stms then env
      else error "type error: while condition must be bool"
checkStm env ftype (For stm1 exp1 stm2 stms)
   = let env' = checkStm  env ftype stm1
         t1 = checkExpr env' exp1
         env'' = checkStm  env' ftype stm2 -- Isto deveria ser um stmt! To do
     in if t1==TypeBool && (checkBlock env'' ftype stms) then env
        else error "type error: for condition is invalid"

-------------------------------------------------Other----------------------------------------------
checkStm env ftype (Return expr)
   = let t0 = checkExpr env expr
     in if (t0 == ftype) then env
          else error "type error: return statement type error"

checkStm env ftype (CompExp expr) =  let t0 = checkExpr env expr
                                     in Map.delete "asdfghjklpoiuytrewq" env'
                                       where env' = Map.insert "asdfghjklpoiuytrewq" TypeVoid env


checkStm env ftype (Continue) = env
checkStm env ftype (Break) = env

----------------------------------------------------------------------------------------------------
-- Expressions
checkExpr :: TypeEnv -> Exp -> Type
---------------------------------------- Base Types ------------------------------------------------
checkExpr _ (Num _) = TypeInt
checkExpr _ (Str _) = TypeString
checkExpr _ (BoolValue _) = TypeBool
checkExpr _ (Ch _) = TypeChar
---------------------------------------- Variable Lookups ------------------------------------------
checkExpr env (Var x) = case Map.lookup x env of
                 Nothing -> error "undeclared variable"
                 Just t -> t
---------------------------------------- Arithmetic Operators --------------------------------------
checkExpr env (Add e1 e2)
   = let t1 = checkExpr env e1
         t2 = checkExpr env e2
   in if t1==TypeInt && t2==TypeInt then TypeInt
      else error "type error in +"
checkExpr env (Minus e1 e2)
   = let t1 = checkExpr env e1
         t2 = checkExpr env e2
   in if t1==TypeInt && t2==TypeInt then TypeInt
        else error "type error in -"
checkExpr env (Mult e1 e2)
  = let t1 = checkExpr env e1
        t2 = checkExpr env e2
  in if t1==TypeInt && t2==TypeInt then TypeInt
       else error "type error in /"
checkExpr env (Div e1 e2)
  = let t1 = checkExpr env e1
        t2 = checkExpr env e2
  in if t1==TypeInt && t2==TypeInt then TypeInt
               else error "type error in -"
checkExpr env (Mod e1 e2)
  = let t1 = checkExpr env e1
        t2 = checkExpr env e2
  in if t1==TypeInt && t2==TypeInt then TypeInt
       else error "type error in mod"
---------------------------------------- Comparison Operators --------------------------------------
checkExpr env (LessThan e1 e2)
  = let t1 = checkExpr env e1
        t2 = checkExpr env e2
  in if t1==TypeInt && t2==TypeInt then TypeBool
       else error "type error in <"
checkExpr env (LessEqThan e1 e2)
  = let t1 = checkExpr env e1
        t2 = checkExpr env e2
  in if t1==TypeInt && t2==TypeInt then TypeBool
       else error "type error in <="
checkExpr env (GreatThan e1 e2)
  = let t1 = checkExpr env e1
        t2 = checkExpr env e2
  in if t1==TypeInt && t2==TypeInt then TypeBool
       else error "type error in >"
checkExpr env (GreatEqThan e1 e2)
  = let t1 = checkExpr env e1
        t2 = checkExpr env e2
  in if t1==TypeInt && t2==TypeInt then TypeBool
       else error "type error in >="
checkExpr env (Equal e1 e2)
  = let t1 = checkExpr env e1
        t2 = checkExpr env e2
  in if (t1==TypeInt && t2==TypeInt) || (t1==TypeBool && t2==TypeBool) then TypeBool
       else error "type error in =="
checkExpr env (NotEqual e1 e2)
  = let t1 = checkExpr env e1
        t2 = checkExpr env e2
  in if (t1==TypeInt && t2==TypeInt) || (t1==TypeBool && t2==TypeBool) then TypeBool
       else error "type error in !="
---------------------------------------- Logical Operators --------------------------------------
checkExpr env (BoolOr e1 e2)
   = let t1 = checkExpr env e1
         t2 = checkExpr env e2
         in if t1==TypeBool && t2==TypeBool then TypeBool
              else error "type error in ||"
checkExpr env (BoolAnd e1 e2)
   = let t1 = checkExpr env e1
         t2 = checkExpr env e2
         in if t1==TypeBool && t2==TypeBool then TypeBool
              else error "type error in &&"
checkExpr env (BoolNot e1)
   = let t1 = checkExpr env e1
     in if t1==TypeBool then TypeBool
          else error "type error in !()"
--------------------------Funtions------------------------------------------------------------------
checkExpr env (Function ident args) = if (checkArgs env ident args) then case Map.lookup ident env of
                                                                      Just x  -> case x of
                                                                           TypeFun types ftype -> ftype
                                                                           _  -> error "FUNCTION CALL ERROR"
                                                                      Nothing -> error "FUNCTION CALL ERROR"
                                      else error "FUNCTION CALL ERROR"

checkArgs :: TypeEnv -> Ident -> [Exp] -> Bool
checkArgs env ident exps= case Map.lookup ident env of
                              Just  typefun -> checkArgsAux env (getTypes typefun) exps
                              Nothing -> error "FUNCTION ARGUMENTS ERROR"

getTypes :: Type -> [Type]
getTypes (TypeFun types ftype) = types
getTypes _ = error "getType ERROR"

checkArgsAux :: TypeEnv -> [Type] -> [Exp] -> Bool
checkArgsAux env (t:types) (expr:exprs) = let t0 = checkExpr env expr
                                          in if(t==t0) then checkArgsAux env types exprs
                                               else False
checkArgsAux env  [] [] = True
checkArgsAux env  _ [] = error "FUNCTION CALL ERROR"
checkArgsAux env [] _ = error "FUNCTION CALL ERROR"
----------------------------------------------------------------------------------------------------

checkDecls :: TypeEnv -> Type -> [Decl] -> TypeEnv
checkDecls env ty (x:xs) = checkDecls env' ty xs
                         where env' = checkDecl env ty x
checkDecls env ty [] = env

checkDecl :: TypeEnv -> Type -> Decl -> TypeEnv
checkDecl env ty (Decl ident) = case Map.lookup ident env of
                                  Just _  -> error "INIT ERROR"
                                  Nothing -> Map.insert ident ty env

checkDecl env ty (Init ident expr) = case Map.lookup ident env of
                                  Just _  -> error "INIT ERROR"
                                  Nothing -> let t0 = checkExpr env' expr
                                             in if t0==ty then env'
                                                  else error "INIT ERROR"
                                     where env' = Map.insert ident ty env
