module AST where

type Ident = String

type Program = [FunDecl]

data Type = TypeBool
          | TypeInt
          | TypeString
          | TypeChar
          | TypeFloat
          | TypeVoid
          | TypeFun [Type] Type
          deriving (Show, Eq)

data FunDecl = FunDecl Type Ident [DeclF] [Stmt]
             deriving Show


data DeclF = DeclF Type Ident
           | DeclFA Type Ident
           | DeclFP Type Ident
           deriving Show


--- statements
data Stmt = Assign Ident Exp
          | AssignArr Ident Int Exp
          | Decls Type [Decl]
          | If Exp [Stmt]
          | IfElse Exp [Stmt] [Stmt]
          | While Exp [Stmt]
          | For Stmt Exp Stmt [Stmt]
          | CompExp Exp
          | Continue
          | Break
          | Return Exp
          deriving Show

data Decl = Init Ident Exp
          | Decl Ident
          | DeclA Ident Exp
          | InitA Ident Exp
          | InitAS Ident Exp Exp
          | DeclP Ident
          | InitP Ident Exp
          deriving Show


          --- expressions
data Exp = Num Int
         | Real Double
         | Null
         | BoolValue Bool
         | Function Ident [Exp]
         | Add Exp Exp
         | Minus Exp Exp
         | Mult Exp Exp
         | Div Exp Exp
         | Mod Exp Exp
         | LessThan Exp Exp
         | LessEqThan Exp Exp
         | GreatThan Exp Exp
         | GreatEqThan Exp Exp
         | Equal Exp Exp
         | NotEqual Exp Exp
         | BoolOr Exp Exp
         | BoolAnd Exp Exp
         | BoolNot Exp
         | Var Ident
         | Str String
         | Ch String
         | ArrayInd Ident Exp
         | ArrayStream [Exp]
         | Reff Ident
         deriving Show
