
{
module Parser where
import Lexer
import AST
}

%name parser
%tokentype { Token }
%error { parseError }

%nonassoc '>' ">=" '<' "<=" "==" "!=" "||" "&&" '!' '|' '&'
%left '+' '-'
%left '*' '/' '%'
--por agora a associação default de ';' está a funcionar com o ciclo 'for'

%token

ident     { TOK_IDENT $$ }
num       { TOK_NUM $$ }
string    { TOK_STRING $$ }
char      { TOK_CHAR_VALUE $$ }
real      { TOK_REAL $$ }
nul       { TOK_NULL }

true      { TOK_BOOL_TRUE  }
false     { TOK_BOOL_FALSE }

if        { TOK_IF }
else      { TOK_ELSE }

while     { TOK_WHILE }
for       { TOK_FOR }

int       { TOK_INT }
charT     { TOK_CHAR_TYPE }
bool      { TOK_BOOL }
strT      { TOK_STRING_TYPE }
float     { TOK_FLOAT_TYPE }
void      { TOK_VOID_TYPE }

continue  { TOK_CONTINUE }
break     { TOK_BREAK }
return    { TOK_RETURN }


'+'     { TOK_PLUS }
'-'     { TOK_MINUS }
'*'     { TOK_MULT }
'/'     { TOK_DIV }
'('     { TOK_LPAREN }
')'     { TOK_RPAREN }
'='     { TOK_EQUAL}
'<'     { TOK_LESS }
'%'     { TOK_MOD }
"<="    { TOK_LESSEQ }
'>'     { TOK_GREATER }
">="    { TOK_GREATEREQ }
"=="    { TOK_EQUALTO }
"!="    { TOK_DIFF }
'{'     { TOK_BRACKET_OPEN }
'}'     { TOK_BRACKET_CLOSE }
'['     { TOK_SQBRACKET_OPEN}
']'     { TOK_SQBRACKET_CLOSE }
';'     { TOK_SEMICOLON }
'!'     { TOK_NOT }
','     { TOK_COMMA }
"||"    { TOK_OR }
"&&"    { TOK_AND }
'&'     { TOK_BIN_AND }
'|'     { TOK_BIN_OR }

%%

Program : FunDecl            { [$1] }
        | FunDecl Program    { $1:$2 }


----------------------- Functions ---------------------------

FunDecl : Type ident '(' DeclFStream ')' IBlockF    { FunDecl $1 $2 $4 $6 }
        | Type ident '(' ')' IBlockF                { FunDecl $1 $2 [] $5 }
        | Type ident '(' DeclFStream ')' ';'        { FunDecl $1 $2 $4 [] }
        | Type ident '(' ')' ';'                    { FunDecl $1 $2 [] [] }


DeclFStream : DeclF                                 { [$1] }
               | DeclF ',' DeclFStream              { $1:$3 }

DeclF : Type ident                                  { DeclF $1 $2 }
      | Type ident '[' ']'                          { DeclFA $1 $2 }
      | Type '*' ident                              { DeclFP $1 $3 }

IBlockF : '{' Instrs '}'                            { $2 }

---------------- Instructions ------------------------

Instrs :: { [ Stmt ] }
   : Instr                                  { [$1] }
   | Instr Instrs                           { $1:$2 }
   | {- empty -}	                          { [] }

Instr :: { Stmt }
   : Stmt ';'                               { $1 }
   | SpecialStmt                            { $1 }
   | Exp ';'                                { CompExp $1 }

----------------- Statements ---------------------------

Stmt :: { Stmt } -- D:
   : ident '=' Exp                           { Assign $1 $3 }
   | ident '[' num ']' '=' Exp               { AssignArr $1 $3 $6 }
   | Type DeclStream                         { Decls $1 $2 }
   | break                                   { Break }
   | continue                                { Continue }
   | return Exp                              { Return $2 }
   | ident '+' '+'                           { Assign $1 (Add (Var $1) (Num 1)) }

SpecialStmt :: { Stmt }
    : if '(' Exp ')' IBlock                      { If $3 $5 }
    | if '(' Exp ')' IBlock else IBlock          { IfElse $3 $5 $7 }
    | while '(' Exp ')' IBlock                   { While $3 $5 }
    | for '(' Stmt ';' Exp ';' Stmt ')' IBlock    { For $3 $5 $7 $9 }


Type : int   { TypeInt }
     | bool  { TypeBool }
     | strT  { TypeString }
     | charT { TypeChar }
     | float { TypeFloat }
     | void  { TypeVoid }

IBlock :: { [ Stmt ] }
   : '{' Instrs '}'                                 { $2 }
   | Instr                                          { [$1] }



ArgStream :: { [ Exp ] }
ArgStream : Exp                                     { [$1] }
          | Exp ',' ArgStream                       { $1:$3 }

DeclStream :: { [ Decl ] }
DeclStream : Decl                                   { [$1] }
           | Decl ',' DeclStream                    { $1:$3 }

Decl : ident '=' Exp                                { Init $1 $3 }
     | ident                                        { Decl $1 }
     | ident '[' Exp ']'                            { DeclA $1 $3 }
     | ident '[' ']' '=' Exp                        { InitA $1 $5 }
     | ident '[' Exp ']' '=' Exp                    { InitAS $1 $3 $6 }
     | '*' ident                                    { DeclP $2 }
     | '*' ident '=' Exp                            { InitP $2 $4 }




----------------- Expressions ---------------------------

Exp :  '(' Exp ')'                            { $2 }
    | Exp '<' Exp                             { LessThan $1 $3 }
    | Exp "<=" Exp                            { LessEqThan $1 $3 }
    | Exp '>' Exp                             { GreatThan $1 $3 }
    | Exp ">=" Exp                            { GreatEqThan $1 $3 }
    | Exp "==" Exp                            { Equal $1 $3 }
    | Exp "!=" Exp                            { NotEqual $1 $3 }
    | Exp '+' Exp                             { Add $1 $3 }
    | Exp '-' Exp                             { Minus $1 $3 }
    | Exp '*' Exp                             { Mult $1 $3 }
    | Exp '/' Exp                             { Div $1 $3 }
    | Exp '%' Exp                             { Mod $1 $3 }
    | '!' Exp                                 { BoolNot $2 }
    | Exp "||" Exp                            { BoolOr $1 $3 }
    | Exp "&&" Exp                            { BoolAnd $1 $3 }
    | num                                     { Num $1 }
    | true                                    { BoolValue True }
    | false                                   { BoolValue False }
    | ident                                   { Var $1 }
    | ident '(' ArgStream ')'                 { Function $1 $3 }
    | ident '('  ')'                          { Function $1 [] }
    | string                                  { Str $1 }
    | char                                    { Ch $1 }
    | ident '[' Exp ']'                       { ArrayInd $1 $3 }
    | '{' ArgStream '}'                       { ArrayStream $2 }
    | '{' '}'                                 { ArrayStream [] }
    | real                                    { Real $1 }
    | nul                                     { Null }
    | '&' ident                               { Reff $2 }


----------------------------------------------------------------------

{


}
