{
module Lexer where
import Data.Char
import Numeric ( readHex )
}

%wrapper "basic"

$white =  [\ \t\n\r]
$digit =  [0-9]
$alpha =  [_a-zA-Z]
$hexDigit = [0-9a-fA-F]

$comment = $printable # \n

tokens :-

$white+  ; -- ignorar carateres brancos

"//"$comment*             ;

"+"         { \_ -> TOK_PLUS }
"-"         { \_ -> TOK_MINUS }
"*"         { \_ -> TOK_MULT }
"%"         { \_ -> TOK_MOD }
"/"         { \_ -> TOK_DIV }

"("         { \_ -> TOK_LPAREN }
")"         { \_ -> TOK_RPAREN }
"{"         { \_ -> TOK_BRACKET_OPEN }
"}"         { \_ -> TOK_BRACKET_CLOSE }
"["         { \_ -> TOK_SQBRACKET_OPEN}
"]"         { \_ -> TOK_SQBRACKET_CLOSE }

"<"         { \_ -> TOK_LESS }
"<="        { \_ -> TOK_LESSEQ }
">"         { \_ -> TOK_GREATER }
">="        { \_ -> TOK_GREATEREQ }
"=="        { \_ -> TOK_EQUALTO }
"!="        { \_ -> TOK_DIFF }
"="         { \_ -> TOK_EQUAL }

"!"         { \_ -> TOK_NOT }
";"         { \_ -> TOK_SEMICOLON }
","         { \_ -> TOK_COMMA }
"&"         { \_ -> TOK_BIN_AND }
"|"         { \_ -> TOK_BIN_OR }
"&&"        { \_ -> TOK_AND }
"||"        { \_ -> TOK_OR }

"int"       { \_ -> TOK_INT}
"bool"      { \_ -> TOK_BOOL}
"string"    { \_ -> TOK_STRING_TYPE }
"char"      { \_ -> TOK_CHAR_TYPE }
"float"     { \_ -> TOK_FLOAT_TYPE }
"void"     { \_  -> TOK_VOID_TYPE }

"while"     { \_ -> TOK_WHILE }
"for"       { \_ -> TOK_FOR }

"if"        { \_ -> TOK_IF }
"else"      { \_ -> TOK_ELSE }

"break"     { \_ -> TOK_BREAK }
"continue"  { \_ -> TOK_CONTINUE }

"true"      { \_ -> TOK_BOOL_TRUE }
"false"     { \_ -> TOK_BOOL_FALSE }

"return"    { \_ -> TOK_RETURN }

"NULL"                                                  { \_ -> TOK_NULL }
"0x"$hexDigit+                                          { \(x0:xx:xs) -> TOK_NUM (fst(head(readHex xs)))}

$digit+                                                 { \s -> TOK_NUM (read s) }
$digit+("."$digit+)?(("e"|"E")("-"|"+")?$digit+)?       { \s -> TOK_REAL (read s)}


\"$printable*\"               { \s -> TOK_STRING s }
\'$printable\'                { \s -> TOK_CHAR_VALUE s }

$alpha($alpha|$digit)*        { \s -> TOK_IDENT s }

{

data Token = TOK_RETURN
  | TOK_BREAK
  | TOK_CONTINUE
  | TOK_PLUS
  | TOK_MINUS
  | TOK_MULT
  | TOK_BRACKET_OPEN
  | TOK_BRACKET_CLOSE
  | TOK_SQBRACKET_OPEN
  | TOK_SQBRACKET_CLOSE
  | TOK_DIV
  | TOK_LPAREN
  | TOK_RPAREN
  | TOK_MOD
  | TOK_OR
  | TOK_BIN_OR
  | TOK_BIN_AND
  | TOK_LESS
  | TOK_LESSEQ
  | TOK_AND
  | TOK_GREATER
  | TOK_GREATEREQ
  | TOK_EQUALTO
  | TOK_EQUAL
  | TOK_DIFF
  | TOK_WHILE
  | TOK_FOR
  | TOK_IF
  | TOK_ELSE
  | TOK_NOT
  | TOK_SEMICOLON
  | TOK_COMMA
  | TOK_INT
  | TOK_BOOL
  | TOK_NUM Int
  | TOK_REAL Double
  | TOK_IDENT String
  | TOK_STRING String
  | TOK_STRING_TYPE
  | TOK_CHAR_TYPE
  | TOK_FLOAT_TYPE
  | TOK_CHAR_VALUE String
  | TOK_BOOL_FALSE
  | TOK_BOOL_TRUE
  | TOK_NOTHING
  | TOK_NULL
  | TOK_VOID_TYPE
  deriving (Eq, Show)


boolFromString :: String -> Bool
boolFromString str
            | x == "false" = False
            | x == "true" = True
            | otherwise = error "Lexing error D: (Boolean)"
    where x = map toLower str



parseError :: [Token] -> a
parseError toks = error "Parsing error"

}
