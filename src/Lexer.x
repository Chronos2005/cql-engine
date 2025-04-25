{
module Lexer where

import Data.Char (isDigit, isAlpha, isAlphaNum)
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$alphaNum = [a-zA-Z0-9]
$symbol = [\!\#\$\%\&\*\+\-\/\<\=\>\?\@\\\^\|\~]


tokens :-

  $white+                           ;   -- ignore whitespace
  "--".*                              ;   -- single line comment
 

  -- Keywords
  FROM                                { \p s -> TokenFrom p }
  WHERE                               { \p s -> TokenWhere p }
  SELECT                              { \p s -> TokenSelect p }
  AS                                  { \p s -> TokenAs p }
  AND                                 { \p s -> TokenAnd p }
  OR                                  { \p s -> TokenOr p }
  NOT                                 { \p s -> TokenNot p }
  ORDER                               { \p s -> TokenOrder p }
  BY                                  { \p s -> TokenBy p }
  ASC                                 { \p s -> TokenAsc p }
  DESC                                { \p s -> TokenDesc p }
  EMPTY                               { \p s -> TokenEmpty p }
  EXISTS                              { \p s -> TokenExists p }
  EQUALS                              { \p s -> TokenEquals p }
  COALESCE                            { \p s -> TokenCoalesce p }
  UNION                               { \p s -> TokenUnion p }
  INTERSECT                           { \p s -> TokenIntersect p }
  EXCEPT                              { \p s -> TokenExcept p }

  -- Identifiers and literals
  $alpha [$alphaNum \_ \']*           { \p s -> TokenIdentifier p s }
  $digit+                             { \p s -> TokenInt p (read s) }
  \"[^\"]*\"                        { \p s -> TokenString p (init (tail s)) }  -- String literals in double quotes
 

  -- Operators and delimiters
  "=="                                { \p s -> TokenEq p }
  "!="                                { \p s -> TokenNeq p }
  "<"                                 { \p s -> TokenLt p }
  "<="                                { \p s -> TokenLe p }
  ">"                                 { \p s -> TokenGt p }
  ">="                                { \p s -> TokenGe p }
  "+"                                 { \p s -> TokenPlus p }
  "-"                                 { \p s -> TokenMinus p }
  "*"                                 { \p s -> TokenTimes p }
  "/"                                 { \p s -> TokenDiv p }
  "("                                 { \p s -> TokenLParen p }
  ")"                                 { \p s -> TokenRParen p }
  ","                                 { \p s -> TokenComma p }
  "."                                 { \p s -> TokenDot p }
  ";"                                 { \p s -> TokenSemicolon p }
  ":"                                 { \p s -> TokenColon p }
  




{


-- Extract line and column from AlexPosn
getLineCol :: AlexPosn -> (Int, Int)
getLineCol (AlexPn _ line col) = (line, col)

data PosnToken = PT AlexPosn Token deriving (Eq, Show)

-- The token type:
data Token
  = TokenFrom AlexPosn
  | TokenWhere AlexPosn
  | TokenSelect AlexPosn
  | TokenAs AlexPosn
  | TokenAnd AlexPosn
  | TokenOr AlexPosn
  | TokenNot AlexPosn
  | TokenOrder AlexPosn
  | TokenBy AlexPosn
  | TokenAsc AlexPosn
  | TokenDesc AlexPosn
  | TokenEmpty AlexPosn
  | TokenExists AlexPosn
  | TokenEquals AlexPosn
  | TokenCoalesce AlexPosn
  | TokenUnion AlexPosn
  | TokenIntersect AlexPosn
  | TokenExcept AlexPosn
  | TokenIdentifier AlexPosn String
  | TokenInt  AlexPosn Int
  | TokenString  AlexPosn String
  | TokenEq AlexPosn
  | TokenNeq AlexPosn
  | TokenLt AlexPosn
  | TokenLe AlexPosn
  | TokenGt AlexPosn
  | TokenGe AlexPosn
  | TokenPlus AlexPosn
  | TokenMinus AlexPosn
  | TokenTimes AlexPosn
  | TokenDiv AlexPosn
  | TokenLParen AlexPosn
  | TokenRParen AlexPosn
  | TokenComma AlexPosn
  | TokenDot AlexPosn
  | TokenSemicolon AlexPosn
  | TokenColon AlexPosn
  deriving (Eq, Show) 

-- Extract position from a token
tokenPosn :: Token -> AlexPosn
tokenPosn (TokenFrom p) = p
tokenPosn (TokenWhere p) = p
tokenPosn (TokenSelect p) = p
tokenPosn (TokenAs p) = p
tokenPosn (TokenAnd p) = p
tokenPosn (TokenOr p) = p
tokenPosn (TokenNot p) = p
tokenPosn (TokenOrder p) = p
tokenPosn (TokenBy p) = p
tokenPosn (TokenAsc p) = p
tokenPosn (TokenDesc p) = p
tokenPosn (TokenEmpty p) = p
tokenPosn (TokenExists p) = p
tokenPosn (TokenEquals p) = p
tokenPosn (TokenCoalesce p) = p
tokenPosn (TokenUnion p) = p
tokenPosn (TokenIntersect p) = p
tokenPosn (TokenExcept p) = p
tokenPosn (TokenIdentifier p _) = p
tokenPosn (TokenInt p _) = p
tokenPosn (TokenString p _) = p
tokenPosn (TokenEq p) = p
tokenPosn (TokenNeq p) = p
tokenPosn (TokenLt p) = p
tokenPosn (TokenLe p) = p
tokenPosn (TokenGt p) = p
tokenPosn (TokenGe p) = p
tokenPosn (TokenPlus p) = p
tokenPosn (TokenMinus p) = p
tokenPosn (TokenTimes p) = p
tokenPosn (TokenDiv p) = p
tokenPosn (TokenLParen p) = p
tokenPosn (TokenRParen p) = p
tokenPosn (TokenComma p) = p
tokenPosn (TokenDot p) = p
tokenPosn (TokenSemicolon p) = p
tokenPosn (TokenColon p) = p


-- Helper function for formatting error messages
formatError :: Token -> String -> String  
formatError token msg = let ( line, col) = getLineCol (tokenPosn token)
                       in "Error at line " ++ show line ++ ", column " ++ show col ++ ": " ++ msg


}