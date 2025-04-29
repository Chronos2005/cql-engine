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
  FROM                                { \p _ -> TokenFrom p }
  WHERE                               { \p _ -> TokenWhere p }
  SELECT                              { \p _ -> TokenSelect p }
  AS                                  { \p _ -> TokenAs p }
  AND                                 { \p _ -> TokenAnd p }
  OR                                  { \p _ -> TokenOr p }
  NOT                                 { \p _ -> TokenNot p }
  ORDER                               { \p _ -> TokenOrder p }
  BY                                  { \p _ -> TokenBy p }
  ASC                                 { \p _ -> TokenAsc p }
  DESC                                { \p _ -> TokenDesc p }
  EMPTY                               { \p _ -> TokenEmpty p }
  EXISTS                              { \p _ -> TokenExists p }
  EQUALS                              { \p _ -> TokenEquals p }
  COALESCE                            { \p _ -> TokenCoalesce p }
  UNION                               { \p _ -> TokenUnion p }
  INTERSECT                           { \p _ -> TokenIntersect p }
  EXCEPT                              { \p _ -> TokenExcept p }

  -- Identifiers and literals
  $alpha [$alphaNum \_ \']*           { \p s -> TokenIdentifier p s }
  $digit+                             { \p s -> TokenInt p (read s) }
  \"[^\"]*\"                        { \p s -> TokenString p (init (tail s)) }  -- String literals in double quotes
 

  -- Operators and delimiters
  "=="                                { \p _ -> TokenEq p }
  "!="                                { \p _ -> TokenNeq p }
  "<"                                 { \p _ -> TokenLt p }
  "<="                                { \p _ -> TokenLe p }
  ">"                                 { \p _ -> TokenGt p }
  ">="                                { \p _ -> TokenGe p }
  "+"                                 { \p _ -> TokenPlus p }
  "-"                                 { \p _ -> TokenMinus p }
  "*"                                 { \p _ -> TokenTimes p }
  "/"                                 { \p _ -> TokenDiv p }
  "("                                 { \p _ -> TokenLParen p }
  ")"                                 { \p _ -> TokenRParen p }
  ","                                 { \p _ -> TokenComma p }
  "."                                 { \p _ -> TokenDot p }
  ";"                                 { \p _ -> TokenSemicolon p }
  ":"                                 { \p _ -> TokenColon p }
  




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