{
module Parser where

import Lexer
import Data.Char (isDigit)
import Data.List (sortBy)
import Control.Monad (liftM, ap)
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  FROM      { TokenFrom _ }
  WHERE     { TokenWhere _ }
  SELECT    { TokenSelect _ }
  AS        { TokenAs _ }
  AND       { TokenAnd _ }
  OR        { TokenOr _ }
  NOT       { TokenNot _ }
  ORDER     { TokenOrder _ }
  BY        { TokenBy _ }
  ASC       { TokenAsc _ }
  DESC      { TokenDesc _ }
  EMPTY     { TokenEmpty _ }
  EXISTS    { TokenExists _ }
  EQUALS    { TokenEquals _ }
  COALESCE  { TokenCoalesce _ }
  UNION     { TokenUnion _ }
  INTERSECT { TokenIntersect _ }
  EXCEPT    { TokenExcept _ }
  ident     { TokenIdentifier _ $$ }
  int       { TokenInt _ $$ }
  string    { TokenString _ $$ }
  '=='      { TokenEq _ }
  '!='      { TokenNeq _ }
  '<'       { TokenLt _ }
  '<='      { TokenLe _ }
  '>'       { TokenGt _ }
  '>='      { TokenGe _ }
  '+'       { TokenPlus _ }
  '-'       { TokenMinus _ }
  '*'       { TokenTimes _ }
  '/'       { TokenDiv _ }
  '('       { TokenLParen _ }
  ')'       { TokenRParen _ }
  ','       { TokenComma _ }
  '.'       { TokenDot _ }
  ';'       { TokenSemicolon _ }
  ':'       { TokenColon _ }

%right OR
%right AND
%nonassoc '==' '!=' '<' '<=' '>' '>='
%left '+' '-'
%left '*' '/'
%right NOT
%left '.'
%left UNION INTERSECT EXCEPT

%%

-- Main program structure
Query
    : SingleQuery             { $1 }
    | Query UNION     Query  { Union     $1 $3 }
    | Query INTERSECT Query  { Intersect $1 $3 }
    | Query EXCEPT    Query  { Except    $1 $3 }
    

SingleQuery
    : FromClause WhereClause SelectClause OrderByClause { BaseQuery $1 $2 $3 $4 }
    | FromClause WhereClause SelectClause              { BaseQuery $1 $2 $3 [] }
    | FromClause SelectClause OrderByClause             { BaseQuery $1 Nothing $2 $3 }
    | FromClause SelectClause                          { BaseQuery $1 Nothing $2 [] }
    

FromClause : FROM SourceList                              { $2 }

SourceList : Source                                        { [$1] }
          | Source ',' SourceList                          { $1 : $3 }

Source : ident                                             { Source $1 $1 }
       | ident AS ident                                    { Source $1 $3 }


WhereClause : WHERE Expr                                   { Just $2 }


SelectClause : SELECT SelectItemList                       { $2 }

SelectItemList : SelectItem                                { [$1] }
              | SelectItem ',' SelectItemList              { $1 : $3 }

SelectItem : Expr                                          { SelectItem $1 Nothing }
          | Expr AS ident                                  { SelectItem $1 (Just $3) }


OrderByClause : ORDER BY OrderItemList                     { $3 }

OrderItemList : OrderItem                                  { [$1] }
             | OrderItem ',' OrderItemList                 { $1 : $3 }

OrderItem : Expr                                           { (OrderItem $1, True) }  -- Default ASC
         | Expr ASC                                        { (OrderItem $1, True) }
         | Expr DESC                                       { (OrderItem $1, False) }


Expr : Term                                                { $1 }
     | Expr '==' Expr                                      { BinaryOp Eq $1 $3 }
     | Expr '!=' Expr                                      { BinaryOp Neq $1 $3 }
     | Expr '<' Expr                                       { BinaryOp Lt $1 $3 }
     | Expr '<=' Expr                                      { BinaryOp Le $1 $3 }
     | Expr '>' Expr                                       { BinaryOp Gt $1 $3 }
     | Expr '>=' Expr                                      { BinaryOp Ge $1 $3 }
     | Expr '+' Expr                                       { BinaryOp Plus $1 $3 }
     | Expr '-' Expr                                       { BinaryOp Minus $1 $3 }
     | Expr '*' Expr                                       { BinaryOp Times $1 $3 }
     | Expr '/' Expr                                       { BinaryOp Div $1 $3 }
     | Expr AND Expr                                       { BinaryOp And $1 $3 }
     | Expr OR Expr                                        { BinaryOp Or $1 $3 }
     | NOT Expr                                            { UnaryOp Not $2 }
     | EXISTS '(' Expr ')'                                 { UnaryOp Exists $3 }
     | COALESCE '(' Expr ',' Expr ')'                      { FunctionCall "COALESCE" [$3, $5] }

Term : ident                                               { Identifier $1 }
     | ident '.' ident                                     { ColumnRef $1 $3 }
     | ident '.' int                                       { ColumnIndexRef $1 $3 }
     | string                                              { StringLit $1 }
     | int                                                 { IntLit $1 }
     | EMPTY                                               { StringLit "" }
     | '(' Expr ')'                                        { $2 }

{
parseError :: [Token] -> a
parseError [] =
  error "Parse error: unexpected end of input"
parseError (t:_) =
  let pos          = tokenPosn t
      (line, col)  = getLineCol pos
  in  error $  "Parse error at line " 
            ++ show line ++ ", column "
            ++ show col ++ ": unexpected token"


data Query
  = BaseQuery
      { fromSources   :: [Source]
      , whereClause   :: Maybe Expr
      , selectClause  :: [SelectItem]
      , orderByClause :: [(OrderItem, Bool)]
      }
  | Union     Query Query
  | Intersect Query Query
  | Except    Query Query
  deriving (Show,Eq)


data Source = Source {
    sourceName :: String,
    sourceAlias :: String
} deriving (Show, Eq)

data SelectItem = SelectItem {
    selectExpr :: Expr,
    selectAlias :: Maybe String
} deriving (Show, Eq)

data OrderItem = OrderItem {
    orderExpr :: Expr
} deriving (Show, Eq)

data Expr
    = BinaryOp BinOp Expr Expr
    | UnaryOp UnOp Expr
    | FunctionCall String [Expr]
    | ColumnRef String String  
    | ColumnIndexRef String Int  
    | Identifier String
    | StringLit String
    | IntLit Int
    deriving (Show, Eq)

data BinOp = Eq | Neq | Lt | Le | Gt | Ge
           | Plus | Minus | Times | Div
           | And | Or
    deriving (Show, Eq)

data UnOp = Not | Exists
    deriving (Show, Eq)
}