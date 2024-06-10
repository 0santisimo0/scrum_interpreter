
module AbstractGrammar where 

type Strings = String

data Program = Expression deriving (Eq, Show)


-- data VarType = String String
--              | Bool Bool 
--              | Integer Int
--              | Role String
--              deriving (Eq, Show)


data Expression = Literal Literal
                | VariableLiteral String Literal
                | FunctionCall String [Expression]
                | Conditional BoolExpression Expression Expression 
                deriving (Eq, Show)



data Literal = BooleanLiteral Bool
             | IntegerLiteral Integer
             | FloatingPointLiteral Double
             | StringLiteral String
             deriving (Eq, Show)

data BoolExpression = BooleanTerm
                    | BoolOp BoolExpression BoolOperator BooleanTerm deriving (Eq, Show)

data BooleanTerm = BoolFactor
                 | BoolTerm BoolOperator BoolFactor deriving (Eq, Show)

data BoolFactor = Comparison
                | Not BoolFactor deriving (Eq, Show)

data Comparison = Comp Expression CompOperator Expression deriving (Eq, Show)

data BoolOperator = And | Or deriving (Eq, Show)

data CompOperator = Equal
                  | NotEqual
                  | Less
                  | LessEqual
                  | Greater
                  | GreaterEqual deriving (Eq, Show)
