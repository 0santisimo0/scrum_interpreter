module AST(Expression(..), Identifier, Literal(..), Comparison(..), CompOperator(..)) where 

data Program = Program


data Expression = Assign Identifier Expression
                | Literal Literal
                | FunctionCall String [Expression]
                | Conditional Comparison Expression Expression 
                deriving(Show, Eq)

type Identifier = String 

data Literal = BooleanLiteral Bool
             | IntegerLiteral Int
             | FloatingPointLiteral Double
             | StringLiteral String
             deriving (Eq, Show)

{-
data BoolExpression = BooleanTerm
                    | BoolOp BoolExpression BoolOperator BooleanTerm deriving (Eq, Show)

data BooleanTerm = BoolFactor
                 | BoolTerm BoolOperator BoolFactor 
                 deriving (Eq, Show)

data BoolFactor = Comparison
                | Not BoolFactor 
                deriving (Eq, Show)
-}
data Comparison = Comp Expression CompOperator Expression 
                deriving (Eq, Show)

data CompOperator = Equal
                  | NotEqual
                  | Less
                  | LessEqual
                  | Greater
                  | GreaterEqual 
                deriving (Eq, Show)