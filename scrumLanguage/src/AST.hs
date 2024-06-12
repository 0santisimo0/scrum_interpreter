module AST(Expression(..), Identifier, Literal(..), Role(..)) where 

data Program = Expression deriving(Show, Eq)


data Expression = Assign Identifier Expression
                | Role Role
                | Literal Literal
                | FunctionCall String [Expression]
                | Conditional BoolExpression Expression Expression 
                deriving(Show, Eq)

type Identifier = String 

data Role = ScrumMaster String
          | ProductOwner String
          | TeamMember String
          deriving(Show, Eq)

data Literal = BooleanLiteral Bool
             | IntegerLiteral Int
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



