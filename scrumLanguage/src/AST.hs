module AST(Expression(..), 
            Identifier, 
            Literal(..), 
            Role(..),
            Comparison(..),
            CompOperator(..),
            BinaryOperator(..),
            BinaryExpression(..),
            ListExpression(..),
            ForLoop(..)) 
        where 

data Program = Expression deriving(Show, Eq)

type Identifier = String 

data Literal = BooleanLiteral Bool
            | IntegerLiteral Integer
            | FloatingPointLiteral Double
            | StringLiteral String
            deriving(Show, Eq)


data Expression = Assign Identifier Expression
                | Literal Literal
                | Variable Identifier
                | Conditional Comparison [Expression] [Expression]
                | Function String [String] [Expression]
                | BinaryExpression BinaryExpression
                | ListExpression ListExpression
                | ForLoopExpression ForLoop
                | Role Role
                | ReturnStatement Expression
                | UserStory UserStory
                | FunctionCall String [Expression]
                deriving(Show, Eq)


data CompOperator = Equal
                | NotEqual
                | Less
                | LessEqual
                | Greater
                | GreaterEqual 
                deriving(Show, Eq)

data Comparison = Comp Expression CompOperator Expression
                deriving (Show, Eq)

data BinaryExpression = BinExpr Literal BinaryOperator Literal
                    deriving (Show, Eq)

data BinaryOperator = Add    
                | Sub   
                | Mul   
                | Div   
                deriving (Show, Eq)

data ListExpression = ListExpr Identifier [Literal]
                    deriving (Show, Eq)

data ForLoop = ForLoop Expression Expression Expression
                deriving (Show, Eq)

data Role = ScrumMaster String
            | ProductOwner String
            | TeamMember String
            deriving(Show, Eq)

data UserStory = UserStoryID UserStoryFormatBlock deriving(Show, Eq)

data UserStoryFormatBlock = Title UserStoryType AssignedTo Description Estimation Acceptance deriving(Show, Eq)

data UserStoryType = Feature String
                    | Spike String
                    | POC String
                    | Fix String
                    | HotFix String
                    deriving(Show, Eq)

type UserStoryID = Int
type Title = String
type AssignedTo = Role
type Description = String
type Estimation = Int
type Acceptance = String