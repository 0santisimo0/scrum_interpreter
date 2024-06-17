module AST(Expression(..), 
            Identifier, 
            Literal(..), 
            BinaryOperator(..),
            BinaryExpression(..),
            ListExpression(..),
            ForLoop(..),
            UserStoryType(..),
            UserStoryFormatBlock(..),
            UserStory(..),
            UserStoryID,
            Title,
            AssignedTo,
            Description,
            Estimation,
            Acceptance,
            Role(..)) 
        where 

data Program = Expression deriving(Show, Eq)

type Identifier = String 
type Variable = Identifier

data Literal = BooleanLiteral Bool
            | IntegerLiteral Integer
            | FloatingPointLiteral Double
            | StringLiteral String
            deriving(Show, Eq)

data Expression = Assign Identifier Expression
                | LiteralExpr Literal
                | Variable Variable
                | Conditional Conditional
                | BinaryExpression BinaryExpression
                | ListExpression ListExpression
                | ForLoopExpression ForLoop
                | Role Role
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

data Comparison = Comp Expression CompOperator Expression deriving(Show, Eq)

data Conditional = Comparison Expression Expression deriving(Show, Eq)

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

data UserStory = UserStoryExpr UserStoryID UserStoryFormatBlock deriving(Show, Eq)

data UserStoryFormatBlock = UserStoryFormatBlock Title UserStoryType AssignedTo Description Estimation Acceptance deriving(Show, Eq)

data UserStoryType = Feature
                    | Spike 
                    | POC
                    | Fix 
                    | HotFix
                    deriving(Show, Eq)

type UserStoryID = String
type Title = String
type AssignedTo = Role
type Description = String
type Estimation = Integer
type Acceptance = String