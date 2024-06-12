module AST(Expression(..), Identifier) where 

data Program = Program


data Expression = Assign Identifier Expression
                | Value Int
                deriving(Show, Eq)

type Identifier = String 