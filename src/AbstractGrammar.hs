
module AbstractGrammar where 

type Strings = String

data Variable = Variable VarType String
    deriving Show

data VarType = String | Bool | Integer
    deriving Show


    
