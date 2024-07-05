module CodeGenerator (
    generateCode
    ) where

import AST
import Data.List (intercalate)


generateLiteral :: Literal -> String
generateLiteral (IntegerLiteral n) = show n
generateLiteral (FloatingPointLiteral n) = show n
generateLiteral (BooleanLiteral b) = if b then "True" else "False"
generateLiteral (StringLiteral s) = show s


generateBinaryOperator :: BinaryOperator -> String
generateBinaryOperator Add = "+"
generateBinaryOperator Sub = "-"
generateBinaryOperator Mul = "*"
generateBinaryOperator Div = "/"


generateExpression :: Expression -> String
generateExpression (Literal l) = generateLiteral l
generateExpression (Variable v) = v
generateExpression (FunctionCall name params) = name ++ "(" ++ intercalate ", " (map generateExpression params) ++ ")"
generateExpression (Assign v e) = v ++ " = " ++ generateExpression e
generateExpression (ListExpression (ListExpr id elems)) =
    id ++ " = [" ++ unwords (map generateLiteral elems) ++ "]"
generateExpression (ForLoopExpression (ForLoop var iterable body)) =
    "for " ++ generateExpression var ++ " in " ++ generateExpression iterable ++ ":\n" ++
    indent (generateExpressions body)
generateExpression (ReturnStatement e) = "return " ++ generateExpression e
generateExpression (Conditional cond ifExpr elseExpr) =
    "if " ++ generateComparison cond ++ ":\n" ++
    indent (generateExpressions ifExpr) ++ "else:\n" ++
    indent (generateExpressions elseExpr)
generateExpression (Function name params body) =
    "def " ++ name ++ "(" ++ intercalate ", " (map generateParam params) ++ "):\n" ++
    indent (generateExpressions body)
generateExpression (Role r) = generateRole r
generateExpression (UserStory (UserStoryExpr id formatBlock)) = generateUserStory id formatBlock


generateParam :: Parameter -> String
generateParam (Parameter param) = param


generateRole :: Role -> String
generateRole (ScrumMaster sm) = "sm = ScrumMaster(\""++ sm ++ "\") \nmanager.setScrumMaster(sm)"
generateRole (ProductOwner po) = "po = ProductOwner(\""++ po ++ "\") \nmanager.setProductOwner(sm)"
generateRole (TeamMember tm) = "manager.addTeamMember(TeamMember(\"" ++ tm ++"\"))"

generateComparison :: Comparison -> String
generateComparison (Comp left op right) =
    generateExpression left ++ " " ++ generateCompOperator op ++ " " ++ generateExpression right


generateCompOperator :: CompOperator -> String
generateCompOperator Equal = "=="
generateCompOperator NotEqual = "!="
generateCompOperator Less = "<"
generateCompOperator LessEqual = "<="
generateCompOperator Greater = ">"
generateCompOperator GreaterEqual = ">="


generateExpressions :: [Expression] -> String
generateExpressions = unlines . map generateExpression


indent :: String -> String
indent = unlines . map ("    " ++) . lines


generateCode :: [Expression] -> String
generateCode expressions = generateImports ++ generateExpressions expressions ++ generateView

generateImports :: String
generateImports = unlines
    [ "from TeamMember import TeamMember"
    , "from ScrumMaster import ScrumMaster"
    , "from ProductOwner import ProductOwner"
    , "from Manager import Manager"
    , "from UserStory import UserStory, UserStoryType"
    , ""
    , "manager = Manager()"
    , ""
    ]

generateView :: String
generateView = "\nmanager.showViewIfScrumAdded()"


generateUserStoryType :: UserStoryType -> String
generateUserStoryType Feature = "UserStoryType.FEATURE"
generateUserStoryType Spike = "UserStoryType.SPIKE"
generateUserStoryType POC = "UserStoryType.POC"
generateUserStoryType Fix = "UserStoryType.FIX"
generateUserStoryType HotFix = "UserStoryType.HOTFIX"

generateUserStoryFormatBlock :: UserStoryFormatBlock -> String
generateUserStoryFormatBlock (UserStoryFormatBlock t ty ps ds et ac) = unlines
    [ "    " ++ show t ++ ","
    , "    " ++ generateUserStoryType ty ++ ","
    , "    " ++ generateAssignedTo ps ++ ","
    , "    " ++ show ds ++ ","
    , "    " ++ show et ++ ","
    , "    " ++ show ac
    ]

generateAssignedTo :: Maybe AssignedTo -> String
generateAssignedTo Nothing = "None"
generateAssignedTo (Just (ScrumMaster sm)) = "ScrumMaster(\"" ++ sm ++ "\")"
generateAssignedTo (Just (ProductOwner po)) = "ProductOwner(\"" ++ po ++ "\")"
generateAssignedTo (Just (TeamMember tm)) = "TeamMember(\"" ++ tm ++ "\")"

generateUserStory :: String -> UserStoryFormatBlock -> String
generateUserStory id formatBlock = unlines
    [ "user_story = UserStory("
    , "    " ++ show id ++ ","
    , generateUserStoryFormatBlock formatBlock
    , ")"
    , "manager.addUserStory(user_story)"
    ]