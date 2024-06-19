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
generateExpression (FunctionCall name params) = "print(" ++ name ++ "(" ++ intercalate ", " (map generateExpression params) ++ "))"
generateExpression (Assign v e) = v ++ " = " ++ generateExpression e
generateExpression (ListExpression (ListExpr id elems)) =
    id ++ " = [" ++ unwords (map generateLiteral elems) ++ "]"
generateExpression (ForLoopExpression (ForLoop var iterable body)) =
    "for " ++ generateExpression var ++ " in " ++ generateExpression iterable ++ ":\n" ++
    indent (generateExpression body)
generateExpression (ReturnStatement e) = "return " ++ generateExpression e
generateExpression (Conditional cond ifExpr elseExpr) =
    "if " ++ generateComparison cond ++ ":\n" ++
    indent (generateExpressions ifExpr) ++ "\nelse:\n" ++
    indent (generateExpressions elseExpr)
generateExpression (Function name params body) =
    "def " ++ name ++ "(" ++ intercalate ", " params ++ "):\n" ++
    indent (generateExpressions body)
generateExpression _ = " Error "


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
generateCode = generateExpressions
