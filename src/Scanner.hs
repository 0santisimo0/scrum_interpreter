{-# LANGUAGE UndecidableInstances #-}
module Scanner where
import Data.Char (isAlphaNum, isDigit)

type Col = Int
type Line = Int
type Value = String
type Input = String

data Token = Token Type Value Line Col

data Type = String 
          | Num
          | Variable
          | Bool
          | Error
          | EqualVariable
          | Keyword
          deriving(Eq, Ord)

instance Show Token where
    show (Token t v l c) = show t ++ show v ++ " " ++ show l ++ " " ++ show c ++ "\n"

instance Show Type where
    show String = "String: "
    show Num = "Number: "
    show Variable = "Variable: "
    show Bool = "Bool: "
    show EqualVariable = "EqualVar: "
    show Error = "Error: "
    show Keyword = "Keyword: "
    -- show ImageSlide = "Image: "

instance (Eq Type) => (Eq Token) where
    (Token String _ _ _) == (Token String _ _ _) = True
    (Token Num n1 _ _) == (Token Num n2 _ _) = n1 == n2
    (Token Bool n1 _ _) == (Token Bool n2 _ _) = n1 == n2
    (Token Variable _ _ _) == (Token Variable _ _ _) = True
    (Token EqualVariable _ _ _) == (Token EqualVariable _ _ _) = True
    (Token Keyword k1 _ _) == (Token Keyword k2 _ _) = k1 == k2
    (Token Error k1 _ _) == (Token Error k2 _ _) = k1 == k2
    (Token t1 s1 _ _ ) == (Token t2 s2 _ _ ) = t1 == t2 && s1 == s2

instance Ord Token where
    compare :: Token -> Token -> Ordering
    compare x y | x == y = EQ
                | x <= y = LT
                | otherwise = GT
    (Token t1 s1 _ _ ) <= (Token t2 s2 _ _ ) = t1 < t2 || (t1 == t2 && s1 <= s2)

scanner :: Input -> [Token]
scanner xs = scan xs 1 1

scan :: Input -> Line -> Col -> [Token]
scan [] _ _ = []
scan (x : xs) l c
  | x == ' ' = scan xs l (c + 1)
  | x == '\n' = scan xs (l + 1) 1
  | x == 'v' = verifyIfFunct xs l c
  | isAlphaNum x = if isDigit x
                then let (num, rest) = span isDigit (x:xs)
                    in Token Num num l c : scan rest l (c + length num)
                else let (word, rest) = span isAlphaNumOrSpace (x:xs)
                    in if word == "True" || word == "False"
                        then Token Bool word l c : scan rest l (c + length word)
                        else Token Error [x] l c : scan xs l (c + 1)
    | otherwise = Token Error [x] l c : scan xs l (c + 1)
    where isAlphaNumOrSpace y = isAlphaNum y || y `elem` " :;-.(),"


verifyIfFunct :: Input -> Line -> Col -> [Token]
verifyIfFunct [] _ _ = []
verifyIfFunct ('a':'r':xs) l c = Token Variable "var" l c : scanVariable xs l (c+2)
verifyIfFunct xs l c = scan xs l c

scanVariable :: Input -> Line -> Col -> [Token]
scanVariable [] _ _ = []
scanVariable (x:xs) l c 
    | x == '\n' = scan xs (l+1) c
    | x == '=' = Token EqualVariable [x] l c : scanVariable xs l (c+1)
    | x == ' ' = scanVariable xs l (c+1)
    | isAlphaNum x = if isDigit x
                then let (num, rest) = span isDigit (x:xs)
                    in Token Num num l c : scanVariable rest l (c + length num)
                else let (word, rest) = span isAlphaNumOrSpace (x:xs)
                    in if word == "True" || word == "False"
                        then Token Bool word l c : scanVariable rest l (c + length word)
                        else Token String word l c : scanVariable rest l (c + length word)
    | otherwise = Token Error [x] l c : scanVariable xs l (c + 1)
    where isAlphaNumOrSpace y = isAlphaNum y || y `elem` " :;-.(),"
