module RoleParser (parseRole)where

import AST
import Parser as P
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.String
import Text.Parsec.Language
import qualified Text.Parsec.Token
import Text.Read (Lexeme(String, Char))

parseRole :: Parser Role
parseRole = (P.reserved "SM" *> spaces *> char ':'  *> spaces >> ScrumMaster <$> P.parseStringLiteral)
    <|> (P.reserved "PO"  *> spaces *> char ':'  *> spaces >> ProductOwner <$> P.parseStringLiteral)
    <|> (P.reserved "TM"  *> spaces *> char ':'  *> spaces >> TeamMember <$> P.parseStringLiteral)