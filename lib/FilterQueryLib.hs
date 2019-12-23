{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FilterQueryLib (
    EntityField(..),
    FilterEntity(..),
    parseUserQuery
    ) where

import           Control.Monad          (liftM2)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Database.Esqueleto
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.String

data FilterEntity record where
  FilterEntity :: (PersistField typ,
                   PersistEntity record,
                   ParseableFieldEntity record typ)
               => EntityField record typ -> FilterEntity record

-------------------------------------------------------------------------------

type SqlPredicate a = SqlExpr (Entity a) -> SqlExpr (Value Bool)

class ParseableFieldEntity a b where
    predicateOps :: EntityField a b
                 -> [(String, SqlExpr (Value b) -> SqlExpr (Value b) -> SqlExpr (Value Bool))]
    valueParser :: EntityField a b -> Parser b

instance ParseableFieldEntity a Int where
    predicateOps _ = [("=", (==.)), ("<", (<.)), (">", (>.))]
    valueParser _ = read <$> (spaces *> many1 digit)

instance ParseableFieldEntity a [Char] where
    predicateOps _ = [("=", (==.))]
    valueParser _ = spaces *> choice [
        char '"' *> many1 (noneOf "\"") <* char '"',
        many1 alphaNum
                                     ]
parserForEntity :: FilterEntity a -> Parser (SqlPredicate a)
parserForEntity (FilterEntity e) = do
    let key = Text.unpack $ unHaskellName $ fieldHaskell $ persistFieldDef e
    string key
    spaces
    op <- choice $ map (\(str, op) -> string str *> pure op) $ predicateOps e
    spaces
    value <- valueParser e
    return (\p -> (p ^. e) `op` (val value))

dbConjunctionOp :: Parser (SqlPredicate a -> SqlPredicate a -> SqlPredicate a)
dbConjunctionOp = spaces *> ( (string "and" *> pure (liftM2 (&&.))) <|> (string "or" *> pure (liftM2 (||.))))

dbExprParser filters = dbFactorParser filters `chainl1` dbConjunctionOp
dbFactorParser filters = spaces
               *> ((char '(' *> dbExprParser filters <* char ')') <|> dbPredicateParsers)
    where
        dbPredicateParsers = choice $ map parserForEntity filters

parseUserQuery filters = runParser (dbExprParser filters) () "bla"
