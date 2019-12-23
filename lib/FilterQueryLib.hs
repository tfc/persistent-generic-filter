{-# LANGUAGE FlexibleContexts      #-}
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
-- TODO:
-- - provide regex op for postgres
-- - provide Maybe X case for optional fields

parserForEntity :: FilterEntity a -> Parser (SqlPredicate a)
parserForEntity (FilterEntity e) = do
    string $ Text.unpack $ unHaskellName $ fieldHaskell $ persistFieldDef e
    spaces
    op <- choice $ map (\(str, op) -> string str *> pure op) $ predicateOps e
    spaces
    value <- valueParser e
    return (\p -> (p ^. e) `op` (val value))

dbConjunctionOp :: Parser (SqlPredicate a -> SqlPredicate a -> SqlPredicate a)
dbConjunctionOp = spaces *> ((f "and" (&&.) <|> f "or" (||.)))
    where f token operator = string token *> pure (liftM2 operator)

dbExprParser filters = dbFactorParser filters `chainl1` dbConjunctionOp
dbFactorParser filters = spaces *> choice [
        char '(' *> dbExprParser filters <* char ')',
        choice $ map parserForEntity filters
                                          ]

-- TODO:
--  - use "try" to fix the problem of multiple fields having same prefix in name
--  - provide some better parser for <= and >= cases etc.

parseUserQuery filters = runParser (dbExprParser filters) () "bla"
