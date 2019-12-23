{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import           Control.Monad           (liftM2)
import           Control.Monad.IO.Class  (liftIO)
import qualified Data.Map.Strict         as Map
import           Data.Maybe              (catMaybes)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Database.Esqueleto
import           Database.Persist.Sqlite hiding ((<.), (==.), (>.), (||.))
import           Database.Persist.TH
import           System.IO               (hFlush, stdout)
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.String

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int
    deriving Show
|]

-------------------------------------------------------------------------------

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

dbExprParser = dbFactorParser `chainl1` dbConjunctionOp
dbFactorParser = spaces
               *> ((char '(' *> dbExprParser <* char ')') <|> dbPredicateParsers)
    where
        dbPredicateParsers = choice $ map parserForEntity filters

-------------------------------------------------------------------------------

-- This is what the developer defines in the source to enable
-- specific columns in the DB table for filtering

filters = [FilterEntity PersonAge, FilterEntity PersonName]

type FilterDict = Map.Map String String

-- This is what comes from the user's HTTP query:
-- https://foo.bar/get_people?name=john&age=10
userDict :: FilterDict
userDict = Map.fromList [("name", "john"), ("age", "10")]

main :: IO ()
main = runSqlite ":memory:" $ do
    runMigration migrateAll

    insert $ Person "john" 10
    insert $ Person "tom"  10
    insert $ Person "jane" 20
    insert $ Person "john" 30

    allPeople <- select . from $ \person -> do
        return (person :: SqlExpr (Entity Person))

    liftIO $ do
        putStrLn "The following people exist in the DB:"
        mapM_ (print . entityVal) allPeople

        putStrLn "\nPlease enter a query. Examples:"
        putStrLn "    age > 12 and name = john"
        putStrLn "    name = jane or (name = john and age > 10)"

    replLoop

replLoop = do
    line <- liftIO $ do
        putStr "query> "
        hFlush stdout
        getLine
    case runParser dbExprParser () "bla" line of
        Left err -> liftIO $ do
            putStrLn $ "Error parsing \"" ++ line ++ "\":"
            print err
        Right predicate -> do
            p <- select . from $ \person -> do
                where_ $ predicate person
                return person
            liftIO $ do
                putStrLn $ "Results of query \"" ++ line ++ "\":"
                mapM_ (print . entityVal) p
    replLoop
