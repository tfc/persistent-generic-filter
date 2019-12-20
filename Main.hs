{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import           Control.Monad.IO.Class  (liftIO)
import qualified Data.Map.Strict         as Map
import           Data.Maybe              (catMaybes)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Database.Esqueleto
import           Database.Persist.Sqlite hiding ((==.))
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int
    deriving Show
|]

data FilterEntity record where
  FilterEntity :: (PersistField typ,
                   PersistEntity record,
                   StringToVal record typ)
               => EntityField record typ -> FilterEntity record

class StringToVal a b where
    toVal :: EntityField a b -> String -> SqlExpr (Value b)

instance StringToVal a Int where
    toVal _ s = val $ (read s :: Int)

instance StringToVal a String where
    toVal _ = val

entityToString :: FilterEntity a -> FilterDict -> Maybe String
entityToString (FilterEntity e) d = Map.lookup key d
    where key = Text.unpack $ unHaskellName $ fieldHaskell
              $ persistFieldDef e

toQuery :: (FilterEntity a, String)
        -> (SqlExpr (Entity a) -> SqlExpr (Value Bool))
toQuery (FilterEntity e, s) p = p ^. (e) ==. toVal e s

queryFromEntityFields :: PersistEntity a
                      => [FilterEntity a]
                      -> FilterDict
                      -> (SqlExpr (Entity a) -> SqlExpr (Value Bool))
queryFromEntityFields entities filters = let
    entityToKeyValue e = (,) e <$> entityToString e filters
    queries = map toQuery $ catMaybes $ map entityToKeyValue entities
    in foldl1 (\l r x -> l x &&. r x) queries

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

    p <- select . from $ \person -> do
        where_ $ queryFromEntityFields filters userDict person
        return person

    liftIO $ mapM_ (print . entityVal) p
