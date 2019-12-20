{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}

import           Control.Monad.IO.Class  (liftIO)
import qualified Data.Map.Strict         as Map
import           Data.Maybe              (catMaybes, fromJust)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Data.Typeable           (cast)
import           Database.Esqueleto
import           Database.Persist.Sqlite hiding ((==.))
import           Database.Persist.TH
import           Generics.Eot

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int
    deriving Show
|]

data FilterEntity record where
  FilterEntity :: (PersistField typ, PersistEntity record, StringToVal record typ)
               => EntityField record typ -> FilterEntity record

f :: [FilterEntity Person] -> [Text]
f [] = []
f (FilterEntity x:rest) = (unHaskellName $ fieldHaskell $ persistFieldDef x):f rest

type FilterDict = Map.Map String String

m :: FilterDict
m = Map.fromList [("name", "john"), ("age", "10")]

g :: [FilterEntity Person] -> FilterDict -> String
g filterEntities keyMap = foldl1 andConcat strings
    where
        entityToKeyValue (FilterEntity e) = let
            key = Text.unpack $ unHaskellName $ fieldHaskell $ persistFieldDef e
            mVal = Map.lookup key keyMap
            in (,) key <$> mVal
        toStrings (k, v) = "(" ++ k ++ " == " ++ v ++ ")"
        strings = map toStrings $ catMaybes $ map entityToKeyValue filterEntities
        andConcat l r = l ++ " AND " ++ r

class StringToVal a b where
    toVal :: EntityField a b -> String -> SqlExpr (Value b)

instance StringToVal a Int where
    toVal _ s = val $ (read s :: Int)

instance StringToVal a String where
    toVal _ = val

entityToString :: FilterEntity Person -> FilterDict -> Maybe String
entityToString (FilterEntity e) d = Map.lookup key d
    where key = Text.unpack $ unHaskellName $ fieldHaskell
              $ persistFieldDef e

toQuery :: (FilterEntity Person, String)
        -> (SqlExpr (Entity Person) -> SqlExpr (Value Bool))
toQuery (FilterEntity e, s) p =
    p ^. (e) ==. toVal e s

queryFromEntityFields :: PersistEntity Person
                      => [FilterEntity Person]
                      -> FilterDict
                      -> (SqlExpr (Entity Person) -> SqlExpr (Value Bool))
queryFromEntityFields entities filters = let
    andConcat l r x = l x &&. r x
    entityToKeyValue e = (,) e <$> (entityToString e filters)
    queries = map toQuery $ catMaybes $ map entityToKeyValue entities
    in foldl1 andConcat queries


main :: IO ()
main = runSqlite ":memory:" $ do
    runMigration migrateAll

    insert $ Person "john" 10
    insert $ Person "tom"  10
    insert $ Person "jane" 20
    insert $ Person "john" 30

    johns <- select . from $ \p -> do
        --where_ (p ^. PersonName ==. val "john")
        where_ (queryFromEntityFields [FilterEntity PersonAge, FilterEntity PersonName] m p)
        return p

    liftIO $ mapM_ (print . entityVal) johns
    return ()
