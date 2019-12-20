{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import           Data.Text               (Text)
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

data FilterEntity where
  FilterEntity :: PersistEntity record => EntityField record typ -> FilterEntity

f :: [FilterEntity] -> [Text]
f [] = []
f (FilterEntity x:rest) = (unHaskellName $ fieldHaskell $ persistFieldDef x):f rest

main :: IO ()
main = runSqlite ":memory:" $ do
    runMigration migrateAll

    insert $ Person "john" 10
    insert $ Person "tom"  10
    insert $ Person "jane" 20
    insert $ Person "john" 30

    johns <- select . from $ \p -> do
        where_ (p ^. PersonName ==. val "john")
        return p

    liftIO $ mapM_ (print . entityVal) johns
    return ()
