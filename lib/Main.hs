{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import           Control.Monad           (forM_)
import           Control.Monad.IO.Class  (liftIO)
import           Data.List               (intercalate)
import           Database.Esqueleto
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           System.IO               (hFlush, stdout)

import           FilterQueryLib

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int
    deriving Show
|]

-- This is what the developer defines in the source to enable
-- specific columns in the DB table for filtering

filters = [FilterEntity PersonAge, FilterEntity PersonName]
userQuery = parseUserQuery filters

main :: IO ()
main = runSqlite ":memory:" $ do
    runMigration migrateAll

    insert $ Person "john" 10
    insert $ Person "tom"  10
    insert $ Person "jane" 20
    insert $ Person "john" 30

    allPeople <- select . from $ \person ->
        return (person :: SqlExpr (Entity Person))

    liftIO $ do
        putStrLn "The following people exist in the DB:"
        mapM_ (print . entityVal) allPeople

        putStrLn "\nPlease enter a query. Examples:"
        putStrLn "    age > 12 and name = john"
        putStrLn "    name = jane or (name = john and age > 10)"

        putStrLn "\nThe following fields and operators are available:"
        forM_ (map listFields filters) $ \(fieldName, operators) ->
            putStrLn $ "Field \"" ++ fieldName ++ "\", operators: "
                ++ unwords operators

    replLoop

replLoop = do
    line <- liftIO $ do
        putStr "query> "
        hFlush stdout
        getLine
    case userQuery line of
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
