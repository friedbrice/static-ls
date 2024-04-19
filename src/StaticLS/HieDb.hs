{-# LANGUAGE ViewPatterns #-}

module StaticLS.HieDb (lookupHieFileFromHie, searchExports) where

import Data.List (intercalate)
import Database.SQLite.Simple
import GHC.Plugins (ModuleName)
import HieDb
import qualified Data.Text as T

{- | Lookup 'HieModule' row from 'HieDb' given the path to the Haskell hie file
A temporary function until this is supported in hiedb proper
-}
lookupHieFileFromHie :: HieDb -> FilePath -> IO (Maybe HieModuleRow)
lookupHieFileFromHie (getConn -> conn) fp = do
    files <- query conn "SELECT * FROM mods WHERE hieFile = ?" (Only fp)
    case files of
        [] -> return Nothing
        [x] -> return $ Just x
        xs ->
            error $
                "DB invariant violated, hieFile in mods not unique: "
                    ++ show fp
                    ++ ". Entries: "
                    ++ intercalate ", " (map (show . toRow) xs)

searchExports :: HieDb -> T.Text -> IO [ModuleName]
searchExports (getConn -> conn) qry =
    query conn "SELECT mods.mod FROM exports JOIN mods USING (hieFile) WHERE occ LIKE ?" (Only $ "%" <> qry <> "%")
