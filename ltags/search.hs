#!/usr/bin/runhaskell

import Control.Monad (forM)

import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HDBC

terms = ["Uri", "ImageList"]

data Span = Span {
  path :: String,
  start :: Int,
  end :: Int,
  tags :: [String]
} 

instance Show Span where
  show s = path s ++ ":" ++ show (start s) ++ show (tags s)

spanFromSql [path, start, end, tags] = 
  Span (fromSql path) (read (fromSql start)) (read (fromSql end)) 
    (words (fromSql tags))

selectPrefix = "SELECT path, start, end, tags FROM spans " 

main = do
  conn <- connectSqlite3 "../.ltags.sqlite"
  spans <- forM terms $ \term -> do
    spanRecs <- quickQuery' conn 
                (selectPrefix ++ "WHERE tags MATCH ?") 
                [toSql term]
    return (map spanFromSql spanRecs)

  let outsideTags (Span path start end selfTags) = do
      spanRecs <- quickQuery' conn
        (selectPrefix ++ "WHERE start <= ? AND end >= ? AND path = ?")
        [SqlInt32 (fromIntegral start), SqlInt32 (fromIntegral end), 
         toSql path]
      return (selfTags ++ (concat (map (tags . spanFromSql) spanRecs)))
  
  resultingSpans <- mapM outsideTags (concat spans)
  mapM (putStrLn . show) resultingSpans
