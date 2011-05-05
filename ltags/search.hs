#!/usr/bin/runhaskell

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

main = do
  conn <- connectSqlite3 "../.ltags.sqlite"
  let searchIndex term = do
      spanRecs <- quickQuery' conn 
        ("SELECT path, start, end, tags FROM spans WHERE tags MATCH ? ") 
        [toSql term]
      return (map spanFromSql spanRecs)
  spans <- concat (mapM searchIndex terms)

  let allTags (Span path start end selfTags) = do
      -- query params didn't work on numbers for some reason
      spanRecs <- quickQuery' conn
        ("SELECT path, start, end, tags FROM spans WHERE start <= " ++ 
        (show start) ++ 
         " AND end >= " ++ (show end) ++ " AND path = ?")
        [toSql path]
      return (selfTags ++ (concat (map (tags . spanFromSql) spanRecs)))
      --return (spanRecs, path, toSql start, toSql end)
  
  resultingSpans <- mapM allTags (concat spans)

  mapM (putStrLn . show) resultingSpans
