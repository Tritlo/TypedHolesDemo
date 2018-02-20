cat :: String -> IO String
cat = ("" <$) . (mapM_ putStrLn =<<) . fmap lines . readFile
