module Hello (sayHello) where

sayHello :: String -> IO ()
sayHello name = do
  putStrLn $ "Hi " ++ name ++ "!"
