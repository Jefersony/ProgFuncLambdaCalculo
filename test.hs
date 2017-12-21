import Control.Exception

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

dangerous :: IO Int
dangerous = error "Fool you!"

main :: IO ()
main = do
    result <- catchAny dangerous $ \e -> do
        putStrLn $ "Got an exception: " ++ show e
        putStrLn "Returning dummy value of -1"
        return (-1)
    print result
