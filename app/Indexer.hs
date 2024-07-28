module Main (main) where

import Index (indexFolder)
import IndexState (numberOfTokens, sortedTokensFreq, sortedTokensFreqPerDoc)
import System.Environment (getArgs)

prettyPrintCommonTokens :: [(String, Int)] -> IO ()
prettyPrintCommonTokens [] = return ()
prettyPrintCommonTokens ((t,p):tps) = do
    putStrLn $ "\t" ++ t ++ " -> " ++ show p
    prettyPrintCommonTokens tps

prettyPrintCommonTokensPerDoc :: [(Int, [(String, Int)])] -> IO ()
prettyPrintCommonTokensPerDoc [] = return ()
prettyPrintCommonTokensPerDoc ((docId, tps):xs) = do
    putStrLn $ "Doc ID: " ++ show docId
    prettyPrintCommonTokens $ take 5 tps
    prettyPrintCommonTokensPerDoc xs

main :: IO ()
main = do
    args <- getArgs
    if length args /= 2 then do
        putStrLn "Wrong arguments: <directory to be index> <serialized file>"
    else do
        let directoryToIndex = head args
        let indexingStateBinary = args !! 1

        indexState <- indexFolder directoryToIndex indexingStateBinary
        putStrLn $ "Number unique of tokens: " ++ show (numberOfTokens indexState)
        putStrLn "Common tokens (token -> #frequency):"
        prettyPrintCommonTokens (take 20 $ sortedTokensFreq indexState)
        prettyPrintCommonTokensPerDoc (sortedTokensFreqPerDoc indexState)
        return ()

        -- is <- indexFolder directoryToIndex indexingStateBinary
        -- case is of
        --     Just indexState -> do
        --         putStrLn $ "Number unique of tokens: " ++ show (numberOfTokens indexState)
        --         putStrLn "Common tokens (token -> #frequency):"
        --         prettyPrintCommonTokens (take 20 $ sortedTokensFreq indexState)
        --         prettyPrintCommonTokensPerDoc (sortedTokensFreqPerDoc indexState)
        --         return ()
        --     Nothing -> do 
        --         putStrLn $ "Directory doesn't exists: " ++ directoryToIndex
        --         return ()
