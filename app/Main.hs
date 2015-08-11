module Main where

import PscInspect

main :: IO ()
main = do
    importableModules <- PscInspect.getImportableModules
    mapM_ putStrLn importableModules
