module Main where

import EAFIT.De3Lang

main :: IO ()
main = do
  (Right cfg)  <- pCFGFile "/home/jfcmacro/tmp/cfg.grm"
  (Right drv)  <- pDrvFile "/home/jfcmacro/tmp/drv.dr"
  (Right tree) <- pTreeFile "/home/jfcmacro/tmp/tree.tr"
  putStrLn $ show cfg
  putStrLn $ show drv
  putStrLn $ show tree
