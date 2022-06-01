module Main where

import           Control.Monad                  ( forever )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           System.Environment             ( getArgs )
import           System.IO                      ( hFlush
                                                , stdout
                                                )
import qualified Text.Megaparsec               as P

import           Text.Pretty.Simple

import           Language.SomnaScript.Eval
import           Language.SomnaScript.Parser

main :: IO ()
main = do
  (filename : _) <- getArgs
  res            <- P.parse parseProgram filename <$> T.readFile filename
  case res of
    Left  err    -> putStrLn $ P.errorBundlePretty err
    Right parsed -> pPrint parsed
