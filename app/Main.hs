module Main where

import           Control.Monad                  ( forever )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Text.Megaparsec               as P
import System.IO (hFlush, stdout)

import           Language.SomnaScript.Eval
import           Language.SomnaScript.Parser

main :: IO ()
main = forever $ do
  putStr "> "
  hFlush stdout
  tx <- T.getLine
  P.parseTest (eval initialEnv <$> expr) tx
