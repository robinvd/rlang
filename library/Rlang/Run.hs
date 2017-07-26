module Rlang.Run where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO

import Rlang.Parsing
import Rlang.Scan
import Rlang.Syntax
import Rlang.Emit
import Rlang.Codegen
import Rlang.Core
import Rlang.Jit

run1 :: String -> IO ()
run1 inp = do
  cont <- readFile inp
  Rlang.Run.run $ T.pack cont

run :: T.Text -> IO ()
run input = do
  let syntaxTree = parseTopLevel input
  case syntaxTree of
    Left err -> print err
    Right x -> do
      hPrint stderr x
      -- print $ fmap (runCheck (scanTop x)) (getBodys x)
      -- print $ fmap (checkTop (scanTop x)) (getFuncs x)
      -- print $ fmap (\a -> (== getRetType a) <$> checkTop (scanTop x) a) (getFuncs x)
      hPrint stderr $ toCore x
      a <- codegen (emptyModule "test") (toCore x)
      runJIT a
      return ()
