-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
import Rlang.Parsing
import Rlang.Scan
import Rlang.Syntax
import Rlang.Emit
import Rlang.Codegen
import Rlang.Core
import Rlang.Jit

import qualified Data.Text.IO as T
import qualified Data.Text as T
import System.IO
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> T.getContents >>= run1
    xs -> mapM_ (\x -> run1 =<< T.readFile x) xs

run1 :: T.Text -> IO ()
run1 input = do
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
