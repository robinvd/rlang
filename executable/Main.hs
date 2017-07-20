-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
import Rlang.Parsing
import Rlang.Scan
import Rlang.Syntax
import Rlang.Codegen

import qualified Data.Text.IO as T
import System.IO

getBodys :: [TopLevel] -> [Expression]
getBodys [] = []
getBodys (Function _ _ _ b:xs) = b:getBodys xs
getBodys (_:xs) = getBodys xs

getFuncs :: [TopLevel] -> [TopLevel]
getFuncs [] = []
getFuncs (x@Function{} :xs) = x:getFuncs xs
getFuncs (_:xs) = getFuncs xs

getRetType (Function t _ _ _) = t

main :: IO ()
main = do
  input <- T.getContents
  let syntaxTree = parseTopLevel input
      -- print syntaxTree
  case syntaxTree of
    Left err -> print err
    Right x -> do
      hPrint stderr syntaxTree
      -- print $ fmap (runCheck (scanTop x)) (getBodys x)
      -- print $ fmap (checkTop (scanTop x)) (getFuncs x)
      -- print $ fmap (\a -> (== getRetType a) <$> checkTop (scanTop x) a) (getFuncs x)
      T.putStrLn $ codegenTop x
