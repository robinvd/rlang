import Rlang.Run

import qualified Data.Text.IO as T
import qualified Data.Text as T
import System.IO
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> run "/dev/stdin"
    x:_ -> run x
    -- xs -> mapM_ (\x -> run =<< T.readFile x) xs


