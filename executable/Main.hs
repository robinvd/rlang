-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
import qualified Example
import Rlang.Parsing

import qualified Data.Text.IO as T

main :: IO ()
main = T.getContents >>= print . parse
