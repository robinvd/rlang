module Rlang.MapStack where

import qualified Data.Map as M
import Control.Applicative

data MapStack k v = MapStack [M.Map k v] deriving (Show)

insert :: Ord k => k -> v -> MapStack k v -> MapStack k v
insert k v (MapStack (x:xs)) = MapStack $ M.insert k v x:xs

lookup :: Ord k => k -> MapStack k v -> Maybe v
lookup k = foldr (<|>) Nothing . map (M.lookup k) . unMapStack

unMapStack :: MapStack k v -> [M.Map k v]
unMapStack (MapStack x) = x

push :: MapStack k v -> MapStack k v
push (MapStack x) = MapStack (M.empty:x)

pop :: MapStack k v -> MapStack k v
pop (MapStack []) = error "mapStack empty"
pop (MapStack [_]) = error "cant pop global value"
pop (MapStack (_:xs)) = MapStack xs

fromSingle :: M.Map k v -> MapStack k v
fromSingle x = MapStack [x]
