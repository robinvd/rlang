{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Rlang.Codegen where

import Data.List
import Data.Function
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Data.String
import qualified Data.Map as M
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.ByteString.Short as B

import Control.Monad.State
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Identity

import GHC.Stack

import LLVM.AST
import LLVM.AST.Global
import qualified LLVM.AST as AST

import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.Visibility as V
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.FunctionAttribute as F
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.IntegerPredicate as IP

import Rlang.Scan
import qualified Rlang.MapStack as MS

tShow :: Show a => a -> Text
tShow = T.pack . show

-------------------------------------------------------------------------------
-- Module Level
-------------------------------------------------------------------------------

newtype LLVM a = LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module )

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM modu (LLVM m) = execState m modu

emptyModule :: String -> AST.Module
emptyModule label = defaultModule { moduleName = fromString label }

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }

defineInline :: Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
defineInline retty label argtys body = addDefn $
  GlobalDefinition $ functionDefaults 
  { name        = mkName label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = body
  , LLVM.AST.Global.functionAttributes = [Right F.AlwaysInline ] --Left (F.GroupID 1)]
  -- , visibility = V.Hidden
  }

define :: Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = mkName label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = body
  }

external ::  Type -> String -> [(Type, Name)] -> LLVM ()
external retty label argtys = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = mkName label
  , linkage     = L.External
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = []
  }

---------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- IEEE 754 double
double :: Type
double = FloatingPointType DoubleFP

int :: Type
int = IntegerType 64

bool :: Type
bool = IntegerType 1

getTypefromOperand :: Operand -> Type
getTypefromOperand (LocalReference t _) = t
getTypefromOperand (ConstantOperand c) =
  case c of
    C.Int b _ -> IntegerType b
    C.Float _ -> FloatingPointType DoubleFP
    C.GlobalReference t _ -> t
    _ -> error "field not done yet"
getTypefromOperand (MetadataOperand _) = error "MetadataOperand not supported"

typeFromInstr :: HasCallStack =>  Instruction -> Type
typeFromInstr x =
  case x of
    Add _ _ op0 _ _ -> getTypefromOperand op0
    Sub _ _ op0 _ _ -> getTypefromOperand op0
    FAdd _ op0 _ _ -> getTypefromOperand op0
    FSub _ op0 _ _ -> getTypefromOperand op0
    Mul _ _ op0 _ _ -> getTypefromOperand op0
    FMul _ op0 _ _ -> getTypefromOperand op0
    Load _ addr _ _ _ -> unPtrType $ getTypefromOperand addr 
    Store _ _ _ _ _ _ -> VoidType
    ICmp _ op0 _ _ -> getTypefromOperand op0
    Phi t _ _ -> t
    Call _ _ _ f _ _ _ -> case f of
      Left _ -> error "typeFromInstr does not support inline asm"
      Right op -> getTypefromOperand op
    BitCast _ t _ -> t
    x -> error $ show x

    

unPtrType :: HasCallStack => Type -> Type
unPtrType (PointerType x _) = x
unPtrType x = error $ "not a ptr type: " ++ show x

-------------------------------------------------------------------------------
-- Names
-------------------------------------------------------------------------------

type Names = Map.Map Text Int

uniqueName :: Text -> Names -> (Text, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm,  Map.insert nm 1 ns)
    Just ix -> (nm <> tShow ix, Map.insert nm (ix+1) ns)

-------------------------------------------------------------------------------
-- Codegen State
-------------------------------------------------------------------------------

type SymbolTable = [(Text, Operand)]

data CodegenState
  = CodegenState 
  { currentBlock :: Name                     -- Name of the active block to append to
  , blocks       :: M.Map Name BlockState  -- Blocks for function
  -- , symtab       :: M.Map Text Operand
  -- , globalTable  :: M.Map Text Type
  , symtab       :: MS.MapStack Text (Codegen Operand)
  , globalName   :: Int
  , toGlobal     :: M.Map Text Global
  , blockCount   :: Int                      -- Count of basic blocksreturn $ 
  , count        :: Word                     -- Count of unnamed instructions
  , names        :: Names                    -- Name Supply
  } -- deriving Show

data BlockState
  = BlockState {
    idx   :: Int                            -- Block index
  , stack :: [Named Instruction]            -- Stack of instructions
  , term  :: Maybe (Named Terminator)       -- Block terminator
  } deriving Show

-------------------------------------------------------------------------------
-- Codegen Operations
-------------------------------------------------------------------------------

newtype CodegenT m a = CodegenT { runCodegen :: StateT CodegenState m a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState)

type Codegen = CodegenT Identity

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, (BlockState _ s t)) = BasicBlock l (reverse s) (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ (show l)

entryBlockName :: Text
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: CodegenState
emptyCodegen = CodegenState 
  (mkName (T.unpack entryBlockName))
  M.empty
  undefined
  0
  M.empty
  1
  0 
  M.empty

execCodegen :: M.Map Text (Codegen Operand) -> Codegen a -> CodegenState
execCodegen env m = execState (runCodegen m) emptyCodegen {symtab = MS.fromSingle env}

fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s { count = 1 + i }
  return $ i + 1

instr :: HasCallStack => Instruction -> Codegen Operand
instr ins = instrT (typeFromInstr ins) ins

instrT :: Type -> Instruction -> Codegen Operand
instrT t ins = do
  n <- fresh
  let ref = (UnName n)
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = (ref := ins) : i } )
  return $ LocalReference t ref


terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk { term = Just trm })
  return trm

-------------------------------------------------------------------------------
-- Block Stack
-------------------------------------------------------------------------------

entry :: Codegen Name
entry = gets currentBlock

addBlock :: Text -> Codegen Name
addBlock bname = do
  bls <- gets blocks
  ix  <- gets blockCount
  nms <- gets names

  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms

  modify $ \s -> s { blocks = Map.insert (mkName (T.unpack qname)) new bls
                   , blockCount = ix + 1
                   , names = supply
                   }
  return (mkName (T.unpack qname))

setBlock :: Name -> Codegen Name
setBlock bname = do
  modify $ \s -> s { currentBlock = bname }
  return bname

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> error $ "No such block: " ++ show c

-------------------------------------------------------------------------------
-- Symbol Table
-------------------------------------------------------------------------------

assign :: Text -> Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s { symtab = MS.insert var (return x) lcls }

getvar :: Text -> Codegen Operand
getvar var = do
  syms <- gets symtab
  case MS.lookup var syms of
    Just x  -> x
    Nothing -> error $ "variable not in scope: " ++ show var
    -- Nothing -> case M.lookup var global of
                 -- Just x -> do
                   -- al <- alloca x
                   -- store al (externf (AST.mkName (T.unpack var)))
                   -- return al
                 -- _ -> error $ "Local variable not in scope: " ++ show var

-------------------------------------------------------------------------------

-- References
localInt ::  Name -> Operand
localInt = LocalReference int

-- global ::  Name -> C.Constant
-- global = C.GlobalReference double

-- externf :: Name -> Operand
-- externf = ConstantOperand . C.GlobalReference double

-- Arithmetic and Constants
fadd :: Operand -> Operand -> Codegen Operand
fadd a b = instrT double $ FAdd NoFastMathFlags a b []

fsub :: Operand -> Operand -> Codegen Operand
fsub a b = instrT double $ FSub NoFastMathFlags a b []

fmul :: Operand -> Operand -> Codegen Operand
fmul a b = instrT double $ FMul NoFastMathFlags a b []

fdiv :: Operand -> Operand -> Codegen Operand
fdiv a b = instrT double $ FDiv NoFastMathFlags a b []

fcmp :: FP.FloatingPointPredicate -> Operand -> Operand -> Codegen Operand
fcmp cond a b = instrT double  $ FCmp cond a b []

icmp :: IP.IntegerPredicate -> Operand -> Operand -> Codegen Operand
icmp cond a b = instrT double $ ICmp cond a b []

cons :: C.Constant -> Operand
cons = ConstantOperand

uitofp :: Type -> Operand -> Codegen Operand
uitofp ty a = instrT double $ UIToFP a ty []

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

-- Effects
call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []

alloca :: Type -> Codegen Operand
alloca ty = instrT ty $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen Operand
store ptr val = instr $ Store False ptr val Nothing 0 []

load :: HasCallStack => Operand -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []

-- Control Flow
br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

phi :: Type -> [(Operand, Name)] -> Codegen Operand
phi ty incoming = instr $ Phi ty incoming []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []
