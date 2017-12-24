{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Duet.Interpreter
    ( interpret
    ) where

import           Data.List                ( partition )
import qualified Data.Map.Strict  as Map
import qualified Data.Text        as Text
import           Duet.Instruction         ( Instruction( Add
                                                       , Jgz
                                                       , Mod
                                                       , Mul
                                                       , Rcv
                                                       , Set
                                                       , Snd
                                                       )
                                          , parse
                                          )
import           Duet.Prelude
import           Duet.Reference           ( Reference( RegisterRef
                                                     , ValueRef
                                                     )
                                          )
import           Duet.RegisterId          ( RegisterId )
import           Duet.Value               ( Value( Value ) )

data InterpreterState = InterpreterState
  { lastSound          :: Maybe Value
  , instructions       :: [Instruction]
  , instructionPointer :: Int
  , registers          :: Map RegisterId Value
  , terminated         :: Bool
  } deriving (Eq, Ord, Read, Show)

interpret :: Monad m => Text -> m Text
interpret s = case failures of
  [] -> execute . catMaybes $ instructions'
  _  -> panic "Failed to parse some instructions."
  where
    (failures, instructions')  = partition null . map parse . Text.lines $ s

execute :: Monad m => [Instruction] -> m Text
execute ins = do
  (val, _) <- execute' ins
  return . show $ val

execute' :: Monad m => [Instruction] -> m (Maybe Value, InterpreterState)
execute' ins = runStateT executeAll $ (fromInstructions ins)

fromInstructions :: [Instruction] -> InterpreterState
fromInstructions instructions' = InterpreterState
  { lastSound          = Nothing
  , instructions       = instructions'
  , instructionPointer = 0
  , registers          = Map.empty
  , terminated         = False
  }

executeAll :: Monad m => StateT InterpreterState m (Maybe Value)
executeAll = do
  void . runExceptT . forever $ do
    lift executeOne

    done <- terminated <$> get
    when done $ stop ()

  lastSound <$> get

executeOne :: Monad m => StateT InterpreterState m ()
executeOne = get >>= \s ->
  case atMay (instructions s) (instructionPointer s) of
    Nothing -> panic "ERROR: Invalid instruction pointer dereferenced. Execution aborted."
    Just instruction -> do
      step instruction
      case instruction of
        Jgz x' _ -> referenceValue x' >>= \x -> if x > 0 then return () else jmp 1
        _        -> jmp 1

step :: Monad m => Instruction -> StateT InterpreterState m ()

step (Add reg ref) = do
  x <- registerValue reg
  y <- referenceValue ref
  setRegister reg (x + y)

step (Jgz ref0 ref1) = do
  x <- referenceValue ref0
  y <- referenceValue ref1
  if x > 0
    then jmp y
    else return ()

step (Mod reg ref) = do
  x <- registerValue reg
  y <- referenceValue ref
  setRegister reg (x `mod` y)

step (Mul reg ref) = do
  x <- registerValue reg
  y <- referenceValue ref
  setRegister reg (x * y)

step (Rcv ref)     = referenceValue ref >>= \x -> when (x /= 0) $ modify terminate
step (Set reg ref) = referenceValue ref >>= setRegister reg
step (Snd ref)     = referenceValue ref >>= \x -> modify (\st -> st { lastSound = Just x })

jmp :: Monad m => Value -> StateT InterpreterState m ()
jmp (Value offset) = do
  modify updatePointer
  oob <- not . inBounds <$> get
  when oob $ modify terminate
  where
    updatePointer :: InterpreterState -> InterpreterState
    updatePointer st = st { instructionPointer = instructionPointer st + offset }

    inBounds :: InterpreterState -> Bool
    inBounds st
      | instructionPointer st < 0                         = False
      | instructionPointer st >= length (instructions st) = False
      | otherwise                                         = True

terminate :: InterpreterState -> InterpreterState
terminate st = st { terminated = True }

stop :: MonadError e m => e -> m a
stop = throwError

setRegister :: Monad m => RegisterId -> Value -> StateT InterpreterState m ()
setRegister regId val = modify $ \st ->
  st { registers = Map.insert regId val (registers st) }

registerValue :: Monad m => RegisterId -> StateT InterpreterState m Value
registerValue regId = get >>= \st -> return $ Map.findWithDefault 0 regId (registers st)

referenceValue :: Monad m => Reference -> StateT InterpreterState m Value
referenceValue (RegisterRef r) = registerValue r
referenceValue (ValueRef v)    = return v
