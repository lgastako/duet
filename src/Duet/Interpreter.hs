{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Duet.Interpreter
    ( interpret
    ) where

import qualified Data.Map.Strict  as Map
import qualified Data.Text        as Text
import           Duet.Instruction         ( Instruction( Add
                                                       , Jgz
                                                       , Mod
                                                       , Mul
                                                       , Rcv
                                                       , Set
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

interpret :: Text -> Text
interpret s = run instructions'
  where
    run           = show . execute
    instructions' = catMaybes . map parse . Text.lines $ s
    -- TODO: catMaybes is questionable here... really should assert that they all parse?

execute :: [Instruction] -> Maybe Value
execute instructions' = fst . runStateT executeAll $ (fromInstructions instructions')

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
    s <- get
    when (terminated s) $ stop ()
  lastSound <$> get

executeOne :: Monad m => StateT InterpreterState m ()
executeOne = do
  s <- get
  case atMay (instructions s) (instructionPointer s) of
    Nothing -> panic "THE FRONT FELL OFF" -- TODO normal termination
    Just instruction -> do
      step instruction
      case instruction of
        Jgz _ _ -> return ()
        _       -> bmp

bmp :: Monad m => StateT InterpreterState m ()
bmp = (+1) . instructionPointer <$> get >>= jmp . Value

step :: Monad m => Instruction -> StateT InterpreterState m ()
step (Add reg ref) = do
  x <- getRegisterValue reg
  y <- getReferenceValue ref
  setRegisterValue reg (x + y)

step (Jgz ref0 ref1) = do
  x <- getReferenceValue ref0
  y <- getReferenceValue ref1
  if x > 0
    then jmp y
    else return ()

step (Mod reg ref) = do
  x <- getRegisterValue reg
  y <- getReferenceValue ref
  setRegisterValue reg (x * y)

step (Mul reg ref) = do
  x <- getRegisterValue reg
  y <- getReferenceValue ref
  setRegisterValue reg (x * y)

step (Rcv ref) = do
  x <- getReferenceValue ref
  when (x /= 0) $ modify terminate

step (Set reg ref) = do
  val <- getReferenceValue ref
  setRegisterValue reg val

jmp :: Monad m => Value -> StateT InterpreterState m ()
jmp offset = do
  modify updatePointer
  s <- get
  when (outOfBounds s) $ modify terminate
  return ()
  where
    updatePointer :: InterpreterState -> InterpreterState
    updatePointer st = st { instructionPointer = (instructionPointer st) + o }
      where
        Value o = offset

    outOfBounds :: InterpreterState -> Bool
    outOfBounds st
      | instructionPointer st < 0                         = True
      | instructionPointer st >= length (instructions st) = True
      | otherwise                                         = False

terminate :: InterpreterState -> InterpreterState
terminate st = st { terminated = True }

stop :: MonadError e m => e -> m a
stop = throwError

setRegisterValue :: Monad m => RegisterId -> Value -> StateT InterpreterState m ()
setRegisterValue regId val = modify $ \st ->
  st { registers = Map.insert regId val (registers st) }

getRegisterValue :: Monad m => RegisterId -> StateT InterpreterState m Value
getRegisterValue regId = get >>= \st -> return $ Map.findWithDefault 0 regId (registers st)

getReferenceValue :: Monad m => Reference -> StateT InterpreterState m Value
getReferenceValue (RegisterRef r) = getRegisterValue r
getReferenceValue (ValueRef v)    = return v
