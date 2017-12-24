{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
-- TODO: remove unused pragmas
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Duet.Interpreter
    ( interpret
    , interpret1
    ) where

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

interpret1 :: Text -> Text
interpret1 s = run instructions'
  where
    run           = show . execute1
    -- TODO: catMaybes is questionable here... really should assert that they all parse?
    instructions' = catMaybes . map parse . Text.lines $ s

interpret :: Text -> IO Text
interpret s = execute instructions'
  where
    -- run           = show . execute
    -- TODO: catMaybes is questionable here... really should assert that they all parse?
    instructions' = catMaybes . map parse . Text.lines $ s

execute :: [Instruction] -> IO Text
execute ins = do
  (val, _) <- execute' ins
  return . show $ val

execute' :: Monad m => [Instruction] -> m (Maybe Value, InterpreterState)
execute' ins = runStateT executeAll $ (fromInstructions ins)

execute1 :: [Instruction] -> Maybe Value
execute1 instructions' = fst . runStateT executeAll $ (fromInstructions instructions')

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
    when (terminated s) $ do
      stop ()

  lastSound <$> get

executeOne :: Monad m => StateT InterpreterState m ()
executeOne = do
  s <- get
  case atMay (instructions s) (instructionPointer s) of
    Nothing -> panic "THE FRONT FELL OFF" -- TODO normal termination
    Just instruction -> do
      step instruction
      case instruction of
        Jgz x' _ -> getReferenceValue x' >>= \x -> if x > 0 then return () else jmp 1
        _        -> jmp 1


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
  setRegisterValue reg (x `mod` y)

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

step (Snd ref) = do
  val <- getReferenceValue ref
  modify (\st -> st { lastSound = Just val })

jmp :: Monad m => Value -> StateT InterpreterState m ()
jmp (Value offset) = do
  modify updatePointer
  s <- get
  when (outOfBounds s) $ do
    modify terminate
  where
    updatePointer :: InterpreterState -> InterpreterState
    updatePointer st = st { instructionPointer = instructionPointer st + offset }

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
