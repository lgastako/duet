{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
-- TODO: remove unused pragmas
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Duet.Interpreter
    ( interpret
    ) where

-- TODO: get rid of "hiding log"

import qualified Data.Map.Strict  as Map
import qualified Data.Text        as Text
import           Duet.Instruction                ( Instruction( Add
                                                              , Jgz
                                                              , Mod
                                                              , Mul
                                                              , Rcv
                                                              , Set
                                                              , Snd
                                                              )
                                                 , parse
                                                 )
import           Duet.Prelude             hiding ( log )
import           Duet.Reference                  ( Reference( RegisterRef
                                                            , ValueRef
                                                            )
                                                 )
import           Duet.RegisterId                 ( RegisterId )
import           Duet.Value                      ( Value( Value ) )
import           System.IO                       ( hFlush
                                                 , stdout
                                                 )

data InterpreterState = InterpreterState
  { lastSound          :: Maybe Value
  , instructions       :: [Instruction]
  , instructionPointer :: Int
  , registers          :: Map RegisterId Value
  , terminated         :: Bool
  } deriving (Eq, Ord, Read, Show)

-- interpret :: Text -> Text
-- interpret s = run instructions'
--   where
--     run = execute
--     -- run           = show . execute
--     -- TODO: catMaybes is questionable here... really should assert that they all parse?
--     instructions' = catMaybes . map parse . Text.lines $ s

-- TODO: replace me with the regular (show . execute) version above
--execute :: [Instruction] -> (Maybe Value, InterpreterState)

interpret :: Text -> IO Text
interpret s = execute instructions'
  where
    -- run           = show . execute
    -- TODO: catMaybes is questionable here... really should assert that they all parse?
    instructions' = catMaybes . map parse . Text.lines $ s

execute :: [Instruction] -> IO Text
execute ins = do
  (val, _) <- execute' ins
  return $ "\n\n\nFinal value: " <> show val <> "\n\n"

execute' :: MonadIO m => [Instruction] -> m (Maybe Value, InterpreterState)
execute' ins = runStateT executeAll $ (fromInstructions ins)

-- execute :: [Instruction] -> Maybe Value
-- execute instructions' = fst . runStateT executeAll $ (fromInstructions instructions')

fromInstructions :: [Instruction] -> InterpreterState
fromInstructions instructions' = InterpreterState
  { lastSound          = Nothing
  , instructions       = instructions'
  , instructionPointer = 0
  , registers          = Map.empty
  , terminated         = False
  }

executeAll :: MonadIO m => StateT InterpreterState m (Maybe Value)
executeAll = do
  void . runExceptT . forever $ do
    lift $ log ""
    lift $ log "================================================================"
    lift $ log ""

    get >>= lift . log . ("BEFORE STATE: " <>) . show

    lift executeOne

    lift $ log ""
    get >>= lift . log . ("AFTER STATE: " <>) . show

    s <- get
    when (terminated s) $ do
      lift $ log "STOPPING"
      stop ()

  log "BEFORE lastsound"
  lastSound <$> get

executeOne :: MonadIO m => StateT InterpreterState m ()
executeOne = do
  s <- get
  case atMay (instructions s) (instructionPointer s) of
    Nothing -> panic "THE FRONT FELL OFF" -- TODO normal termination
    Just instruction -> do
      log . ("EXECUTING INS:  " <>) . show $ instruction
      step instruction
      log "Instruction EXECUTED, updating IP."
      case instruction of
        Jgz x' _ -> getReferenceValue x' >>= \x -> if x > 0 then return () else jmp 1
        _        -> jmp 1

      -- TODO: get rid of this:
      -- modify (stashRegisters instruction)
      st' <- get
      log . ("IP AFTER EXECUTION:   " <>) . show $ instructionPointer st'
      log . ("UPDATED REGISTERS: " <>) . show $ registers st'

step :: MonadIO m => Instruction -> StateT InterpreterState m ()

step (Add reg ref) = do
  x <- getRegisterValue reg
  y <- getReferenceValue ref
  setRegisterValue reg (x + y)

step (Jgz ref0 ref1) = do
  x <- getReferenceValue ref0
  y <- getReferenceValue ref1
  log $ "jgz: " <> show (x, y, x > 0)
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

jmp :: MonadIO m => Value -> StateT InterpreterState m ()
jmp (Value offset) = do
  -- TODO
  s' <- get
  log $ "jmp: " <> show offset
  log $ "instructionPointer pre-jmp: " <> (show $ instructionPointer s')

  modify updatePointer
  s <- get
  log $ "updated pointer: " <> (show $ instructionPointer s) -- TODO
  when (outOfBounds s) $ do
    modify terminate
    log "should be also TERMINATED"
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

setRegisterValue :: MonadIO m => RegisterId -> Value -> StateT InterpreterState m ()
setRegisterValue regId val = modify $ \st ->
  st { registers = Map.insert regId val (registers st) }

getRegisterValue :: MonadIO m => RegisterId -> StateT InterpreterState m Value
getRegisterValue regId = get >>= \st -> return $ Map.findWithDefault 0 regId (registers st)

getReferenceValue :: MonadIO m => Reference -> StateT InterpreterState m Value
getReferenceValue (RegisterRef r) = getRegisterValue r
getReferenceValue (ValueRef v)    = return v


-- ================================================================
-- TODO: get rid of this:
-- ================================================================

-- stashRegisters :: Instruction -> InterpreterState -> InterpreterState
-- stashRegisters instruction st = st { traces = traces' }
--   where
--     traces' = show (instruction, ip, registers st) <> "\n" <> traces st
--     ip = instructionPointer st

-- logRegisters :: MonadIO m  => Instruction -> StateT InterpreterState m ()
-- logRegisters ins = do
--   st <- get
--   log ""
--   log . ("INS:  " <>) . show $ ins
--   log . ("IP:   " <>) . show $ instructionPointer st
--   log . ("REGS: " <>) . show $ registers st

-- renderTrace :: (Instruction, Int, Map RegisterId Value) -> Text
-- renderTrace (ins', ip, regs) = ""
--   <> " INS: "  <> show ins'
--   <> " IP: "   <> show ip
--   <> " REGS: " <> show regs

log :: MonadIO m => Text -> StateT InterpreterState m ()
log s = liftIO $ putLn s >> hFlush stdout
