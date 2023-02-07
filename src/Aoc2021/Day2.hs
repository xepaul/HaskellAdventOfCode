{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wunused-imports #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Aoc2021.Day2 where

-- import Data.Text

-- import Data.Text
import Control.Lens (makeLenses, use, view, (&), (+=), (+~), (-=), (-~), (.=), (^.))
import Control.Monad.Except (MonadError)
import Control.Monad.RWS (modify)
-- import Control.Monad.Trans.Reader ( ReaderT(runReaderT),asks,ask)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Control.Monad.State (MonadState (), evalState, gets, runState)
import Control.Monad.Trans.Except qualified as Except
import Control.Monad.Trans.State.Lazy qualified as StateLazy
-- import Control.Monad.Trans.Reader qualified as R ( asks,ask)
import Data.Coerce (coerce)
import Data.Either.Extra (mapLeft)
import GHC.Generics (Generic)
import Text.Parsec.Prim (parse, (<|>))
import Text.ParserCombinators.Parsec
  ( Parser,
    char,
    sepEndBy,
    string,
  )
import Text.ParserCombinators.Parsec.Number (int)

class SingleAnswer a where
  singleAnswer :: a -> Int

data Instruction
  = Forward HozPosition
  | Down Depth
  | Up Depth
  deriving (Show, Eq, Generic)

parseInstructions :: String -> Either String [Instruction]
parseInstructions s = mapLeft show $ parse p "" s
  where
    p = sepEndBy (forwardCommand <|> downInst <|> upInst) (char '\n')
    forwardCommand :: Parser Instruction
    forwardCommand = do
      _ <- string "forward" <* char ' '
      Forward . toHozPosition <$> int  

    downInst :: Parser Instruction
    downInst = do
      _ <- string "down" <* char ' '
      Down . toDepth <$> int

    upInst :: Parser Instruction
    upInst = do
      _ <- string "up" <* char ' '
      Up . toDepth <$> int

newtype Depth = Depth Int deriving (Show, Ord, Eq, Num, Generic)

newtype HozPosition = HozPosition Int
  deriving stock (Show, Eq, Generic)
  deriving (Num)

data DiveState = DiveState {_hozPosition :: !HozPosition, _depth :: !Depth}

toDepth :: Int -> Depth
toDepth = Depth . fromIntegral

toHozPosition :: Int -> HozPosition
toHozPosition = HozPosition . fromIntegral

makeLenses ''DiveState

instance SingleAnswer DiveState where
  singleAnswer a =
    let h = coerce (a ^. hozPosition)
        d = coerce (a ^. depth)
     in h * d

processInstructions :: [Instruction] -> Int
processInstructions p =
  evalState (xx p) DiveState {_hozPosition = 0, _depth = 0}
  where
    xx :: MonadState DiveState m => [Instruction] -> m Int
    xx i = case i of
      [] -> gets singleAnswer
      x : xs -> do
        modify
          ( \s -> case x of
              Up v -> s & depth -~ v
              Down v -> s & depth +~ v
              Forward v -> s & hozPosition +~ v
          )
        xx xs

newtype AimPosition = AimPosition Int
  deriving stock (Show, Eq, Generic)
  deriving (Num)

data AimInstruction
  = AimForward Int
  | AimDown AimPosition
  | AimUp AimPosition
  deriving (Show)

aimParseInstructions :: String -> Either String [AimInstruction]
aimParseInstructions s = mapLeft show $ parse p "" s
  where
    p = sepEndBy (aimForwardCommand <|> aimDownInst <|> aimUpInst) (char '\n')
    aimForwardCommand :: Parser AimInstruction
    aimForwardCommand = do
      _ <- string "forward" <* char ' '
      AimForward <$> int

    aimDownInst :: Parser AimInstruction
    aimDownInst = do
      _ <- string "down" <* char ' '
      AimDown . toAim <$> int

    aimUpInst :: Parser AimInstruction
    aimUpInst = do
      _ <- string "up" <* char ' '
      AimUp . toAim <$> int
    toAim :: Int -> AimPosition
    toAim = AimPosition . fromIntegral

data AimDiveState = AimDiveState {_aimHozPosition :: !HozPosition, _aimDepth :: !Depth, _aimAim :: !AimPosition}
  deriving (Generic, Show, Eq)

makeLenses ''AimDiveState

instance Semigroup AimDiveState where
  (<>) a b =
    AimDiveState
      { _aimHozPosition = (a ^. aimHozPosition) + (b ^. aimHozPosition),
        _aimDepth = a ^. aimDepth + b ^. aimDepth,
        _aimAim = a ^. aimAim + b ^. aimAim
      }

instance Monoid AimDiveState where
  mempty = AimDiveState {_aimHozPosition = 0, _aimDepth = 0, _aimAim = 0}

instance SingleAnswer AimDiveState where
  singleAnswer a =
    let h = coerce (a ^. aimHozPosition)
        d = coerce (a ^. aimDepth)
     in h * d

aimProcessInstructions :: [AimInstruction] -> Int
aimProcessInstructions p =
  evalState (aimProcessInstructionsRec p) AimDiveState {_aimHozPosition = 0, _aimDepth = 0, _aimAim = 0}
  where
    aimProcessInstructionsRec :: MonadState AimDiveState m => [AimInstruction] -> m Int
    aimProcessInstructionsRec i = case i of
      [] -> gets singleAnswer
      v : vs -> do
        aimDepth .= Depth 0

        modify
          ( \s ->
              case v of
                AimUp x -> s & aimAim -~ x
                AimDown x -> s & aimAim +~ x
                AimForward x ->
                  s & aimHozPosition +~ toHozPosition x
                    & aimDepth +~ toDepth (coerce (s ^. aimAim) * x)
          )
        aimProcessInstructionsRec vs

aimInitialState :: AimDiveState
aimInitialState = AimDiveState {_aimHozPosition = 0, _aimDepth = 0, _aimAim = 0}

aimProcessInstructionsM3 :: [AimInstruction] -> Int
aimProcessInstructionsM3 p =
  let (_, s) = runState (aimProcessInstructionsRec p) aimInitialState
   in singleAnswer s
  where
    aimProcessInstructionsRec :: (MonadState AimDiveState m) => [AimInstruction] -> m ()
    aimProcessInstructionsRec =
      mapM_
        ( \case
            AimUp x -> aimAim -= x
            AimDown x -> aimAim += x
            AimForward x -> do
              currentAimAim <- use aimAim
              aimHozPosition += toHozPosition x
              aimDepth += toDepth (coerce currentAimAim * x)
        )

newtype AimParameters = AimParameters {_speed :: Int}

makeLenses ''AimParameters

initialAimParameters :: AimParameters
initialAimParameters = AimParameters {_speed = 1}

aimProcessInstructionsM4 :: [AimInstruction] -> Either Int Int
aimProcessInstructionsM4 xs =
  singleAnswer <$> Except.runExcept (StateLazy.execStateT processCommands aimInitialState)
  where
    processCommands = runReaderT (traverse processCommand xs) initialAimParameters
    processCommand :: (MonadReader AimParameters m, MonadState AimDiveState m, MonadError Int m) => AimInstruction -> m ()
    processCommand =
      \case
        AimUp x ->
          do
            currentSpeed <- view speed
            aimAim -= (x * coerce currentSpeed)
        AimDown x -> aimAim += x
        AimForward x -> do
          currentAimAim <- use aimAim
          aimHozPosition += toHozPosition x
          aimDepth += toDepth (coerce currentAimAim * x)

aimProcessInstructionsM5 :: [AimInstruction] -> Int
aimProcessInstructionsM5 =
  singleAnswer . flip StateLazy.execState aimInitialState . traverse processCommand
  where
    processCommand :: (MonadState AimDiveState m) => AimInstruction -> m ()
    processCommand =
      \case
        AimUp x -> aimAim -= x
        AimDown x -> aimAim += x
        AimForward x -> do
          currentAimAim <- use aimAim
          aimHozPosition += toHozPosition x
          aimDepth += toDepth (coerce currentAimAim * x)

aimProcessInstructions1 :: [AimInstruction] -> Int
aimProcessInstructions1 =
  singleAnswer
    . foldl
      ( \s -> \case
          AimUp x -> s & aimAim -~ x
          AimDown x -> s & aimAim +~ x
          AimForward x ->
            s & aimHozPosition +~ toHozPosition x
              & aimDepth +~ toDepth (coerce (s ^. aimAim) * x)
      )
      AimDiveState {_aimHozPosition = 0, _aimDepth = 0, _aimAim = 0}

aimProcessInstructions2 :: [AimInstruction] -> Int
aimProcessInstructions2 =
  singleAnswer
    . foldl
      ( \s -> \case
          AimUp x -> s & aimAim -~ x
          AimDown x -> s & aimAim +~ x
          AimForward x ->
            s & aimHozPosition +~ toHozPosition x
              & aimDepth +~ toDepth (coerce (s ^. aimAim) * x)
      )
      mempty

processInputPart1 :: String -> Either String Int
processInputPart1 content = processInstructions <$> parseInstructions content

processInputPart2 :: String -> Either String Int
processInputPart2 content = aimProcessInstructionsM3 <$> aimParseInstructions content
