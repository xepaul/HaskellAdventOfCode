{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Aoc2020.Day8 where

import Control.Lens (makeLenses, (%~), (&), (+~), (^.))
import Control.Monad.RWS (modify)
import Control.Monad.State (MonadState (put), evalState, gets)
import Data.Either.Extra (mapLeft)
import Data.Functor ((<&>))
import qualified Data.Set as Set
import Text.Parsec.Prim (parse, (<|>))
import Text.ParserCombinators.Parsec
  ( Parser,
    char,
    sepEndBy,
    string,
  )
import Text.ParserCombinators.Parsec.Number (int)

data Instruction
  = Nop Int
  | Acc RegisterValue
  | Jmp Address
  deriving (Show, Eq)

parseInstructions :: Parser [Instruction]
parseInstructions = sepEndBy (nopInst <|> accInst <|> jmpInst) (char '\n')
  where
    nopInst :: Parser Instruction
    nopInst = do
      _ <- string "nop" <* char ' '
      Nop <$> int

    accInst :: Parser Instruction
    accInst = do
      _ <- string "acc" <* char ' '
      Acc . toRegisterValue <$> int

    jmpInst :: Parser Instruction
    jmpInst = do
      _ <- string "jmp" <* char ' '
      Jmp . toAddress <$> int

type Program = [Instruction]

newtype Address = Address Int deriving (Show, Ord, Eq, Num)

newtype RegisterValue = RegisterValue Int
  deriving stock (Show, Eq)
  deriving (Num)

data CpuState = CpuState {_pc :: Address, _acc :: RegisterValue}

data ExecutingState = ExecutingState
  { _cpuState :: CpuState,
    _addressHistory :: Set.Set Address
  }

fromAddress :: Address -> Int
fromAddress (Address x) = fromIntegral x

toAddress :: Int -> Address
toAddress = Address . fromIntegral

toRegisterValue :: Int -> RegisterValue
toRegisterValue = RegisterValue . fromIntegral

makeLenses ''CpuState
makeLenses ''ExecutingState

part1 :: [Instruction] -> RegisterValue
part1 program =
  evalState runCpuUntilRepeat ExecutingState {_addressHistory = Set.empty, _cpuState = CpuState {_pc = 0, _acc = 0}}
  where
    runCpuUntilRepeat :: MonadState ExecutingState m => m RegisterValue
    runCpuUntilRepeat = do
      found <- gets (\s -> Set.member (s ^. (cpuState . pc)) (s ^. addressHistory))
      if found
        then gets (\s -> s ^. (cpuState . acc))
        else do
          modify
            ( \s ->
                s & addressHistory %~ Set.insert (s ^. (cpuState . pc))
                  & cpuState %~ executeInstruction (program !! fromAddress (s ^. (cpuState . pc)))
            )
          runCpuUntilRepeat
      where
        executeInstruction :: Instruction -> CpuState -> CpuState
        executeInstruction a s = case a of
          Nop _ -> s & pc +~ 1
          Acc v ->
            s & acc +~ v
              & pc +~ 1
          Jmp r -> s & pc +~ r

parseLines :: String -> Either String [Instruction]
parseLines = mapLeft show . parse inputParser ""
  where
    inputParser = parseInstructions

day8Part1FromInput :: String -> Either String RegisterValue
day8Part1FromInput l = parseLines l <&> part1