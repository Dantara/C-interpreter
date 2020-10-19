{-# LANGUAGE FlexibleInstances #-}

module Language.Interpreter.Cast where

import           Language.Interpreter.Types
import           Language.Syntax.AST
import           Text.Read                  (readMaybe)


newtype Caster a = Caster {
  unCaster :: a
}


instance Castable (Caster Value) where
  castToInt (Caster (IntValue x))   = Caster $ IntValue x
  castToInt (Caster (FloatValue x)) = Caster $ IntValue $ round x
  castToInt (Caster (StringValue s)) = Caster $ IntValue $ mbToInt $ readMaybe s
    where
      mbToInt Nothing  = length s
      mbToInt (Just y) = y
  castToInt (Caster (BoolValue True)) = Caster $ IntValue 1
  castToInt (Caster (BoolValue False)) = Caster $ IntValue 0

  castToFloat (Caster (IntValue x))   = Caster $ FloatValue $ fromIntegral x
  castToFloat (Caster (FloatValue x)) = Caster $ FloatValue x
  castToFloat (Caster (StringValue s)) = Caster $ FloatValue $ mbToInt $ readMaybe s
    where
      mbToInt Nothing  = fromIntegral $ length s
      mbToInt (Just y) = y
  castToFloat (Caster (BoolValue True))  = Caster $ FloatValue 1
  castToFloat (Caster (BoolValue False)) = Caster $ FloatValue 0

  castToString (Caster (IntValue x))      = Caster $ StringValue $ show x
  castToString (Caster (FloatValue x))    = Caster $ StringValue $ show x
  castToString (Caster (StringValue s))   = Caster $ StringValue s
  castToString (Caster (BoolValue True))  = Caster $ StringValue "1"
  castToString (Caster (BoolValue False)) = Caster $ StringValue ""

  castToBool (Caster (IntValue x))
    | x > 0 = Caster $ BoolValue True
    | otherwise = Caster $ BoolValue False
  castToBool (Caster (FloatValue x))
    | x > 0 = Caster $ BoolValue True
    | otherwise = Caster $ BoolValue False
  castToBool (Caster (StringValue s))
    | s == "" = Caster $ BoolValue False
    | otherwise = Caster $ BoolValue True
  castToBool (Caster (BoolValue x)) = Caster $ BoolValue x
