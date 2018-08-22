module Symmetry where

data Mode = Mode { axis :: Axis, glideReflect :: Bool }
  deriving (Eq, Read, Show)

data Axis = Ortho | Dia
  deriving (Eq, Read, Show)
