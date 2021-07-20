{-# LANGUAGE
    DataKinds
  , TypeFamilies
  , ConstraintKinds
  , TypeOperators
#-}

module GraphQL.IO.Kinds where

import GraphQL.Class (TypeKind(..))

data TypeIO = IN | OUT

type family k ?>> io where
  SCALAR       ?>> io  = True
  ENUM         ?>> io  = True
  -- UNION        ?>> OUT = True
  OBJECT       ?>> OUT = True
  INPUT_OBJECT ?>> IN  = True
  (k' k)      ?>> io  = k ?>> io
  k            ?>> io  = False

type k !>> io = k ?>> io ~ True
