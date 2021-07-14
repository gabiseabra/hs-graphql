{-# LANGUAGE DataKinds, TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}

module GraphQL.IO.Kinds where

import GraphQL.Typeable (TypeKind(..))

data TypeIO = IN | OUT

type family k ?>> io where
  SCALAR       ?>> io  = True
  ENUM         ?>> io  = True
  UNION        ?>> OUT = True
  OBJECT       ?>> OUT = True
  INPUT_OBJECT ?>> IN  = True
  (LIST k)     ?>> io  = k ?>> io
  (NULLABLE k) ?>> io  = k ?>> io
  k            ?>> io  = False

type k !>> io = k ?>> io ~ True
