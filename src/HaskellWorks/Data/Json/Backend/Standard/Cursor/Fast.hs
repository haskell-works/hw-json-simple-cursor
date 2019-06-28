{-# LANGUAGE TypeFamilies #-}

module HaskellWorks.Data.Json.Backend.Standard.Cursor.Fast
  ( Cursor
  ) where

import Data.Word
import HaskellWorks.Data.Json.Backend.Standard.Cursor.Generic
import HaskellWorks.Data.Json.Backend.Standard.Cursor.Specific
import HaskellWorks.Data.RankSelect.CsPoppy

import qualified Data.ByteString                              as BS
import qualified Data.Vector.Storable                         as DVS
import qualified HaskellWorks.Data.BalancedParens.RangeMinMax as RMM

data Fast

instance SpecificCursor Fast where
  type CursorOf Fast = Cursor

type Cursor = GenericCursor BS.ByteString CsPoppy (RMM.RangeMinMax (DVS.Vector Word64))
