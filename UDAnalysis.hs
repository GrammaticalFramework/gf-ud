module UDAnalysis where

import UDConcepts
import GFConcepts
import UDAnnotations

import PGF

import Data.List

data UDType = UDType {
  udVal  :: POS,
  udArgs :: [(POS,(Label,[UDData]))]
  }
 deriving Show
 
typesInUDTree :: UDTree -> [UDType]
typesInUDTree tr@(RTree un uts) =
  UDType (udUPOS un) [(udUPOS n,(udDEPREL n, udFEATS n)) | RTree n _ <- uts] :
  concatMap typesInUDTree uts

-- ignore argument order and morphological tags
normalizeUDType :: UDType -> UDType
normalizeUDType ut = ut {
  udArgs = sort [(p,(l,[])) | (p,(l,_)) <- udArgs ut]
  }