module Types where
import Data.Align
import Data.ByteString (ByteString)
import Data.Eq.Deriving
import Data.Functor.Foldable
import Data.Int
import Data.Text (Text)
import Data.These
import GHC.Generics
import Text.Show.Deriving

data NT r
  = NInteger
  | NDouble
  | NText
  | NByteArray
  | NBool
  | NJSON
  | NTuple [r]
  | NStruct [(String, r)]
  | NSum [(String, r)]
  | NArray r
  | NOptional r
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Generic1)

deriveEq1 ''NT
deriveShow1 ''NT

data NV r
  = VInteger Int64
  | VDouble Double
  | VText Text
  | VByteArray ByteString
  | VBool Bool
  | VJSON Text
  | VTuple [r]
  | VStruct [r]
  | VSum Int r
  | VArray Int [r]
  | VOptional (Maybe r)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Generic1)

deriveEq1 ''NV
deriveShow1 ''NV

typeCheck :: NT a -> NV b -> Maybe (NV (a, b))
typeCheck  NInteger     (VInteger   i) = Just $ VInteger i
typeCheck  NDouble      (VDouble    d) = Just $ VDouble d
typeCheck  NText        (VText      t) = Just $ VText t
typeCheck  NByteArray   (VByteArray b) = Just $ VByteArray b
typeCheck  NJSON        (VJSON      j) = Just $ VJSON j
typeCheck (NTuple   ts) (VTuple    vs) = VTuple <$> traverse justThese (align ts vs)
typeCheck (NSum     ts) (VSum cons vs) = if cons >= 0 && cons < length ts
                                         then Just $ VSum cons (snd $ ts !! cons, vs)
                                         else Nothing
typeCheck (NArray    t) (VArray l  ts) = Just . VArray l $ fmap (t,) ts
typeCheck (NOptional t) (VOptional  m) = Just . VOptional $ fmap (t,) m
typeCheck _             _              = Nothing

tc :: Fix NT -> Fix NV -> Maybe (Fix NV)
tc (Fix a) (Fix b) = typeCheck a b >>= fmap Fix . traverse (uncurry tc)
