{-# LANGUAGE TemplateHaskell #-}
module Types where
import Data.Align
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Eq.Deriving
import Data.Functor.Foldable
import Data.Int
import Data.Ord.Deriving
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
deriveOrd1 ''NT
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
deriveOrd1 ''NV
deriveShow1 ''NV

data X a = X (Fix NT) (NV a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Generic1)

deriveEq1 ''X
deriveOrd1 ''X
deriveShow1 ''X

typeCheck :: NT a -> NV b -> Maybe (NV (a, b))
typeCheck  NInteger     (VInteger   i) = Just $ VInteger i
typeCheck  NDouble      (VDouble    d) = Just $ VDouble d
typeCheck  NText        (VText      t) = Just $ VText t
typeCheck  NByteArray   (VByteArray b) = Just $ VByteArray b
typeCheck  NBool        (VBool      b) = Just $ VBool b
typeCheck  NJSON        (VJSON      j) = Just $ VJSON j
typeCheck (NTuple   ts) (VTuple    vs) = VTuple <$> traverse justThese (align ts vs)
typeCheck (NStruct  ts) (VStruct   vs) = VStruct <$> traverse justThese (align (fmap snd ts) vs)
typeCheck (NSum     ts) (VSum cons vs) = if cons >= 0 && cons < length ts
                                         then Just $ VSum cons (snd $ ts !! cons, vs)
                                         else Nothing
typeCheck (NArray    t) (VArray l  ts) = Just . VArray l $ fmap (t,) ts
typeCheck (NOptional t) (VOptional  m) = Just . VOptional $ fmap (t,) m
typeCheck _             _              = Nothing

tc :: Fix NT -> Fix NV -> Maybe (Fix NV)
tc (Fix a) (Fix b) = typeCheck a b >>= fmap Fix . traverse (uncurry tc)

afoo :: Fix X -> Maybe (Fix X)
afoo = go where go (Fix x) = altfoo x >>= fmap Fix . traverse go

altfoo :: X a -> Maybe (X a)
altfoo (X (Fix t) v) = X (Fix t) . fmap snd <$> typeCheck t v

foo :: X a -> Maybe (X a)
foo (X (Fix t) v) = case typeCheck t v of
                      Nothing -> Nothing
                      Just w  -> X (Fix t) <$> baz (qux t) w

baz :: ([a] -> Bool) -> NV (a, b) -> Maybe (NV b)
baz f b = case traverse (first pure) b of
            (a, v) -> if f a then Just v else Nothing

qux :: Eq a => NT a -> [a] -> Bool
qux NInteger [] = True
qux NDouble [] = True
qux NText [] = True
qux NByteArray [] = True
qux NBool [] = True
qux NJSON [] = True
qux (NTuple ts) vs = boink ts vs
qux (NStruct ts) vs = boink (fmap snd ts) vs
qux (NSum ts) vs = boink (fmap snd ts) vs
qux (NArray t) vs = all (== t) vs
qux (NOptional t) v = case v of
                        []  -> True
                        [x] -> x == t
                        _   -> False
qux _ _ = False

boink :: Eq a => [a] -> [a] -> Bool
boink as bs = maybe False comp go
  where go = traverse justThese (align as bs)
        comp = all $ uncurry (==)

extractNV :: Fix X -> Fix NV
extractNV = Fix . go . fmap extractNV . unfix
  where go :: X a -> NV a
        go (X _ v) = v

extractNT :: Fix X -> Fix NT
extractNT (Fix (X t _)) = t
