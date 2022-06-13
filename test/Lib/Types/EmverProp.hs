{-# LANGUAGE TemplateHaskell #-}

module Lib.Types.EmverProp where

import Startlude hiding (
    Any,
    reduce,
 )

import Data.Attoparsec.Text qualified as Atto
import Hedgehog as Test
import Hedgehog.Gen as Gen
import Hedgehog.Range
import Lib.Types.Emver
import UnliftIO qualified


versionGen :: MonadGen m => m Version
versionGen = do
    a <- word (linear 0 30)
    b <- word (linear 0 30)
    c <- word (linear 0 30)
    d <- word (linear 0 30)
    pure $ Version (a, b, c, d)


rangeGen :: MonadGen m => m VersionRange
rangeGen = choice [pure None, pure Any, anchorGen, disjGen, conjGen]


anchorGen :: MonadGen m => m VersionRange
anchorGen = do
    c <- element [LT, EQ, GT]
    f <- element [Left, Right]
    Anchor (f c) <$> versionGen


conjGen :: MonadGen m => m VersionRange
conjGen = liftA2 Conj rangeGen rangeGen


disjGen :: MonadGen m => m VersionRange
disjGen = liftA2 Disj rangeGen rangeGen


prop_conjAssoc :: Property
prop_conjAssoc = property $ do
    a <- forAll rangeGen
    b <- forAll rangeGen
    c <- forAll rangeGen
    obs <- forAll versionGen
    (obs <|| Conj a (Conj b c)) === (obs <|| Conj (Conj a b) c)


prop_conjCommut :: Property
prop_conjCommut = property $ do
    a <- forAll rangeGen
    b <- forAll rangeGen
    obs <- forAll versionGen
    (obs <|| Conj a b) === (obs <|| Conj b a)


prop_disjAssoc :: Property
prop_disjAssoc = property $ do
    a <- forAll rangeGen
    b <- forAll rangeGen
    c <- forAll rangeGen
    obs <- forAll versionGen
    (obs <|| Disj a (Disj b c)) === (obs <|| Disj (Disj a b) c)


prop_disjCommut :: Property
prop_disjCommut = property $ do
    a <- forAll rangeGen
    b <- forAll rangeGen
    obs <- forAll versionGen
    (obs <|| Disj a b) === (obs <|| Disj b a)


prop_anyIdentConj :: Property
prop_anyIdentConj = property $ do
    a <- forAll rangeGen
    obs <- forAll versionGen
    obs <|| Conj Any a === obs <|| a


prop_noneIdentDisj :: Property
prop_noneIdentDisj = property $ do
    a <- forAll rangeGen
    obs <- forAll versionGen
    obs <|| Disj None a === obs <|| a


prop_noneAnnihilatesConj :: Property
prop_noneAnnihilatesConj = property $ do
    a <- forAll rangeGen
    obs <- forAll versionGen
    obs <|| Conj None a === obs <|| None


prop_anyAnnihilatesDisj :: Property
prop_anyAnnihilatesDisj = property $ do
    a <- forAll rangeGen
    obs <- forAll versionGen
    obs <|| Disj Any a === obs <|| Any


prop_conjDistributesOverDisj :: Property
prop_conjDistributesOverDisj = property $ do
    a <- forAll rangeGen
    b <- forAll rangeGen
    c <- forAll rangeGen
    obs <- forAll versionGen
    obs <|| Conj a (Disj b c) === obs <|| Disj (Conj a b) (Conj a c)


prop_disjDistributesOverConj :: Property
prop_disjDistributesOverConj = property $ do
    a <- forAll rangeGen
    b <- forAll rangeGen
    c <- forAll rangeGen
    obs <- forAll versionGen
    obs <|| Disj a (Conj b c) === obs <|| Conj (Disj a b) (Disj a c)


prop_anyAcceptsAny :: Property
prop_anyAcceptsAny = property $ do
    obs <- forAll versionGen
    assert $ obs <|| Any


prop_noneAcceptsNone :: Property
prop_noneAcceptsNone = property $ do
    obs <- forAll versionGen
    assert . not $ obs <|| None


prop_conjBoth :: Property
prop_conjBoth = property $ do
    a <- forAll rangeGen
    b <- forAll rangeGen
    obs <- forAll versionGen
    (obs <|| Conj a b) === (obs <|| a && obs <|| b)


prop_disjEither :: Property
prop_disjEither = property $ do
    a <- forAll rangeGen
    b <- forAll rangeGen
    obs <- forAll versionGen
    (obs <|| Disj a b) === (obs <|| a || obs <|| b)


prop_rangeParseRoundTrip :: Property
prop_rangeParseRoundTrip = withShrinks 0 . property $ do
    a <- forAll rangeGen
    obs <- forAll versionGen
    -- we do not use 'tripping' here since 'tripping' requires equality of representation
    -- we only want to check equality up to OBSERVATION
    annotateShow a
    annotateShow (Atto.parseOnly parseRange (show a))
    (satisfies obs <$> Atto.parseOnly parseRange (show a)) === Right (satisfies obs a)


prop_anchorLeftIsNegatedRight :: Property
prop_anchorLeftIsNegatedRight = property $ do
    a <- forAll anchorGen
    neg <- case a of
        Anchor (Right o) v -> pure $ Anchor (Left o) v
        Anchor (Left o) v -> pure $ Anchor (Right o) v
        _ -> Test.discard
    obs <- forAll versionGen
    obs <|| a /== obs <|| neg


prop_reduceConjAnchor :: Property
prop_reduceConjAnchor = property $ do
    a <- forAll anchorGen
    b <- forAll anchorGen
    obs <- forAll versionGen
    obs <|| reduce (Conj a b) === obs <|| Conj a b


prop_reduceDisjAnchor :: Property
prop_reduceDisjAnchor = property $ do
    a <- forAll anchorGen
    b <- forAll anchorGen
    obs <- forAll versionGen
    obs <|| reduce (Disj a b) === obs <|| Disj a b


prop_reduceIdentity :: Property
prop_reduceIdentity = withTests 2000 . withDiscards 90 $
    property $ do
        -- a   <- forAll rangeGen
        a <- forAll conjGen
        obs <- forAll versionGen
        b <- liftIO $ pure (reduce a) `catch` \e -> throwIO (e :: ErrorCall)
        unless (b /= a) Test.discard
        obs <|| a === obs <|| b


tests :: IO Bool
tests = checkParallel $ $$discover
