{-# LANGUAGE TemplateHaskell #-}

module Lib.Types.EmverProp where

import Startlude hiding (
    Any,
    filter,
    reduce,
 )

import Data.Attoparsec.Text qualified as Atto
import Hedgehog as Test
import Hedgehog.Gen as Gen
import Hedgehog.Range
import Lib.Types.Emver (Version (Version), VersionRange (..), fl, fr, geq, gt, leq, lt, neq, nodes, parseRange, reduce, satisfies, (<||))
import System.Timeout (timeout)


versionGen :: MonadGen m => m Version
versionGen = do
    a <- word (linear 0 30)
    b <- word (linear 0 30)
    c <- word (linear 0 30)
    d <- word (linear 0 30)
    pure $ Version (a, b, c, d)


rangeGen :: MonadGen m => m VersionRange
rangeGen = shrink rangeShrink $ choice [pure None, pure Any, anchorGen, disjGen, conjGen]
    where
        rangeShrink (Conj a b) = [a, b]
        rangeShrink (Disj a b) = [a, b]
        rangeShrink _ = []


anchorGen :: MonadGen m => m VersionRange
anchorGen = do
    c <- element [LT, EQ, GT]
    f <- element [Left, Right]
    Anchor (f c) <$> versionGen


conjGen :: MonadGen m => m VersionRange
conjGen = liftA2 Conj rangeGen rangeGen


conjOnlyGen :: MonadGen m => m VersionRange
conjOnlyGen = shrink conjOnlyShrink . prune $ choice [anchorGen, liftA2 Conj conjOnlyGen conjOnlyGen]
    where
        conjOnlyShrink :: VersionRange -> [VersionRange]
        conjOnlyShrink (Conj a b) = [a, b]
        conjOnlyShrink _ = []


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


prop_reduceTerminates :: Property
prop_reduceTerminates = withTests 1000 . property $ do
    a <- forAll $ filter ((<= 100) . nodes) rangeGen
    b <- lift $ timeout 100_000 (pure $! reduce a)
    case b of
        Nothing -> failure
        Just _ -> success


prop_reduceIdentity :: Property
prop_reduceIdentity = withTests 1000 . property $ do
    a <- forAll $ filter (((>= 3) <&&> (<= 100)) . nodes) rangeGen
    obs <- forAll versionGen
    let b = reduce a
    unless (b /= a) Test.discard
    obs <|| a === obs <|| b


prop_reduceIdempotence :: Property
prop_reduceIdempotence = withTests 1000 . property $ do
    a <- forAll $ filter (((>= 3) <&&> (<= 100)) . nodes) rangeGen
    let b = reduce a
    annotateShow b
    let c = reduce b
    annotateShow c
    b === c


prop_reduceConjTreeNormalForm :: Property
prop_reduceConjTreeNormalForm = withTests 1000 . property $ do
    a <- forAll $ filter ((<= 100) . nodes) conjOnlyGen
    let b = reduce a
    annotateShow b
    assert $ isConjNF b


isConjNF :: VersionRange -> Bool
isConjNF = \case
    Any -> True
    None -> True
    Anchor _ _ -> True
    Conj (Anchor _ pta) (Anchor _ ptb) -> pta < ptb
    Conj (Conj (Anchor opa pta) (Anchor opb ptb)) (Anchor opc ptc) ->
        pta < ptb
            && opa /= neq
            && opb /= neq
            && opc == neq
            && ptc > pta
            && ptc < ptb
    Conj (Anchor opa pta) (Conj (Anchor opb ptb) (Anchor opc ptc)) ->
        opb == neq
            && opc == neq
            && ptb < ptc
            && (opa /= neq || pta < ptb)
            && ((opa /= gt && opa /= geq) || pta < ptb)
            && ((opa /= lt && opa /= leq) || pta > ptc)
    (Conj (Conj (Anchor opa pta) (Anchor opb ptb)) (Conj (Anchor opc ptc) (Anchor opd ptd))) ->
        opc == neq && opd == neq && opa /= neq && opb /= neq && pta < ptb && ptc < ptd && ptc > pta && ptd < ptb
    (Conj x@(Conj (Anchor opa pta) (Anchor opb ptb)) (Conj (Anchor opc ptc) r)) ->
        pta < ptc && ptc < ptb && opa /= neq && opb /= neq && opc == neq && isConjNF (Conj x r)
    (Conj a@(Anchor opa pta) y'@(Conj (Anchor _ ptb) r)) ->
        (opa == neq && pta < ptb && isConjNF y')
            || (fr opa && pta < ptb && isConjNF y')
            || (fl opa && pta > ptb && isConjNF (Conj a r))
    _ -> False


tests :: IO Bool
tests = checkParallel $ $$discover
