{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

-- |
--Module : Lib.Types.Emver
--Description : Semver with 4th digit extension for Embassy
--License : Start9 Non-Commercial
--Maintainer : keagan@start9labs.com
--Stability : experimental
--Portability : portable
--
--This module was designed to address the problem of releasing updates to Embassy Packages where the upstream project was
--either unaware of or apathetic towards supporting their application on the Embassy platform. In most cases, the original
--package will support <https://semver.org/spec/v2.0.0.html semver2>. This leaves us with the problem where we would like
--to preserve the original package's version, since one of the goals of the Embassy platform is transparency. However, on
--occasion, we have screwed up and published a version of a package that needed to have its metadata updated. In this
--scenario we were left with the conundrum of either unilaterally claiming a version number of a package we did not author
--or let the issue persist until the next update. Neither of these promote good user experiences, for different reasons.
--This module extends the semver standard linked above with a 4th digit, which is given PATCH semantics.
module Lib.Types.Emver (
    major,
    minor,
    patch,
    revision,
    satisfies,
    (<||),
    (||>),
    -- we do not export 'None' because it is useful for its internal algebraic properties only
    VersionRange (..),
    Version (..),
    AnyRange (..),
    AllRange (..),
    conj,
    disj,
    exactly,
    parseVersion,
    parseRange,
    reduce,
    nodes,
) where

import Startlude (
    Alternative ((<|>)),
    Applicative (liftA2, pure, (*>), (<*)),
    Bool (..),
    Either (..),
    Eq (..),
    Foldable (foldMap, length),
    Hashable,
    IsString (..),
    Monad ((>>=)),
    Monoid (mappend, mempty),
    NFData (..),
    Num ((+)),
    Ord (compare),
    Ordering (..),
    Read,
    Semigroup ((<>)),
    Show,
    String,
    Word,
    either,
    flip,
    id,
    on,
    seq,
    show,
    ($),
    ($>),
    (&&),
    (.),
    (<$>),
    (<&>),
    (<<$>>),
    (||),
 )

import Control.Monad.Fail (fail)
import Data.Aeson (ToJSONKey)
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Text as T
import GHC.Base (error)
import qualified GHC.Read as GHC (
    readsPrec,
 )
import qualified GHC.Show as GHC (
    show,
 )


-- | AppVersion is the core representation of the SemverQuad type.
newtype Version = Version {unVersion :: (Word, Word, Word, Word)} deriving (Eq, Ord, ToJSONKey, Hashable)


instance Show Version where
    show (Version (x, y, z, q)) =
        let postfix = if q == 0 then "" else '.' : GHC.show q
         in GHC.show x <> "." <> GHC.show y <> "." <> GHC.show z <> postfix
instance IsString Version where
    fromString s = either error id $ Atto.parseOnly parseVersion (T.pack s)
instance Read Version where
    readsPrec _ s = case Atto.parseOnly parseVersion (T.pack s) of
        Left _ -> []
        Right a -> [(a, "")]


-- | A change in the value found at 'major' implies a breaking change in the API that this version number describes
major :: Version -> Word
major (Version (x, _, _, _)) = x


-- | A change in the value found at 'minor' implies a backwards compatible addition to the API that this version number
-- describes
minor :: Version -> Word
minor (Version (_, y, _, _)) = y


-- | A change in the value found at 'patch' implies that the implementation of the API has changed without changing the
-- invariants promised by the API. In many cases this will be incremented when repairing broken functionality
patch :: Version -> Word
patch (Version (_, _, z, _)) = z


-- | This is the fundamentally new value in comparison to the original semver 2.0 specification. It is given the same
-- semantics as 'patch' above, which begs the question, when should you update this value instead of that one. Generally
-- speaking, if you are both the package author and maintainer, you should not ever increment this number, as it is
-- redundant with 'patch'. However, if you maintain a package on some distribution channel, and you are /not/ the
-- original author, then it is encouraged for you to increment 'quad' instead of 'patch'.
revision :: Version -> Word
revision (Version (_, _, _, q)) = q


-- | 'Operator' is the type that specifies how to compare against the target version. Right represents the ordering,
-- Left negates it
type Operator = Either Ordering Ordering


primOrd :: Operator -> Ordering
primOrd = either id id


complement :: Ordering -> Ordering -> Ordering
complement LT EQ = GT
complement LT GT = EQ
complement EQ GT = LT
complement a b | a == b = a
complement a b = complement b a


lt :: Operator
lt = Right LT


leq :: Operator
leq = Left GT


eq :: Operator
eq = Right EQ


neq :: Operator
neq = Left EQ


gt :: Operator
gt = Right GT


geq :: Operator
geq = Left LT


-- | 'VersionRange' is the algebra of sets of versions. They can be constructed by having an 'Anchor' term which
-- compares against the target version, or can be described with 'Conj' which is a conjunction, or 'Disj', which is a
-- disjunction. The 'Any' and 'All' terms are primarily there to round out the algebra, but 'Any' is also exposed due to
-- its usage in semantic versioning in general. The 'None' term is not useful to the end user as there would be no
-- reasonable usage of it to describe version sets. It is included for its utility as a unit on 'Disj' and possibly as
-- a zero on 'Conj'
--
-- Laws (reflected in implementations of smart constructors):
-- Commutativity of conjunction: Conj a b === Conj b a
-- Commutativity of disjunction: Disj a b === Disj b a
-- Associativity of conjunction: Conj (Conj a b) c === Conj a (Conj b c)
-- Associativity of disjunction: Disj (Disj a b) c === Disj a (Disj b c)
-- Identity of conjunction: Any `Conj` a === a
-- Identity of disjunction: None `Disj` a === a
-- Zero of conjunction: None `Conj` a === None
-- Zero of disjunction: Any `Disj` a === Any
-- Distributivity of conjunction over disjunction: Conj a (Disj b c) === Disj (Conj a b) (Conj a c)
-- Distributivity of disjunction over conjunction: Disj a (Conj b c) === Conj (Disj a b) (Disj a c)
data VersionRange
    = Anchor !Operator !Version
    | Conj VersionRange VersionRange
    | Disj VersionRange VersionRange
    | Any
    | None
    deriving (Eq)


instance NFData VersionRange where
    rnf (Conj a b) = rnf a `seq` rnf b
    rnf (Disj a b) = rnf a `seq` rnf b
    rnf other = other `seq` ()


nodes :: VersionRange -> Word64
nodes Any = 1
nodes None = 1
nodes (Anchor _ _) = 1
nodes (Conj a b) = 1 + nodes a + nodes b
nodes (Disj a b) = 1 + nodes a + nodes b


-- | Smart constructor for conjunctions. Eagerly evaluates zeros and identities
conj :: VersionRange -> VersionRange -> VersionRange
conj a b = reduce $ Conj a b


-- | Smart constructor for disjunctions. Eagerly evaluates zeros and identities
disj :: VersionRange -> VersionRange -> VersionRange
disj a b = reduce $ Disj a b


reduce :: VersionRange -> VersionRange
reduce Any = Any
reduce None = None
reduce vr@(Anchor _ _) = vr
reduce (Conj Any vr) = vr
reduce (Conj vr Any) = vr
reduce (Conj None _) = None
reduce (Conj _ None) = None
reduce (Disj Any _) = Any
reduce (Disj _ Any) = Any
reduce (Disj None vr) = vr
reduce (Disj vr None) = vr
reduce x@(Conj a@(Anchor op pt) b@(Anchor op' pt')) = case compare pt pt' of
    GT -> reduce (Conj b a) -- conj commutes so we can make normalization order the points
    EQ -> case (isRight op, isRight op', isRight op == isRight op', primOrd op == primOrd op') of
        -- the theorems found here will elucidate what is going on
        -- https://faculty.uml.edu/klevasseur/ads/s-laws-of-set-theory.html
        -- conj idempodent law: these sets are identical
        (_, _, True, True) -> a
        -- conj complement law: these sets are opposites
        (_, _, False, True) -> None
        -- inequality incompatibility: these sets do not overlap
        (True, True, _, False) -> None
        -- conj absorption law (right): the right set is more specific
        (False, True, _, False) -> b
        -- conj absorption law (left): the left set is more specific
        (True, False, _, False) -> a
        -- all that is left is to intersect these sets. In every one of these cases the intersection can be expressed
        -- as exactly the ordering that is not mentioned by the other two.
        (False, False, _, False) -> Anchor (Right $ complement (primOrd op) (primOrd op')) pt
    LT -> case (op, op') of -- at this point the left post is is guaranteed to be a lower version than the right
        (Left LT, Left LT) -> b
        (Left LT, Left EQ) -> x
        (Left LT, Left GT) -> x
        (Left LT, Right LT) -> x
        (Left LT, Right EQ) -> b
        (Left LT, Right GT) -> b
        (Left EQ, Left LT) -> b
        (Left EQ, Left EQ) -> x
        (Left EQ, Left GT) -> x
        (Left EQ, Right LT) -> x
        (Left EQ, Right EQ) -> b
        (Left EQ, Right GT) -> b
        (Left GT, Left LT) -> None
        (Left GT, Left EQ) -> a
        (Left GT, Left GT) -> a
        (Left GT, Right LT) -> a
        (Left GT, Right EQ) -> None
        (Left GT, Right GT) -> None
        (Right LT, Left LT) -> None
        (Right LT, Left EQ) -> a
        (Right LT, Left GT) -> a
        (Right LT, Right LT) -> a
        (Right LT, Right EQ) -> None
        (Right LT, Right GT) -> None
        (Right EQ, Left LT) -> None
        (Right EQ, Left EQ) -> a
        (Right EQ, Left GT) -> a
        (Right EQ, Right LT) -> a
        (Right EQ, Right EQ) -> None
        (Right EQ, Right GT) -> None
        (Right GT, Left LT) -> b
        (Right GT, Left EQ) -> x
        (Right GT, Left GT) -> x
        (Right GT, Right LT) -> x
        (Right GT, Right EQ) -> b
        (Right GT, Right GT) -> b
reduce x@(Disj a@(Anchor op pt) b@(Anchor op' pt')) = case compare pt pt' of
    GT -> reduce (Disj b a)
    EQ -> case (isRight op, isRight op', isRight op == isRight op', primOrd op == primOrd op') of
        (_, _, True, True) -> a
        (_, _, False, True) -> Any
        (True, True, _, False) -> Anchor (Left $ complement (primOrd op) (primOrd op')) pt
        (False, True, _, False) -> a
        (True, False, _, False) -> b
        (False, False, _, False) -> Any
    LT -> case (op, op') of
        (Left LT, Left LT) -> a
        (Left LT, Left EQ) -> Any
        (Left LT, Left GT) -> Any
        (Left LT, Right LT) -> Any
        (Left LT, Right EQ) -> a
        (Left LT, Right GT) -> a
        (Left EQ, Left LT) -> a
        (Left EQ, Left EQ) -> Any
        (Left EQ, Left GT) -> Any
        (Left EQ, Right LT) -> Any
        (Left EQ, Right EQ) -> a
        (Left EQ, Right GT) -> a
        (Left GT, Left LT) -> x
        (Left GT, Left EQ) -> b
        (Left GT, Left GT) -> b
        (Left GT, Right LT) -> b
        (Left GT, Right EQ) -> x
        (Left GT, Right GT) -> x
        (Right LT, Left LT) -> x
        (Right LT, Left EQ) -> b
        (Right LT, Left GT) -> b
        (Right LT, Right LT) -> b
        (Right LT, Right EQ) -> x
        (Right LT, Right GT) -> x
        (Right EQ, Left LT) -> x
        (Right EQ, Left EQ) -> b
        (Right EQ, Left GT) -> b
        (Right EQ, Right LT) -> b
        (Right EQ, Right EQ) -> x
        (Right EQ, Right GT) -> x
        (Right GT, Left LT) -> a
        (Right GT, Left EQ) -> Any
        (Right GT, Left GT) -> Any
        (Right GT, Right LT) -> Any
        (Right GT, Right EQ) -> a
        (Right GT, Right GT) -> a
reduce (Conj a@(Conj _ _) b@(Anchor _ _)) = reduce (Conj b a)
reduce x@(Conj a@(Anchor op pt) b@(Conj p q)) = case (p, q) of
    ((Anchor opP ptP), (Anchor opQ ptQ)) ->
        if ptP >= ptQ
            then bail
            else case (opP, opQ) of
                -- diamonds <>
                (Right GT, Right LT) ->
                    if
                            | (op == lt || op == leq || op == eq) && pt <= ptP -> None
                            | (op == gt || op == geq || op == neq) && pt <= ptP -> b
                            | (op == lt || op == leq || op == neq) && pt >= ptQ -> b
                            | (op == gt || op == geq || op == eq) && pt >= ptQ -> None
                            | (op == lt || op == leq) -> Conj p a
                            | (op == gt || op == geq) -> Conj a q
                            | op == eq -> a
                            | op == neq -> Conj b a
                            | otherwise -> x
                (Left LT, Right LT) ->
                    if
                            | (op == lt || op == leq || op == eq) && pt < ptP -> None
                            | (op == gt || op == geq || op == neq) && pt < ptP -> b
                            | op == lt && pt == ptP -> None
                            | op == leq && pt == ptP -> Anchor (Right EQ) pt
                            | op == eq && pt == ptP -> a
                            | op == gt && pt == ptP -> Conj a q
                            | op == geq && pt == ptP -> b
                            | op == neq && pt == ptP -> Conj (Anchor (Right GT) pt) q
                            | (op == geq || op == neq) && pt == ptP -> b
                            | (op == lt || op == leq || op == neq) && pt >= ptQ -> b
                            | (op == gt || op == geq || op == eq) && pt >= ptQ -> None
                            | (op == lt || op == leq) -> Conj p a
                            | (op == gt || op == geq) -> Conj a q
                            | op == eq -> a
                            | op == neq -> Conj b a
                            | otherwise -> x
                (Right GT, Left GT) ->
                    if
                            | (op == lt || op == leq || op == eq) && pt <= ptP -> None
                            | (op == gt || op == geq || op == neq) && pt <= ptP -> b
                            | (op == gt || op == geq || op == eq) && pt > ptQ -> None
                            | (op == lt || op == leq || op == neq) && pt > ptQ -> b
                            | op == lt && pt == ptQ -> Conj p a
                            | op == leq && pt == ptQ -> b
                            | op == eq && pt == ptQ -> a
                            | op == gt && pt == ptQ -> None
                            | op == geq && pt == ptQ -> Anchor (Right EQ) pt
                            | op == neq && pt == ptQ -> Conj p (Anchor lt pt)
                            | (op == gt || op == geq) -> Conj a q
                            | (op == lt || op == leq) -> Conj p a
                            | op == eq -> a
                            | op == neq -> Conj b a
                            | otherwise -> x
                (Left LT, Left GT) ->
                    if
                            | (op == lt || op == leq || op == eq) && pt < ptP -> None
                            | (op == gt || op == geq || op == neq) && pt < ptP -> b
                            | otherwise -> x
                -- fish left <x
                (Right GT, Left EQ) -> x
                (Left LT, Left EQ) -> x
                -- fish right x>
                (Left EQ, Right LT) -> x
                (Left EQ, Left GT) -> x
                -- dead eyes xx
                (Left EQ, Left EQ) -> x
                -- all other states are unstable for conj
                _ -> bail
    _ -> x
    where
        bail = reduce (Conj a (reduce b))
reduce rest = rest


exactly :: Version -> VersionRange
exactly = Anchor (Right EQ)


instance Show VersionRange where
    show (Anchor (Left EQ) v) = '!' : '=' : GHC.show v
    show (Anchor (Right EQ) v) = '=' : GHC.show v
    show (Anchor (Left LT) v) = '>' : '=' : GHC.show v
    show (Anchor (Right LT) v) = '<' : GHC.show v
    show (Anchor (Left GT) v) = '<' : '=' : GHC.show v
    show (Anchor (Right GT) v) = '>' : GHC.show v
    show (Conj a@(Disj _ _) b@(Disj _ _)) = paren (GHC.show a) <> (' ' : paren (GHC.show b))
    show (Conj a@(Disj _ _) b) = paren (GHC.show a) <> (' ' : GHC.show b)
    show (Conj a b@(Disj _ _)) = GHC.show a <> (' ' : paren (GHC.show b))
    show (Conj a b) = GHC.show a <> (' ' : GHC.show b)
    show (Disj a b) = GHC.show a <> " || " <> GHC.show b
    show Any = "*"
    show None = "!"
instance Read VersionRange where
    readsPrec _ s = case Atto.parseOnly parseRange (T.pack s) of
        Left _ -> []
        Right a -> [(a, "")]


paren :: String -> String
paren = mappend "(" . flip mappend ")"


newtype AnyRange = AnyRange {unAnyRange :: VersionRange}
instance Semigroup AnyRange where
    (<>) = AnyRange <<$>> disj `on` unAnyRange
instance Monoid AnyRange where
    mempty = AnyRange None


newtype AllRange = AllRange {unAllRange :: VersionRange}
instance Semigroup AllRange where
    (<>) = AllRange <<$>> conj `on` unAllRange
instance Monoid AllRange where
    mempty = AllRange Any


-- | Predicate for deciding whether the 'Version' is in the 'VersionRange'
satisfies :: Version -> VersionRange -> Bool
satisfies v (Anchor op v') = either (\c x y -> compare x y /= c) (\c x y -> compare x y == c) op v v'
satisfies v (Conj a b) = v `satisfies` a && v `satisfies` b
satisfies v (Disj a b) = v `satisfies` a || v `satisfies` b
satisfies _ Any = True
satisfies _ None = False


(<||) :: Version -> VersionRange -> Bool
(<||) = satisfies
{-# INLINE (<||) #-}


(||>) :: VersionRange -> Version -> Bool
(||>) = flip satisfies
{-# INLINE (||>) #-}


parseOperator :: Atto.Parser Operator
parseOperator =
    (Atto.char '=' $> Right EQ)
        <|> (Atto.string "!=" $> Left EQ)
        <|> (Atto.string ">=" $> Left LT)
        <|> (Atto.string "<=" $> Left GT)
        <|> (Atto.char '>' $> Right GT)
        <|> (Atto.char '<' $> Right LT)


parseVersion :: Atto.Parser Version
parseVersion = do
    major' <- Atto.decimal <* Atto.char '.'
    minor' <- Atto.decimal <* Atto.char '.'
    patch' <- Atto.decimal
    quad' <- Atto.option 0 $ Atto.char '.' *> Atto.decimal
    pure $ Version (major', minor', patch', quad')


-- >>> Atto.parseOnly parseRange "=2.3.4 1.2.3.4 - 2.3.4.5 (>3.0.0 || <3.4.5)"
-- Right =2.3.4 >=1.2.3.4 <=2.3.4.5 ((>3.0.0 || <3.4.5))
-- >>> Atto.parseOnly parseRange "0.2.6"
-- >>> Atto.parseOnly parseRange ">=2.14.1.1 <3.0.0"
-- Right >=2.14.1.1 <3.0.0
parseRange :: Atto.Parser VersionRange
parseRange = s <|> any <|> none <|> (Anchor (Right EQ) <$> parseVersion)
    where
        any = Atto.char '*' *> pure Any
        none = Atto.char '!' *> pure None
        sub = Atto.char '(' *> Atto.skipSpace *> parseRange <* Atto.skipSpace <* Atto.char ')'
        s =
            unAnyRange
                . foldMap AnyRange
                <$> ((p <|> sub) `Atto.sepBy1` (Atto.skipSpace *> Atto.string "||" <* Atto.skipSpace))
        p = unAllRange . foldMap AllRange <$> ((a <|> sub) `Atto.sepBy1` Atto.space)
        a = liftA2 Anchor parseOperator parseVersion <|> caret <|> tilde <|> wildcard <|> hyphen <|> any <|> none


-- >>> liftA2 satisfies (Atto.parseOnly parseVersion "0.20.1.1") (Atto.parseOnly parseRange "^0.20.1")
-- Right True
caret :: Atto.Parser VersionRange
caret =
    (Atto.char '^' *> parseVersion) <&> \case
        v@(Version (0, 0, 0, _)) -> Anchor (Right EQ) v
        v@(Version (0, 0, z, _)) -> rangeIE v (Version (0, 0, z + 1, 0))
        v@(Version (0, y, _, _)) -> rangeIE v (Version (0, y + 1, 0, 0))
        v@(Version (x, _, _, _)) -> rangeIE v (Version (x + 1, 0, 0, 0))


-- >>> Atto.parseOnly tilde "~1.2.3.4"
-- Right >=1.2.3.4 <1.2.4
tilde :: Atto.Parser VersionRange
tilde =
    (Atto.char '~' *> (Atto.decimal `Atto.sepBy1` Atto.char '.')) >>= \case
        [x, y, z, q] -> pure $ rangeIE (Version (x, y, z, q)) (Version (x, y, z + 1, 0))
        [x, y, z] -> pure $ rangeIE (Version (x, y, z, 0)) (Version (x, y + 1, 0, 0))
        [x, y] -> pure $ rangeIE (Version (x, y, 0, 0)) (Version (x, y + 1, 0, 0))
        [x] -> pure $ rangeIE (Version (x, 0, 0, 0)) (Version (x + 1, 0, 0, 0))
        o -> fail $ "Invalid number of version numbers: " <> show (length o)


range :: Bool -> Bool -> Version -> Version -> VersionRange
range inc0 inc1 v0 v1 =
    let lo = if inc0 then Left LT else Right GT
        hi = if inc1 then Left GT else Right LT
     in Conj (Anchor lo v0) (Anchor hi v1)


rangeIE :: Version -> Version -> VersionRange
rangeIE = range True False


-- >>> Atto.parseOnly wildcard "1.2.3.x"
-- Right >=1.2.3 <1.2.4
wildcard :: Atto.Parser VersionRange
wildcard =
    (Atto.many1 (Atto.decimal <* Atto.char '.') <* Atto.char 'x') >>= \case
        [x, y, z] -> pure $ rangeIE (Version (x, y, z, 0)) (Version (x, y, z + 1, 0))
        [x, y] -> pure $ rangeIE (Version (x, y, 0, 0)) (Version (x, y + 1, 0, 0))
        [x] -> pure $ rangeIE (Version (x, 0, 0, 0)) (Version (x + 1, 0, 0, 0))
        o -> fail $ "Invalid number of version numbers: " <> show (length o)


-- >>> Atto.parseOnly hyphen "0.1.2.3 - 1.2.3.4"
-- Right >=0.1.2.3 <=1.2.3.4
hyphen :: Atto.Parser VersionRange
hyphen = liftA2 (range True True) parseVersion (Atto.skipSpace *> Atto.char '-' *> Atto.skipSpace *> parseVersion)
