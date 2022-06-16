{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}
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
    eq,
    neq,
    lt,
    leq,
    gt,
    geq,
    fl,
    fr,
    faces,
) where

import Startlude (
    Alternative ((<|>)),
    Applicative (liftA2, pure, (*>), (<*)),
    Bool (..),
    Bounded (maxBound),
    Either (..),
    Eq (..),
    Foldable (length),
    Hashable,
    IsString (..),
    Monad ((>>=)),
    Monoid (mappend, mempty),
    NFData (..),
    Num ((+)),
    Ord (..),
    Ordering (..),
    Read,
    Semigroup ((<>)),
    Show,
    String,
    Word,
    Word64,
    either,
    empty,
    flip,
    fst,
    id,
    isRight,
    maybe,
    not,
    on,
    otherwise,
    seq,
    show,
    snd,
    toS,
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
import Data.Attoparsec.Text qualified as Atto
import Data.List.NonEmpty qualified as NE
import Data.Semigroup.Foldable (Foldable1 (foldMap1))
import Data.String.Interpolate.IsString (i)
import Data.Text qualified as T
import Debug.Trace qualified
import GHC.Base (error)
import GHC.Read qualified as GHC (
    readsPrec,
 )
import GHC.Show qualified as GHC (
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


adjacent :: Version -> Version -> Bool
adjacent a b =
    major a == major b
        && minor a == minor b
        && patch a == patch b
        && (revision a == revision b + 1 && revision b /= maxBound || revision b == revision a + 1 && revision a /= maxBound)


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


faces :: Operator -> (Bool, Bool) -- faces left right both or neither, fst is "faces left" snd is "faces right"
-- eq faces neither
faces (Right EQ) = (False, False)
-- neq faces both
faces (Left EQ) = (True, True)
-- lt/e faces left
faces (Right LT) = (True, False)
faces (Left GT) = (True, False)
-- gt/e faces right
faces (Right GT) = (False, True)
faces (Left LT) = (False, True)


fr :: Operator -> Bool
fr = snd . faces


fl :: Operator -> Bool
fl = fst . faces


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


instance Show VersionRange where
    show (Anchor (Left EQ) v) = '!' : '=' : GHC.show v
    show (Anchor (Right EQ) v) = '=' : GHC.show v
    show (Anchor (Left LT) v) = '>' : '=' : GHC.show v
    show (Anchor (Right LT) v) = '<' : GHC.show v
    show (Anchor (Left GT) v) = '<' : '=' : GHC.show v
    show (Anchor (Right GT) v) = '>' : GHC.show v
    -- show (Conj a@(Disj _ _) b@(Disj _ _)) = paren (GHC.show a) <> (' ' : paren (GHC.show b))
    -- show (Conj a@(Disj _ _) b) = paren (GHC.show a) <> (' ' : GHC.show b)
    -- show (Conj a b@(Disj _ _)) = GHC.show a <> (' ' : paren (GHC.show b))
    show (Conj a b) = paren (GHC.show a) <> (' ' : paren (GHC.show b))
    show (Disj a b) = paren (GHC.show a) <> " || " <> paren (GHC.show b)
    show Any = "*"
    show None = "!"
instance Read VersionRange where
    readsPrec _ s = case Atto.parseOnly parseRange (T.pack s) of
        Left _ -> []
        Right a -> [(a, "")]
instance IsString VersionRange where
    fromString = either error id . Atto.parseOnly parseRange . toS


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


beforeAfter :: Show a => a -> a -> a
beforeAfter before after = Debug.Trace.trace [i|BEFORE: #{before} ===> After #{after}|] after


dbg :: Show a => a -> a
dbg = show >>= Debug.Trace.trace


reduce :: VersionRange -> VersionRange
-- atomic forms
reduce Any = Any
reduce None = None
reduce vr@(Anchor op v@(Version (0, 0, 0, 0))) = case op of
    (Right LT) -> None
    (Right EQ) -> vr
    (Right GT) -> vr
    (Left LT) -> Any
    (Left EQ) -> Anchor gt v
    (Left GT) -> Anchor eq v
reduce vr@(Anchor _ _) = vr
reduce orig@(Conj x y) = case (reduce x, reduce y) of
    -- conj units
    (Any, vr) -> vr
    (vr, Any) -> vr
    -- conj annihilators
    (None, _) -> None
    (_, None) -> None
    -- primitive conjunction reduction
    (a@(Anchor op pt), b@(Anchor op' pt')) -> case compare pt pt' of
        -- conj commutes so we can make normalization order the points
        GT -> conj b a
        -- trivial cases where the points are identical
        EQ -> case (isRight op, isRight op == isRight op', primOrd op == primOrd op') of
            -- the theorems found here will elucidate what is going on
            -- https://faculty.uml.edu/klevasseur/ads/s-laws-of-set-theory.html
            -- conj idempodent law: these sets are identical
            (_, True, True) -> a
            -- conj complement law: these sets are opposites
            (_, False, True) -> None
            -- inequality incompatibility: these sets do not overlap
            (True, True, False) -> None
            -- conj absorption law (left): the left set is more specific
            (True, False, False) -> a
            -- conj absorption law (right): the right set is more specific
            (False, False, False) -> b
            -- all that is left is to intersect these sets. In every one of these cases the intersection can be expressed
            -- as exactly the ordering that is not mentioned by the other two.
            (False, True, False) -> Anchor (Right $ complement (primOrd op) (primOrd op')) pt
        -- pattern reduction, throughout you will see the following notation (primarily for visualization purposes)
        -- o: this means 'eq'
        -- >: this means 'lt' or 'lte'
        -- <: this means 'gt' or 'gte'
        -- x: this means 'neq'
        -- you may find this notation a bit odd, after all the less than and greater than signs seem backwards, you can see
        -- it more clearly by viewing them as area dividers. the wide part of the angle represents this concept of 'faces'.
        -- By this analogy, > faces left, < faces right, x faces both left and right, and o faces neither left nor right
        -- it turns out that we only care about the inner two components: the right facing component of the lesser point,
        -- and the left facing component of the greater point. Why is left as an exercise to the reader.
        LT -> case (fr op, fl op') of
            -- Annihilator patterns: oo, ><, o<, >o
            (False, False) -> None
            -- Left specific patterns: ox, o>, >x, >>
            (False, True) -> a
            -- Right specific patterns: xo, <o, x<, <<
            (True, False) -> b
            -- Irreducible patterns: <>, <x, x>, xx
            (True, True) ->
                if adjacent pt pt'
                    then -- here we have some weird edge cases if versions are immediately adjacent
                    case (op, op') of
                        -- <>
                        (Right GT, Right LT) -> None
                        (Left LT, Right LT) -> a
                        (Right GT, Left GT) -> b
                        (Left LT, Left GT) -> Conj a b
                        -- x>
                        (Left EQ, Right LT) -> Anchor lt pt
                        (Left EQ, Left GT) -> Conj a b
                        -- <x
                        (Right GT, Left EQ) -> Anchor gt pt'
                        (Left LT, Left EQ) -> Conj a b
                        -- xx
                        (Left EQ, Left EQ) -> Conj a b
                        _ -> error [i|impossible reduction (anchor, anchor)|]
                    else Conj a b
    -- insert anchor into irreducible conj pair
    (a@(Anchor opa pta), y'@(Conj b@(Anchor opb ptb) c@(Anchor opc ptc)))
        | opa == neq && opb == neq && opc == neq && pta < ptb && ptb < ptc -> Conj a y'
        | otherwise ->
            case (compare pta ptb, compare pta ptc) of
                -- eq patterns reduce so prioritize conj'ing those
                (EQ, LT) -> conj (conj a b) c
                (GT, EQ) -> conj b (conj a c)
                -- here we are to the right of an irreducible conj, so we try to reduce with the right side
                (GT, GT) -> case conj c a of
                    -- cascade if successful
                    None -> None
                    Any -> b
                    cca@(Anchor _ _) -> conj b cca
                    -- otherwise we move xs out right
                    -- we know for sure at this point that the opc is x
                    _ -> case (fl opb, fr opa) of
                        (True, True) -> Conj b (Conj c a) -- xxx: x (xx)
                        (True, False) -> Conj a (Conj b c) -- xx>: > (xx)
                        (False, True) -> Conj b (Conj c a) -- <xx: < (xx)
                        (False, False) -> Conj (Conj b a) c -- <x>: (<>) x
                        -- here we are to the left of an irreducible conj, so we try to reduce the left side
                (LT, LT) -> case conj a b of
                    -- cascade if successful
                    None -> None
                    Any -> c
                    cab@(Anchor _ _) -> conj cab c
                    -- otherwise we move xs out right
                    -- we know for sure that opb is x
                    _ -> case (fl opa, fr opc) of
                        (True, True) -> Conj a (Conj b c) -- xxx: x (xx)
                        (True, False) -> Conj c (Conj a b) -- xx>: > (xx)
                        (False, True) -> Conj a (Conj b c) -- <xx: < (xx)
                        (False, False) -> Conj (Conj a c) b -- <x>: (<>) x
                        -- here we are in the middle of an irreducible conj
                (GT, LT)
                    -- <x> <xx xx> xxx all irreducible
                    | opa == neq -> conj b (Conj a c)
                    -- if there is a remaining left face component it will reduce with the right side
                    | fl opa -> conj b (conj a c)
                    -- corollary
                    | fr opa -> conj (conj b a) c
                    -- only remaining case is eq, which subsumes both sides
                    | otherwise -> a
                -- impossible because all anchors of equal versions reduce
                (EQ, EQ) -> error [i|bug in equal anchor version reduction: #{orig} -> #{a}, #{b}, #{c}|]
                -- ordinarily we reorder things so the lesser point is on the left
                -- the only exception to this is the accumulation of x's on the right
                -- so these cases should be impossible
                _ -> error $ [i|bug in anchor order normalization: #{orig} -> #{a}, #{b}, #{c}|]
    (x'@(Conj (Anchor opa pta) (Anchor opb ptb)), c@(Anchor opc ptc))
        -- in this case it is a fully reduced conj form with lower and upper bounds and a single x
        | opc == neq && opa /= neq && opb /= neq && ptc > pta && ptc < ptb -> Conj x' c
        -- isn't in normal form and we will attempt to do so by swapping args and reducing to hit the case above
        | otherwise -> conj c x'
    (x'@(Conj _ _), y'@(Anchor _ _)) -> conj y' x'
    -- insert anchor into conj tree with a single bounded conj tree or x tree
    (a@(Anchor opa pta), y'@(Conj b@(Anchor opb ptb) at@(Conj _ _))) ->
        case (opa == neq, opb == neq) of
            -- this means that the conj tree is all x's
            (True, True) ->
                case compare pta ptb of
                    LT -> Conj a y'
                    EQ -> y'
                    GT -> Conj b (Conj a at)
            -- this means that b is a bound and so we put a with the rest of the x's
            (True, False) ->
                if fl opb && pta < ptb || fr opb && pta > ptb
                    then conj b (conj a at)
                    else y'
            -- this means that the conj tree is all neqs and so when a is a bound we give it the bound position
            (False, True) -> case compare pta ptb of
                LT -> if fr opa then Conj a y' else a
                EQ -> conj (conj a b) at
                GT ->
                    if fl opa
                        then case conj a at of
                            a'@(Anchor _ _) -> conj a' b
                            (Conj a' at') -> Conj a' (Conj b at')
                            r -> error [i|impossible reduction: #{r}|]
                        else conj a at
            -- a and b are the bounds and we leave the x tree alone
            (False, False) -> conj (conj a b) at
    -- insert anchor into conj tree that has two bounds and a x tree
    (a@(Anchor opa pta), y'@(Conj bounds@(Conj (Anchor _ ptb) (Anchor _ ptc)) neqs))
        -- if the anchor is an x we put it into the x tree
        | opa == neq -> if pta > ptb && pta < ptc then Conj bounds (conj a neqs) else y'
        -- if not we smash it in with the bounds
        | otherwise -> conj (conj a bounds) neqs
    -- zip two irreducible conj pairs together
    (x'@(Conj a@(Anchor opa pta) b@(Anchor opb ptb)), y'@(Conj c@(Anchor opc ptc) d@(Anchor opd ptd)))
        -- we know for sure that a < b and c < d here
        | opa /= neq && opb /= neq && opc == neq && opd == neq && ptc > pta && ptc < ptb && ptd < ptb -> Conj x' y'
        | otherwise -> case (opa == neq, opb == neq, opc == neq, opd == neq) of
            (False, False, False, False) -> conj (conj a c) (conj b d)
            (False, _, False, _) -> conj (conj a c) (conj b d)
            (_, False, _, False) -> conj (conj a c) (conj b d)
            (True, True, False, False) -> conj y' x'
            (False, True, True, False) -> conj (conj a d) (Conj b c)
            (True, False, False, True) -> conj (conj b c) (Conj a d)
            (False, False, True, True) -> case (ptc > pta && ptc < ptb, ptd > pta && ptd < ptb) of
                (True, True) -> Conj x' y'
                (True, False) -> Conj x' c
                (False, True) -> Conj x' d
                (False, False) -> x'
            (True, True, True, False) -> Conj d (conj c x')
            (True, True, False, True) -> Conj c (conj d x')
            (True, False, True, True) -> Conj b (conj a y')
            (False, True, True, True) -> Conj a (conj b y')
            (True, True, True, True) -> conj a (conj b y')
    -- insert irreducible conj pair into single bounded conj tree OR x tree
    (x'@(Conj a@(Anchor opa pta) b@(Anchor opb ptb)), y'@(Conj c@(Anchor opc ptc) r))
        | opa /= neq && opb /= neq && opc == neq ->
            if ptc > pta && ptc < ptb
                then conj c (conj x' r)
                else conj x' r
        | otherwise -> conj a (conj b y')
    (x'@(Conj (Anchor _ _) _), y'@(Conj (Anchor _ _) (Anchor _ _))) -> conj y' x'
    -- insert irreducible conj pair into double bounded conj tree
    (Conj a@(Anchor _ _) b@(Anchor _ _), y'@(Conj (Conj (Anchor _ _) (Anchor _ _)) _)) -> conj a (conj b y')
    (x'@(Conj (Conj (Anchor _ _) (Anchor _ _)) _), y'@(Conj (Anchor _ _) (Anchor _ _))) -> conj y' x'
    -- zip two conj trees
    (Conj xBounds@(Conj (Anchor _ _) (Anchor _ _)) xNeqs, Conj yBounds@(Conj (Anchor _ _) (Anchor _ _)) yNeqs) ->
        conj (conj xBounds yBounds) (conj xNeqs yNeqs)
    (Conj xBounds@(Anchor _ _) xNeqs@(Conj _ _), Conj yBounds@(Conj (Anchor _ _) (Anchor _ _)) yNeqs) ->
        conj (conj xBounds yBounds) (conj xNeqs yNeqs)
    (Conj xBounds@(Conj (Anchor _ _) (Anchor _ _)) xNeqs, Conj yBounds@(Anchor _ _) yNeqs@(Conj _ _)) ->
        conj (conj xBounds yBounds) (conj xNeqs yNeqs)
    (Conj xBounds@(Anchor _ _) xNeqs@(Conj _ _), Conj yBounds@(Anchor _ _) yNeqs@(Conj _ _)) ->
        conj (conj xBounds yBounds) (conj xNeqs yNeqs)
    -- distribute right
    (x', Disj p q) -> disj (conj x' p) (conj x' q)
    -- distribute left
    (Disj p q, y') -> disj (conj p y') (conj q y')
    -- bugs
    (Conj (Disj _ _) _, _) -> [i|bug in distributing conj over disj|]
    (Conj _ (Disj _ _), _) -> [i|bug in distributing conj over disj|]
    (_, Conj (Disj _ _) _) -> [i|bug in distributing conj over disj|]
    (_, Conj _ (Disj _ _)) -> [i|bug in distributing conj over disj|]
    (Conj Any _, _) -> [i|bug in conj any unit|]
    (Conj _ Any, _) -> [i|bug in conj any unit|]
    (_, Conj Any _) -> [i|bug in conj any unit|]
    (_, Conj _ Any) -> [i|bug in conj any unit|]
    (Conj None _, _) -> [i|bug in conj none annihilation|]
    (Conj _ None, _) -> [i|bug in conj none annihilation|]
    (_, Conj None _) -> [i|bug in conj none annihilation|]
    (_, Conj _ None) -> [i|bug in conj none annihilation|]
    (x', y') -> error [i|missing conj case: #{orig} -> #{x'}, #{y'}|]
reduce orig@(Disj x y) = case (reduce x, reduce y) of
    -- disj annihilators
    (Any, _) -> Any
    (_, Any) -> Any
    -- disj units
    (None, vr) -> vr
    (vr, None) -> vr
    -- primitive disj reduction
    (a@(Anchor op pt), b@(Anchor op' pt')) -> case compare pt pt' of
        GT -> disj b a
        EQ -> case (isRight op, isRight op == isRight op', primOrd op == primOrd op') of
            -- idempotence
            (_, True, True) -> a
            -- complement
            (_, False, True) -> Any
            -- union these sets
            (True, True, False) -> Anchor (Left $ complement (primOrd op) (primOrd op')) pt
            -- disj absorption left: the left set is more general
            (False, False, False) -> a
            -- disj absorption right: the right set is more general
            (True, False, False) -> b
            -- inequality hypercompatibility: these sets are universal
            (False, True, False) -> Any
        LT -> case (fr op, fl op') of
            -- Annihilator patterns: <>, <x, x>, xx
            (True, True) -> Any
            -- Left general patterns: x<, xo, <o, <<
            (True, False) -> a
            -- Right general patterns: >x, ox, o>, >>
            (False, True) -> b
            -- Irreducible patterns: >< >o o< oo
            (False, False) -> Disj a b
    (a@(Anchor opa pta), y'@(Disj b@(Anchor opb ptb) c@(Anchor opc ptc))) -> case (compare pta ptb, compare pta ptc) of
        -- eq patterns reduce so prioritize disj'ing those
        (EQ, LT) -> disj (disj a b) c
        (GT, EQ) -> disj b (disj a c)
        -- here we are to the right of an irreducible conj, so we try to reduce with the right side
        (GT, GT) -> case disj c a of
            -- cascade if successful
            dca@(Anchor _ _) -> disj b dca
            -- otherwise we move o's out right
            -- we know for sure at this point that the opc is o
            _ -> case (fl opb, fr opa) of
                (True, True) -> Disj (Disj b a) c -- >o<: (><) o
                (True, False) -> Disj b (Disj c a) -- >oo: > (oo)
                (False, True) -> Disj a (Disj b c) -- oo<: < (oo)
                (False, False) -> Disj b (Disj c a) -- ooo: o (oo)
                -- here we are to the left of an irreducible conj, so we try to reduce the left side
        (LT, LT) -> case disj a b of
            --     -- cascade if successful
            dab@(Anchor _ _) -> disj dab c
            -- otherwise we move xs out right
            -- we know for sure that opb is o
            _ -> case (fl opa, fr opc) of
                (True, True) -> Disj (Disj a c) b -- >o<: (><) o
                (True, False) -> Disj a (Disj b c) -- >oo: > (oo)
                (False, True) -> Disj c (Disj a b) -- oo<: < (oo)
                (False, False) -> Disj a (Disj b c) -- ooo: o (oo)
                -- here we are in the middle of an irreducible conj
        (GT, LT)
            -- >o< >oo oo< ooo all irreducible
            | opa == eq -> Disj b (Disj a c)
            -- if there is a remaining left face component it will reduce with the left side
            | fl opa -> disj (disj b a) c
            -- corollary
            | fr opa -> disj b (disj a c)
            -- only remaining case is neq, which subsumes both sides
            | otherwise -> a
        -- impossible because all anchors of equal versions reduce
        (EQ, EQ) -> error [i|bug in equal anchor version reduction: #{orig} -> #{a}, #{b}, #{c}|]
        -- ordinarily we reorder things so the lesser point is on the left
        -- the only exception to this is the accumulation of x's on the right
        -- so these cases should be impossible
        _
            | opb == eq && opc == eq -> Disj a y'
            | otherwise -> error $ [i|bug in anchor order normalization: #{orig} -> #{a}, #{b}, #{c}|]
    (x'@(Disj a@(Anchor _ pta) b@(Anchor _ ptb)), c@(Anchor _ ptc))
        | ptc > pta && ptc > ptb -> disj a (disj b c)
        | otherwise -> disj c x'
    (a@(Anchor opa pta), y'@(Conj b@(Anchor opb ptb) c@(Anchor opc ptc))) ->
        case (compare pta ptb, compare pta ptc) of
            (GT, GT) -> case (fr opc, fl opa) of
                -- <> / x> || o / <
                (False, False) -> Disj y' a
                -- <> / x> || x / >
                (False, True) -> a
                -- <x / xx || o / <
                (True, False) -> y'
                -- <x / xx || x / >
                (True, True) -> Any
            (LT, LT) -> case (fr opa, fl opb) of
                -- o / > || <> / <x
                (False, False) -> Disj a y'
                -- o / > || x> / xx
                (False, True) -> y'
                -- < / x || <> / <x
                (True, False) -> a
                -- < / x || x> / xx
                (True, True) -> Any
            (GT, LT) -> case (fl opa, fr opa) of
                (True, True) -> Any
                (True, False) -> c
                (False, True) -> b
                (False, False) -> y'
            (EQ, EQ) -> error [i|bug in equal anchor version reduction: #{orig} -> #{a}, #{b}, #{c}|]
            (EQ, _) -> conj (conj a b) c
            (_, EQ) -> conj b (conj a c)
            (LT, GT) -> error [i|bug in anchor order normalization: #{orig} -> #{a}, #{b}, #{c}|]
    (x'@(Conj (Anchor _ _) (Anchor _ ptb)), c@(Anchor opc ptc))
        | ptc > ptb && not (fl opc) -> Disj x' c
        | otherwise -> disj c x'
    (x'@(Disj a@(Anchor _ pta) b), c@(Anchor _ ptc)) -> case disj c a of
        dca@(Anchor _ _) -> disj dca b
        _ -> if ptc < pta then Disj c x' else disj a (disj c b)
    (Conj _ _, Conj _ _) -> error "Disj (Conj _ _) (Conj _ _)"
    (Conj _ _, _) -> error "Disj (Conj _ _) _"
    (_, Conj _ _) -> error "Disj _ (Conj _ _)"
    (Disj _ _, Disj _ _) -> error "Disj (Disj _ _) (Disj _ _)"
    (x', y') -> error [i|missing disj case: #{orig} -> #{x'}, #{y'}|]


exactly :: Version -> VersionRange
exactly = Anchor (Right EQ)


paren :: String -> String
paren = mappend "(" . flip mappend ")"


newtype AnyRange = AnyRange {unAnyRange :: VersionRange}
instance Semigroup AnyRange where
    (<>) = AnyRange <<$>> Disj `on` unAnyRange
instance Monoid AnyRange where
    mempty = AnyRange None


newtype AllRange = AllRange {unAllRange :: VersionRange}
instance Semigroup AllRange where
    (<>) = AllRange <<$>> Conj `on` unAllRange
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
-- Right =2.3.4 >=1.2.3.4 <=2.3.4.5 (>3.0.0 * || <3.4.5 * || !) * || !
-- >>> Atto.parseOnly parseRange "0.2.6"
-- >>> Atto.parseOnly parseRange ">=2.14.1.1 <3.0.0"
-- Right =0.2.6
-- Right >=2.14.1.1 <3.0.0 * || !
parseRange :: Atto.Parser VersionRange
parseRange = s <|> any <|> none <|> (Anchor (Right EQ) <$> parseVersion)
    where
        any = Atto.char '*' *> pure Any
        none = Atto.char '!' *> pure None
        sub = Atto.char '(' *> Atto.skipSpace *> parseRange <* Atto.skipSpace <* Atto.char ')'
        s = do
            exprs <- ((p <|> sub) `Atto.sepBy1` (Atto.skipSpace *> Atto.string "||" <* Atto.skipSpace))
            ne <- maybe empty pure $ NE.nonEmpty exprs
            pure . unAnyRange . foldMap1 AnyRange $ ne
        p = do
            exprs <- ((a <|> sub) `Atto.sepBy1` Atto.space)
            ne <- maybe empty pure $ NE.nonEmpty exprs
            pure . unAllRange . foldMap1 AllRange $ ne
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
