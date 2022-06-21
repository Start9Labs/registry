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
    (&),
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
import Debug.Trace (trace)
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
beforeAfter before after = trace [i|BEFORE: #{before} ===> AFTER: #{after}|] after


leftRight :: Show a => (a, a) -> ((a, a) -> a) -> a
leftRight (l, r) f = let after = f (l, r) in trace [i|BEFORE: #{l}, #{r} ===> AFTER: #{after}|] after


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
reduce orig@(Conj x y) =
    (reduce x, reduce y) & \case
        -- conj units
        (Any, vr) -> vr
        (vr, Any) -> vr
        -- conj annihilators
        (None, _) -> None
        (_, None) -> None
        -- primitive conjunction reduction
        (a@(Anchor (Right EQ) pt), y') -> if pt <|| y' then a else None
        (x', b@(Anchor (Right EQ) pt)) -> if pt <|| x' then b else None
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
                (False, True, False) -> reduce $ Anchor (Right $ complement (primOrd op) (primOrd op')) pt
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
                            (Left LT, Right LT) -> Anchor eq pt
                            (Right GT, Left GT) -> Anchor eq pt'
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
                        GT -> conj b (conj a at)
                -- this means that b is a bound and so we put a with the rest of the x's
                (True, False) -> case compare pta ptb of
                    LT -> if fl opb then conj b (conj a at) else y'
                    EQ -> conj (conj a b) at
                    GT -> if fr opb then conj b (conj a at) else y'
                -- this means that the conj tree is all neqs and so when a is a bound we give it the bound position
                (False, True) -> case compare pta ptb of
                    LT -> if fr opa then Conj a y' else a
                    EQ -> conj (conj a b) at
                    GT ->
                        if fl opa
                            then case conj a at of
                                a'@(Anchor _ _) -> conj a' b
                                (Conj l@(Anchor opl _) r@(Anchor opr _))
                                    | opl /= neq -> Conj l (conj b r)
                                    | opr /= neq -> Conj r (conj b l)
                                    | otherwise -> error "bug"
                                (Conj a'@(Anchor _ _) at'@(Conj _ _)) -> Conj a' (conj b at')
                                r -> error [i|impossible reduction: #{r}|]
                            else conj a at
                -- a and b are the bounds and we leave the x tree alone
                (False, False) -> conj (conj a b) at
        -- insert anchor into conj tree that has two bounds and a x tree
        (a@(Anchor opa pta), y'@(Conj bounds@(Conj b@(Anchor _ ptb) c@(Anchor _ ptc)) neqs))
            -- if the anchor is an x we put it into the x tree
            | opa == neq ->
                if
                        | pta > ptb && pta < ptc -> Conj bounds (conj a neqs)
                        | pta == ptb -> conj (conj (conj a b) c) neqs
                        | pta == ptc -> conj (conj b (conj a c)) neqs
                        | otherwise -> y'
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
                (False, False, True, True)
                    | ptc <|| x' && ptd <|| x' -> Conj x' y'
                    | ptc <|| x' -> conj x' c
                    | ptd <|| x' -> conj x' d
                    | otherwise -> x'
                (True, True, True, False) -> conj d (conj c x')
                (True, True, False, True) -> conj c (conj d x')
                (True, False, True, True) -> conj b (conj a y')
                (False, True, True, True) -> conj a (conj b y')
                (True, True, True, True) -> conj a (conj b y')
        -- insert irreducible conj pair into single bounded conj tree OR x tree
        (x'@(Conj a@(Anchor opa pta) b@(Anchor opb ptb)), y'@(Conj c@(Anchor opc ptc) r))
            | opa /= neq && opb /= neq && opc == neq -> case (compare ptc pta, compare ptc ptb) of
                (LT, LT) -> if fl opa then conj (conj c x') r else conj x' r
                (EQ, LT) -> conj (conj (conj a c) b) r
                (GT, LT) -> conj (conj (conj a c) b) r
                (GT, EQ) -> conj (conj a (conj b c)) r
                (GT, GT) -> if fr opb then conj (conj x' c) r else conj x' r
                _ -> error "bug"
            | otherwise -> conj a (conj b y')
        (x'@(Conj (Anchor _ _) _), y'@(Conj (Anchor _ _) (Anchor _ _))) -> conj y' x'
        -- insert irreducible conj pair into double bounded conj tree
        (Conj a@(Anchor _ _) b@(Anchor _ _), y'@(Conj (Conj (Anchor _ _) (Anchor _ _)) _)) -> conj a (conj b y')
        (x'@(Conj (Conj (Anchor _ _) (Anchor _ _)) _), y'@(Conj (Anchor _ _) (Anchor _ _))) -> conj y' x'
        -- x tree, x tree
        (Conj a@(Anchor (Left EQ) pta) ra, Conj b@(Anchor (Left EQ) ptb) rb) ->
            let r = conj ra rb
             in case compare pta ptb of
                    LT -> Conj a (Conj b r)
                    EQ -> Conj a r
                    GT -> Conj b (Conj a r)
        -- x tree, single bounded conj tree
        (x'@(Conj (Anchor (Left EQ) _) (Conj _ _)), Conj b@(Anchor _ _) rb@(Conj _ _)) -> conj b (conj x' rb)
        -- single bounded conj tree, x tree
        (Conj a@(Anchor _ _) ra@(Conj _ _), y'@(Conj (Anchor (Left EQ) _) (Conj _ _))) -> conj a (conj y' ra)
        -- x tree, double bounded conj tree
        (Conj a@(Anchor (Left EQ) _) ra@(Conj _ _), Conj yBounds@(Conj (Anchor _ _) (Anchor _ _)) yNeqs) -> conj ra (conj yBounds (conj a yNeqs))
        -- double bounded conj tree, x tree
        (Conj xBounds@(Conj (Anchor _ _) (Anchor _ _)) xNeqs, Conj b@(Anchor (Left EQ) _) rb@(Conj _ _)) -> conj rb (conj xBounds (conj b xNeqs))
        -- double bounded conj tree, double bounded conj tree
        (Conj xBounds@(Conj (Anchor _ _) (Anchor _ _)) xNeqs, Conj yBounds@(Conj (Anchor _ _) (Anchor _ _)) yNeqs) ->
            conj (conj xBounds yBounds) (conj xNeqs yNeqs)
        (Conj xBound@(Anchor _ _) xNeqs@(Conj _ _), Conj yBounds@(Conj (Anchor _ _) (Anchor _ _)) yNeqs) ->
            conj (conj xBound yBounds) (conj xNeqs yNeqs)
        (Conj xBounds@(Conj (Anchor _ _) (Anchor _ _)) xNeqs, Conj yBound@(Anchor _ _) yNeqs@(Conj _ _)) ->
            conj (conj xBounds yBound) (conj xNeqs yNeqs)
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
reduce orig@(Disj x y) =
    (reduce x, reduce y) & \case
        -- disj annihilators
        (Any, _) -> Any
        (_, Any) -> Any
        -- disj units
        (None, vr) -> vr
        (vr, None) -> vr
        -- primitive disj reduction
        (a@(Anchor (Left EQ) pt), y') -> if pt <|| y' then Any else a
        (x', b@(Anchor (Left EQ) pt)) -> if pt <|| x' then Any else b
        (a@(Anchor op pt), b@(Anchor op' pt')) -> case compare pt pt' of
            GT -> disj b a
            EQ -> case (isRight op, isRight op == isRight op', primOrd op == primOrd op') of
                -- idempotence
                (_, True, True) -> a
                -- complement
                (_, False, True) -> Any
                -- union these sets
                (True, True, False) -> reduce $ Anchor (Left $ complement (primOrd op) (primOrd op')) pt
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
        -- anchor, irreducible disj pair
        (a@(Anchor opa pta), y'@(Disj b@(Anchor opb ptb) c@(Anchor opc ptc))) -> case (compare pta ptb, compare pta ptc) of
            -- eq patterns reduce so prioritize disj'ing those
            (EQ, LT) -> disj (disj a b) c
            (GT, EQ) -> disj b (disj a c)
            -- here we are to the right of an irreducible conj, so we try to reduce with the right side
            (GT, GT) -> case disj c a of
                -- cascade if successful
                Any -> Any
                None -> b
                dca@(Anchor _ _) -> disj b dca
                -- otherwise just assemble to list
                Disj _ _ -> Disj b (Disj c a)
                _ -> error "bug"
            -- here we are to the left of an irreducible conj, so we try to reduce the left side
            (LT, LT) -> case disj a b of
                Any -> Any
                None -> c
                -- cascade if successful
                dab@(Anchor _ _) -> disj dab c
                -- otherwise we move os out right
                -- we know for sure that opb is o
                _ -> Disj a y'
            (GT, LT)
                -- >o< >oo oo< ooo all irreducible
                | opa == eq -> disj b (Disj a c)
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
        -- irreducible disj pair, anchor
        (x'@(Disj a@(Anchor _ pta) b@(Anchor _ ptb)), c@(Anchor _ ptc))
            | ptc > pta && ptc > ptb -> disj a (disj b c)
            | otherwise -> disj c x'
        -- anchor, irreducible conj pair
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
                (EQ, _) -> conj (disj a b) (disj a c)
                (_, EQ) -> conj (disj a b) (disj a c)
                (LT, GT) -> error [i|bug in anchor order normalization: #{orig} -> #{a}, #{b}, #{c}|]
        -- irreducible conj pair, anchor
        (x'@(Conj (Anchor _ _) (Anchor opb ptb)), c@(Anchor opc ptc))
            | ptc > ptb && not (fl opc) && not (fr opb) -> Disj x' c
            | otherwise -> disj c x'
        -- anchor, x tree
        (a@(Anchor _ _), Conj b@(Anchor (Left EQ) ptb) r@(Conj _ _)) ->
            if ptb <|| a
                then disj a r
                else conj b (disj a r)
        -- x tree, anchor
        (x'@(Conj (Anchor (Left EQ) _) (Conj _ _)), a@(Anchor _ _)) -> disj a x'
        -- anchor, single bounded conj tree
        (a@(Anchor _ pta), y'@(Conj b@(Anchor _ ptb) r@(Conj _ _))) -> case disj a b of
            Any -> disj a r
            dab@(Anchor _ _) -> conj dab (disj a r)
            Disj _ _ -> if pta < ptb then Disj a y' else Disj y' a
            _ -> error [i|bug in single bounded conj tree normalization|]
        -- single bounded conj tree, anchor
        (x'@(Conj (Anchor opa pta) (Conj _ _)), b@(Anchor opb ptb))
            | ptb > pta && fr opb && not (fl opb) && fl opa && not (fr opa) -> Disj x' b
            | otherwise -> disj b x'
        -- anchor, double bounded conj tree
        (x'@(Anchor _ _), y'@(Conj bounds@(Conj (Anchor _ _) (Anchor _ _)) neqs)) -> case disj x' bounds of
            dx'bounds@(Anchor _ _) -> conj dx'bounds (disj x' neqs)
            dx'bounds@(Conj _ _) -> conj dx'bounds (disj x' neqs)
            Disj (Anchor _ _) (Conj (Anchor _ _) (Anchor _ _)) -> Disj x' y'
            Disj (Conj (Anchor _ _) (Anchor _ _)) (Anchor _ _) -> Disj y' x'
            _ -> error "bug"
        -- double bounded conj tree, anchor
        (x'@(Conj (Conj (Anchor _ _) (Anchor opb ptb)) _), y'@(Anchor opc ptc))
            | ptc > ptb && not (fl opc) && not (fr opb) -> Disj x' y'
            | otherwise -> disj y' x'
        -- anchor, disj tree with conj head
        (x'@(Anchor _ _), Disj ct@(Conj _ _) r) -> case disj x' ct of
            Any -> Any
            dx'ct@(Anchor _ _) -> disj dx'ct r
            dx'ct@(Conj _ _) -> disj dx'ct r
            Disj x''@(Anchor opa pta) ct'@(Conj (Anchor opb ptb) (Anchor _ _))
                | pta < ptb && not (fl opb) && not (fr opa) -> Disj x'' (disj ct' r)
                | otherwise -> error "bug"
            Disj x''@(Anchor opa pta) ct'@(Conj (Anchor opb ptb) (Conj _ _))
                | pta < ptb && not (fl opb) && not (fr opa) -> Disj x'' (disj ct' r)
                | otherwise -> error "bug"
            Disj x''@(Anchor opa pta) ct'@(Conj (Conj (Anchor opb ptb) (Anchor _ _)) _)
                | pta < ptb && not (fl opb) && not (fr opa) -> Disj x'' (disj ct' r)
                | otherwise -> error "bug"
            Disj ct'@(Conj (Anchor _ _) (Anchor opb ptb)) x''@(Anchor opc ptc)
                | ptc > ptb && not (fr opb) && not (fl opc) -> Disj ct' (disj x'' r)
                | otherwise -> error "bug"
            Disj ct'@(Conj (Anchor opa pta) (Conj _ _)) x''@(Anchor opb ptb)
                | ptb > pta && not (fr opa) && not (fl opb) -> Disj ct' (disj x'' r)
                | otherwise -> error "bug"
            Disj ct'@(Conj (Conj (Anchor _ _) (Anchor opb ptb)) _) x''@(Anchor opc ptc)
                | ptc > ptb && not (fr opb) && not (fl opc) -> Disj ct' (disj x'' r)
                | otherwise -> error "bug"
            _ -> error "bug"
        -- disj tree with conj head, anchor
        (x'@(Disj (Conj _ _) _), y'@(Anchor _ _)) -> disj y' x'
        -- conj tree, disj tree with conj head
        (x'@(Conj _ _), y'@(Disj b@(Conj _ _) r)) -> case disj x' b of
            Any -> Any
            dx'b@(Anchor _ _) -> disj dx'b r
            dx'b@(Conj _ _) -> disj dx'b r
            Disj p q
                | p == x' && q == b -> Disj x' y'
                | p == b && q == x' -> disj b (disj x' r)
                | otherwise -> error "bug"
            other -> error [i|#{other}, #{x'}, #{y'}|]
        -- disj tree with conj head, conj tree
        (x'@(Disj (Conj _ _) _), y'@(Conj _ _)) -> disj y' x'
        -- anchor, disj tree with anchor head
        (a@(Anchor _ _), y'@(Disj b@(Anchor _ _) r)) -> case disj a b of
            Disj p q
                | p == a && q == b -> Disj a y'
                | p == b && q == a -> disj b (disj a r)
            other -> disj other r
        -- disj tree with anchor head, anchor
        (x'@(Disj (Anchor _ _) _), y'@(Anchor _ _)) -> disj y' x'
        -- irreducible conj pair, irreducible conj pair
        (x'@(Conj a@(Anchor opa pta) b@(Anchor opb ptb)), y'@(Conj c@(Anchor opc ptc) d@(Anchor opd ptd))) ->
            -- we know for certain that pta < ptb and ptc < ptd
            case (fl opa, fr opb, fl opc, fr opd) of
                -- <> <>
                (False, False, False, False) -> case (compare pta ptc, compare pta ptd, compare ptb ptc, compare ptb ptd) of
                    -- a = c < b = d
                    (EQ, _, _, EQ)
                        | opa == geq && opc == gt && opd == leq && opb == lt -> Conj a d
                        | opc == geq && opa == gt && opb == leq && opd == lt -> Conj c b
                        | (opa == geq || opc == gt) && (opb == leq || opd == lt) -> x'
                        | (opc == geq || opa == gt) && (opd == leq || opb == lt) -> y'
                        | otherwise -> error "bug"
                    -- a = c < b < d
                    (EQ, _, _, LT)
                        | opa == geq && opc == gt -> Conj a d
                        | otherwise -> y'
                    -- a = c < d < b
                    (EQ, _, _, GT)
                        | opa == gt && opc == geq -> Conj c b
                        | otherwise -> x'
                    -- a < c < b = d
                    (LT, _, _, EQ)
                        | opd == leq && opb == lt -> Conj a d
                        | otherwise -> x'
                    -- c < a < b = d
                    (GT, _, _, EQ)
                        | opb == leq && opd == lt -> Conj c b
                        | otherwise -> y'
                    -- a < b = c < d
                    (_, _, EQ, _)
                        | opb == leq || opc == geq -> Conj a d
                        | otherwise -> Conj (Conj a d) (Anchor neq ptb)
                    -- c < a = d < b
                    (_, EQ, _, _)
                        | opd == leq || opa == geq -> Conj c b
                        | otherwise -> Conj (Conj c b) (Anchor neq pta)
                    -- a < b < c < d
                    (_, _, LT, _) -> Disj x' y'
                    -- c < d < a < b
                    (_, GT, _, _) -> Disj y' x'
                    -- a < c < b < d
                    (LT, _, GT, LT) -> Conj a d
                    -- c < a < d < b
                    (GT, LT, _, GT) -> Conj c b
                    -- a < c < d < b
                    (LT, _, _, GT) -> x'
                    -- c < a < b < d
                    (GT, _, _, LT) -> y'
                -- <> <x
                (False, False, False, True)
                    | ptb < ptc -> Disj x' y'
                    | ptd <|| x' -> disj x' c
                    | ptb == ptd && opb == leq -> disj a c
                    | ptb == ptd && opb == lt -> Conj (disj a c) d
                    | ptb == ptc && opb == lt && opc == gt -> Conj a (Conj (Anchor neq ptb) d)
                    | pta == ptd && opa == gt -> y'
                    | a == c -> conj a (disj b d)
                    | pta == ptc -> conj (Anchor geq pta) (disj b d)
                    | ptd < pta -> y'
                    | ptc < pta && ptb < ptd -> y'
                    | pta < ptc -> Conj a d
                    | otherwise -> error [i|#{x'}, #{y'}|]
                -- <> x>
                (False, False, True, False)
                    | ptd < pta -> Disj y' x'
                    | ptc <|| x' -> disj d x'
                    | pta == ptc && opa == geq -> disj b d
                    | pta == ptc && opa == gt -> Conj c (disj b d)
                    | pta == ptd && opa == gt && opd == lt -> Conj b (Conj c (Anchor neq pta))
                    | ptb == ptc && opb == lt -> y'
                    | b == d -> conj (disj a c) b
                    | ptb == ptd -> conj (disj a c) (Anchor leq ptd)
                    | ptb < ptc -> y'
                    | pta > ptc && ptb < ptd -> y'
                    | ptb > ptd -> Conj c b
                    | otherwise -> error [i|#{x'}, #{y'}|]
                -- <x <>
                (False, True, False, False)
                    | ptd < pta -> Disj y' x'
                    | otherwise -> disj y' x'
                -- x> <>
                (True, False, False, False)
                    | ptc > ptb -> Disj x' y'
                    | otherwise -> disj y' x'
                -- <x <x
                (False, True, False, True)
                    | ptd <|| x' -> disj c x'
                    | ptb <|| y' -> disj a y'
                    | ptb == ptd -> conj (disj a c) b
                    | otherwise -> error "bug"
                -- <x x>
                (False, True, True, False)
                    | ptd < pta -> Disj y' x'
                    | ptb <|| y' -> disj a y'
                    | ptc <|| x' -> disj d x'
                    | pta < ptd && ptc < pta && ptb > ptd -> Conj c b
                    | ptb == ptc -> b
                    | pta == ptd && opa == gt && opd == lt -> Conj c (Conj (Anchor neq pta) b)
                    | pta == ptd -> Conj c b
                    | pta == ptc && opa == gt -> Conj c (disj b d)
                    | ptb == ptd && opd == lt -> Conj (disj a c) b
                    | otherwise -> error [i|#{x'}, #{y'}|]
                -- x> <x
                (True, False, False, True)
                    | ptc > ptb -> Disj x' y'
                    | otherwise -> disj y' x'
                -- x> x>
                (True, False, True, False)
                    | pta <|| y' -> disj b y'
                    | ptc <|| x' -> disj d x'
                    | pta == ptc -> conj (disj b d) a
                    | otherwise -> error "bug"
                -- xx ??
                (True, True, _, _)
                    | pta <|| y' && ptb <|| y' -> Any
                    | pta <|| y' -> b
                    | ptb <|| y' -> a
                    | otherwise -> x'
                -- ?? xx
                (_, _, True, True)
                    | ptc <|| x' && ptd <|| x' -> Any
                    | ptc <|| x' -> d
                    | ptd <|| x' -> c
                    | otherwise -> y'
        -- irreducible conj pair, disj tree with anchor head
        (x'@(Conj (Anchor opa pta) (Anchor opb ptb)), y'@(Disj c@(Anchor opc ptc) r)) -> case disj c x' of
            Any -> Any
            dcx'@(Anchor _ _) -> disj dcx' r
            dcx'@(Conj _ _) -> disj dcx' r
            Disj _ _
                | ptc > ptb && not (fl opc) && not (fr opb) -> Disj x' y'
                | ptc < pta && not (fr opc) && not (fl opa) -> disj c (disj x' r)
                | otherwise -> error "bug"
            _ -> error "bug"
        -- disj tree with anchor head, irreducible conj pair
        (x'@(Disj (Anchor _ _) _), y'@(Conj (Anchor _ _) (Anchor _ _))) -> disj y' x'
        -- disj tree, disj tree
        (Disj a b, Disj c d) -> disj a (disj c (disj b d))
        (x', y'@(Disj a b)) -> case disj x' a of
            Disj p q
                | p == x' && q == a -> Disj x' y'
                | p == a && q == x' -> disj a (disj x' b)
            dx'a -> disj dx'a b
        (x'@(Disj _ _), y') -> disj y' x'
        -- x tree, x tree
        (x'@(Conj a@(Anchor (Left EQ) pta) ra@(Conj _ _)), y'@(Conj b@(Anchor (Left EQ) ptb) rb@(Conj _ _))) -> case compare pta ptb of
            LT -> if disj a rb == a then Conj a (disj ra y') else disj ra y'
            EQ -> Conj a (disj ra rb)
            GT -> if disj b ra == b then Conj b (disj x' rb) else disj x' rb
        -- single bounded conj tree, x tree
        (x'@(Conj (Anchor _ _) (Conj _ _)), y'@(Conj (Anchor (Left EQ) _) (Conj _ _))) -> disj y' x'
        -- x tree, single bounded conj tree
        (Conj a@(Anchor (Left EQ) pta) r@(Conj _ _), y'@(Conj (Anchor _ _) (Conj _ _))) ->
            if pta <|| y'
                then disj r y'
                else conj a (disj r y')
        -- -- x tree, irreducible conj pair
        (Conj a@(Anchor (Left EQ) pta) r@(Conj _ _), y'@(Conj (Anchor _ _) (Anchor _ _))) ->
            if pta <|| y'
                then disj r y'
                else conj a (disj r y')
        -- irreducible conj pair, single bounded conj tree
        (x'@(Conj (Anchor _ _) (Anchor _ _)), y'@(Conj c@(Anchor _ _) r)) -> case disj x' c of
            Disj p q
                | p == x' && q == c -> Disj x' y'
                | p == c && q == x' -> Disj y' x'
                | otherwise -> error "bug"
            dx'c -> conj dx'c (disj x' r)
        -- single bounded conj tree, irreducible conj pair
        (x'@(Conj (Anchor opa pta) _), y'@(Conj (Anchor opb ptb) (Anchor _ _)))
            | pta < ptb && not (fr opa) && not (fl opb) -> Disj x' y'
            | otherwise -> disj y' x'
        -- irreducible conj pair, double bounded conj tree
        (x'@(Conj (Anchor _ _) (Anchor _ _)), y'@(Conj bounds@(Conj _ _) r)) -> case disj x' bounds of
            Disj p q
                | p == x' && q == bounds -> Disj x' y'
                | p == bounds && q == x' -> Disj y' x'
            dx'bounds -> conj dx'bounds (disj x' r)
        -- double bounded conj tree, irreducible conj pair
        (x'@(Conj (Conj _ (Anchor _ pta)) _), y'@(Conj (Anchor opb ptb) (Anchor _ _)))
            | ptb > pta && not (fl opb) -> Disj x' y'
            | otherwise -> disj y' x'
        -- single bounded conj tree, single bounded conj tree
        (x'@(Conj xBound@(Anchor _ _) xNeqs), y'@(Conj yBound@(Anchor _ _) yNeqs)) ->
            case disj xBound yBound of
                Disj p q
                    | p == xBound && q == yBound -> Disj x' y'
                    | p == yBound && q == xBound -> Disj y' x'
                    | otherwise -> error "bug"
                dxy -> conj (conj dxy (disj xNeqs yNeqs)) (conj (disj xBound yNeqs) (disj yBound xNeqs))
        -- single bounded conj tree, double bounded conj tree
        (x'@(Conj xBound@(Anchor _ _) xNeqs), y'@(Conj yBounds@(Conj (Anchor _ _) (Anchor _ _)) yNeqs)) ->
            case disj xBound yBounds of
                Disj p q
                    | p == xBound && q == yBounds -> Disj x' y'
                    | p == yBounds && q == xBound -> Disj y' x'
                    | otherwise -> error "bug"
                dxy -> conj (conj dxy (disj xNeqs yNeqs)) (conj (disj xBound yNeqs) (disj yBounds xNeqs))
        -- double bounded conj tree, single bounded conj tree
        (x'@(Conj xBounds@(Conj (Anchor _ _) (Anchor _ _)) xNeqs), y'@(Conj yBound@(Anchor _ _) yNeqs)) ->
            case disj xBounds yBound of
                Disj p q
                    | p == xBounds && q == yBound -> Disj x' y'
                    | p == yBound && q == xBounds -> Disj y' x'
                    | otherwise -> error "bug"
                dxy -> conj (conj dxy (disj xNeqs yNeqs)) (conj (disj xBounds yNeqs) (disj yBound xNeqs))
        -- double bounded conj tree, double bounded conj tree
        (x'@(Conj xBounds@(Conj (Anchor _ _) (Anchor _ _)) xNeqs), y'@(Conj yBounds@(Conj (Anchor _ _) (Anchor _ _)) yNeqs)) ->
            case disj xBounds yBounds of
                Disj p q
                    | p == xBounds && q == yBounds -> Disj x' y'
                    | p == yBounds && q == xBounds -> Disj y' x'
                    | otherwise -> error "bug"
                dxy -> conj (conj dxy (disj xNeqs yNeqs)) (conj (disj xBounds yNeqs) (disj yBounds xNeqs))
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
-- Right (=2.3.4) (((>=1.2.3.4) (<=2.3.4.5)) ((>3.0.0) || (<3.4.5)))
-- >>> Atto.parseOnly parseRange "0.2.6"
-- >>> Atto.parseOnly parseRange ">=2.14.1.1 <3.0.0"
-- Right =0.2.6
-- Right (>=2.14.1.1) (<3.0.0)
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

-- data ConjNF = ConjNF
--     { conjNFLower :: Maybe (Version, Bool)
--     , conjNFUpper :: Maybe (Version, Bool)
--     , conjNFExclusions :: [Version]
--     }
--     deriving (Eq)

-- isValidConjNF :: ConjNF -> Bool
-- isValidConjNF = \case
--     ConjNF (Just (v0, i0)) (Just (v1, i1)) ex -> v0 < v1 && all ((> v0) <&&> (< v1)) ex
--     ConjNF Nothing (Just (v1, i1)) ex -> all (< v1) ex
--     ConjNF (Just (v0, i0)) Nothing ex -> all (> v0) ex
--     ConjNF Nothing Nothing ex -> _

-- data DisjNF = DisjNF
--     { disjNFLower :: Maybe (Version, Bool)
--     , disjNFUpper :: Maybe (Version, Bool)
--     }
