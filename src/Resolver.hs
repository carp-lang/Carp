{-# LANGUAGE TupleSections #-}

-- | The Env and Context modules provide mechanisms for finding symbols.
-- Resolvers specify the *order in which* such lookups should be performed, and
-- how lookups should be chained in the case of failure.
--
-- Resolvers are combined using their Semigroup instance, for example:
--
--   topLevelResolver <> localDynamicResolver
--
-- produces a resolver that first attempts to find a symbol at the
-- global top level, then attempts to find the symbol (by name only) in the
-- Dynamic module.
--
-- Resolvers have default orders. In the case above, the localDynamicResolver is
-- of lower order than topLevelResolver, so it will be tried only if
-- topLevelResolver fails to resolve the symbol.
--
-- One can always tweak the order by setting the order of a resolver explicitly:
--
--   topLevelResolver {order = Lower } <> localDynamicResolver {order = Higher}
--
-- will result in a resolver that first applies the localDynamicResolver, then,
-- if it fails will apply the topLevelResolver. The semigroup instance combines
-- resolvers left to right unless the order of the right argument is higher than
-- the left. In the case of equivalent orders, the left will be applied first:
--
--   resolver(higher) <> resolver'(lower) => resolver followed by resolver'
--   resolver(lower) <> resolver'(lower) => resolver followed by resolver'
--   resolver(higher) <> resolver'(higher) => resolver followed by resolver'
--   resolver(lower) <> resolver'(higher) => resolver' followed by resolver
--
-- If you need to debug resolvers, thier show instance prints a string depicting
-- the order in which they were run, e.g.:
--
--   TopLevelReolver -> "LocalDynamicResolver"
module Resolver where

import Obj
import SymPath
import qualified Set as Set
import qualified Env as E
import Control.Applicative
import Control.Monad
import Data.List(intercalate)
import Util
import Context

--------------------------------------------------------------------------------
-- Data

-- | Determines the order in which Resolvers should be chained.
data LookupOrder = Higher | Lower
  deriving(Eq)

instance Ord LookupOrder where
  compare Higher Lower = GT
  compare Lower Higher = LT
  compare Lower Lower = EQ
  compare Higher Higher = EQ

-- | Specifies whether we're resolving all or only dynamic symbols.
data ResolveMode
  = ResolveStatic
  | ResolveDynamic
  deriving(Show)

-- | Specifies how a symbol should be resolved in a given context.
data Resolver = Resolver {
  resolverName :: String,
  order :: LookupOrder,
  resolve :: SymPath -> Context -> Maybe (Context, Binder),
  resolverStack :: [String]
}

-- | Specifies how a Resolver should traverse environments.
data LookupConstraint
  = Direct
  | Children
  | Full

instance Semigroup Resolver where
  resolver <> resolver' =
    if (order resolver) >= (order resolver')
      then resolver {
        resolve = \s c -> (resolve resolver) s c <|> (resolve resolver') s c,
        resolverStack = (resolverStack resolver) ++ (resolverStack resolver')
      }
      else resolver {
        resolve = \s c -> (resolve resolver') s c <|> (resolve resolver) s c,
        resolverStack = (resolverStack resolver') ++ (resolverStack resolver)
      }

instance Show Resolver where
  show Resolver{resolverStack = s} = intercalate "-> " s

-- | Applies a resolver to find a symbols corresponding binder.
applyResolver :: Resolver -> SymPath -> Context -> Maybe (Context, Binder)
applyResolver resolver spath ctx =
  (resolve resolver) spath ctx

--------------------------------------------------------------------------------
-- Constructors

-- TODO: Make (E.search.*Binder contextGlobalEnv) impossible.
mkDynamicResolver :: LookupConstraint -> Resolver
mkDynamicResolver Direct =
  let r (SymPath _ n) ctx = fmap (ctx,) (maybeId (E.getValueBinder (contextGlobalEnv ctx) n))
      rname = "LocalDynamicResolver"
   in Resolver rname Lower r [rname]
mkDynamicResolver Children =
  let r (SymPath p n) ctx =
        fmap (ctx,) (maybeId (E.findValueBinder (contextGlobalEnv ctx) (SymPath ("Dynamic" : p) n)))
      rname = "GlobalDynamicResolver"
   in Resolver rname Lower r [rname]
mkDynamicResolver Full =
  let r (SymPath p n) ctx =
        fmap (ctx,) (maybeId (E.searchValueBinder (contextGlobalEnv ctx) (SymPath ("Dynamic" : p) n)))
      rname = "DynamicResolverFull"
   in Resolver rname Lower r [rname]

mkLocalResolver :: LookupConstraint -> Resolver
mkLocalResolver Direct =
  let r (SymPath _ n) ctx =
        join $ fmap (\e -> fmap (ctx,) (maybeId (E.getValueBinder e n))) (contextInternalEnv ctx)
      rname = "LocalDirectResolver"
   in Resolver rname Higher r [rname]
mkLocalResolver Children =
  let r path ctx =
        join $ fmap (\e -> fmap (ctx,) (maybeId (E.findValueBinder e path))) (contextInternalEnv ctx)
      rname = "LocalChildrenResolver"
   in Resolver rname Higher r [rname]
mkLocalResolver Full =
  let r path ctx =
        join $ fmap (\e -> fmap (ctx,) (maybeId (E.searchValueBinder e path))) (contextInternalEnv ctx)
      rname = "LocalFullResolver"
   in Resolver rname Higher r [rname]

mkGlobalResolver :: LookupConstraint -> Resolver
mkGlobalResolver Direct =
  let r (SymPath _ n) ctx =
        fmap (ctx,) (maybeId (E.getValueBinder (contextGlobalEnv ctx) n))
      rname = "GlobalDirectResolver"
   in Resolver rname Lower r [rname]
mkGlobalResolver Children =
  let r path ctx =
        fmap (ctx,) (maybeId (E.findValueBinder (contextGlobalEnv ctx) path))
      rname = "GlobalChildrenResolver"
   in Resolver rname Lower r [rname]
mkGlobalResolver Full =
  let r path ctx =
        fmap (ctx,) (maybeId (E.searchValueBinder (contextGlobalEnv ctx) path))
      rname = "GlobalFullResolver"
   in Resolver rname Lower r [rname]

--------------------------------------------------------------------------------
-- Base resolvers

-- | Resolves a symbol to a binding in the local environment if that symbol is
-- known to shadow another symbol.
localShadowResolver :: [SymPath] -> Resolver
localShadowResolver shadows =
  let local = mkLocalResolver Direct
      f = resolve local
      rname = "LocalShadowResolver"
   in Resolver
        rname
        Higher
        (\spath ctx -> if spath `elem` shadows then (f spath ctx) else Nothing)
        [rname]

-- | Resolves a symbol to a binding in the current module or one of its sub
-- modules.
currentModuleResolver :: Resolver
currentModuleResolver =
  let r (SymPath p n) ctx =
        -- TODO: Should not need search here; find should be sufficient.
        fmap (ctx,) (maybeId (E.searchValueBinder (contextGlobalEnv ctx) (SymPath ((contextPath ctx)++p) n)))
      rname = "CurrentModuleResolver"
   in Resolver rname Higher r [rname]

-- | Resolves a symbol to a binding in one of the modules currently "used".
usedModuleResolver :: Resolver
usedModuleResolver =
  let r (SymPath p n) ctx =
        let genv = (contextGlobalEnv ctx)
            usemods = (Set.toList (envUseModules genv))
            searches = map (\(SymPath p' n') -> fmap (ctx,) (maybeId (E.searchValueBinder genv (SymPath (p'++(n':p)) n)))) usemods
         in foldl (<|>) Nothing searches
      rname = "UsedModuleResolver"
   in Resolver rname Higher r [rname]

-- | Resolves a symbol to a binding in the global type environment.
typeResolver :: Resolver
typeResolver =
  let r path ctx =
        fmap (ctx,) (maybeId (lookupBinderInTypeEnv ctx path))
      rname = "TypeResolver"
   in Resolver rname Lower r [rname]

--------------------------------------------------------------------------------
-- "legacy" resolvers.
-- These are 1:1 translations to the old implementation of direct lookups in
-- Eval.hs. We should replace these with more specific combinations of
-- resolvers.
--
-- The following current issue prevents us:
--    There are several lookups that seem to rely on *search* methods to find the
--    right binding, these methods traverse cached parents.
--
--    For example, a call to `doc <sym>` in a module M results in a binding
--    M.<sym> in the global environment. Finding this in a defn call is
--    incorrect, since defn does not expect qualified names. So, the defn call's
--    name needs to remain the same.

legacyFull :: Resolver
legacyFull =
  ((mkDynamicResolver Full) {order=Higher})
  <> (mkLocalResolver Full) {
      resolve = \s@(SymPath p _) c -> if null p then (resolve (mkLocalResolver Full)) s c else Nothing
    }
  <> (mkGlobalResolver Full) {order=Higher}
  <> currentModuleResolver
  <> typeResolver {order=Higher}
  <> usedModuleResolver

legacyPreferDynamic :: Resolver
legacyPreferDynamic =
  (mkDynamicResolver Children) {order=Higher}
  <> legacyFull

legacyPreferGlobal :: Resolver
legacyPreferGlobal =
  mkGlobalResolver Children
  <> legacyFull

legacyLocal :: [SymPath] -> Resolver
legacyLocal shadows =
  localShadowResolver shadows
  <> legacyPreferDynamic
