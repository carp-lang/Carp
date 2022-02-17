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

data LookupPreference
  = PreferDynamic
  | PreferGlobal
  | PreferLocal [SymPath]
  deriving (Eq, Show)

data LookupOrder = Higher | Lower
  deriving(Eq)

instance Ord LookupOrder where
  compare Higher Lower = GT
  compare Lower Higher = LT
  compare Lower Lower = EQ
  compare Higher Higher = EQ

instance Ord LookupPreference where
  _ <= PreferDynamic = False
  PreferDynamic <= _ = True
  (PreferLocal _) <= _ = False
  _ <= (PreferLocal _) = True
  _ <= _ = True

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
-- Public functions

-- | Resolves a symbol to a local binding that is stored directly in the
-- context's internal environment.
localDynamicResolver :: Resolver
localDynamicResolver = mkDynamicResolver Direct

-- | Resolves a symbol to a binding in the global Dynamic module.
globalDynamicResolver :: Resolver
globalDynamicResolver = mkDynamicResolver Children

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

-- | Searches the (potentially) stale parents of internal environments for a
-- local binding.
localCacheResolver :: Resolver
localCacheResolver = 
  let cache = (mkLocalResolver Full) 
   in cache {
        resolve = \path@(SymPath p _) ctx -> 
          if null p 
            then (resolve cache) path ctx
            else Nothing,
        resolverName = "LocalCacheResolver", 
        resolverStack = ["LocalCacheResolver"]
      }

-- | Resolves a symbol to a binding that is a direct child of the global
-- environment (a top-level binding).
topLevelResolver :: Resolver 
topLevelResolver = (mkGlobalResolver Direct) {resolverName = "TopLevelResolver", resolverStack = ["TopLevelResolver"]}

-- | Resolves a symbol to a child of the global environment, possibly in a
-- child module of the global environment.
globalResolver :: Resolver
globalResolver = mkGlobalResolver Children

-- | Look everywhere.
universalResolver :: Resolver
universalResolver = 
  let re = (mkLocalResolver Full <> mkGlobalResolver Full <> mkDynamicResolver Full) 
   in re {resolverName = "UniversalResolver", 
          resolverStack = ["UniversalResolver"] ++ tail (resolverStack re)}

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

-- | Standard sequence of resolvers to try when no other resolutions succeed.
-- Always has the lowest order.
fallbackResolver :: Resolver
fallbackResolver = 
  currentModuleResolver <> usedModuleResolver <> universalResolver <> typeResolver {order = Lower}

-- | Sequence of resolvers to try when resolving symbols in function bodies.
functionBodySymbolResolver :: [SymPath] ->  Resolver
functionBodySymbolResolver shadows =
  localShadowResolver shadows <> standardResolver
  
applyResolver :: Resolver -> SymPath -> Context -> Maybe (Context, Binder)
applyResolver resolver spath ctx = 
  (resolve resolver) spath ctx

-- | Normally, local and global resolvers take precedence over dynamic
-- resolvers. This resolver inverts this behavior, combining a given resolver
-- with a dynamic resolver that always takes precedence.
forceDynamicResolver :: Resolver -> Resolver
forceDynamicResolver resolver =
  localDynamicResolver {order = Higher} 
  <> globalDynamicResolver {order = Higher} 
  <> resolver

-- | Given a resolver, returns a new resolver that will attempt to resolve
-- symbols globally first, regardless of the input resolver's precedence.
forceGlobalResolver :: Resolver -> Resolver
forceGlobalResolver resolver =
  globalResolver {order = Higher} <> resolver

-- | Resolve a symbol to a binding in the context's local environment.
localResolver :: Resolver
localResolver = 
  mkLocalResolver Children

dynamicResolver :: Resolver
dynamicResolver = 
  localDynamicResolver <> globalDynamicResolver

standardResolver :: Resolver
standardResolver = 
  -- n.b. we need to call the cache resolver specifically for the case:
  -- primitiveEval, during evaluation of the *arg* argument.
  --
  -- This is a bit strange--in theory, if the environment parents are correct,
  -- we should never need to rely on the parent of an internal environment since
  -- its parent should == the global environment.
  localResolver <> localCacheResolver <> globalDynamicResolver {order = Higher} <> fallbackResolver

standardResolverNoCache :: Resolver
standardResolverNoCache =
  localResolver <> globalDynamicResolver {order = Higher} <> fallbackResolver

-- |
staticEnvResolver :: Env -> Resolver
staticEnvResolver e = 
  let resolver = (mkLocalResolver Children) 
   in resolver {
        resolve = \path ctx -> (resolve resolver) path ctx {contextInternalEnv = Just e},
        resolverName = "StaticEnvResolver",
        resolverStack = ["StaticEnvResolver"]
      }

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
