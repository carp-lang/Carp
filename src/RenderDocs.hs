{-# LANGUAGE OverloadedStrings #-}

module RenderDocs where

import CMark
import Control.Monad (when)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Text as Text
import qualified Map
import qualified Meta
import Obj
import Path
import Project
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import TypeError (typeVariablesInOrderOfAppearance)
import Types

-- TODO: Move the beautification to a much earlier place, preferably when the function is defined/concretized-
-- This might be a duplicate with the work in a PR by @jacereda
beautifyType :: Ty -> Ty
beautifyType t =
  let tys = List.nub (typeVariablesInOrderOfAppearance t)
      mappings =
        Map.fromList
          ( List.zip
              (List.map (\(VarTy name) -> name) tys)
              (List.map (VarTy . (: [])) ['a' ..])
          )
   in replaceTyVars mappings t

saveDocsForEnvs :: Project -> [(SymPath, Binder)] -> IO ()
saveDocsForEnvs ctx pathsAndEnvBinders =
  let dir = projectDocsDir ctx
      title = projectTitle ctx
      generateIndex = projectDocsGenerateIndex ctx
      dependencies = getDependenciesForEnvs (Prelude.map (\(p, b) -> (p, fst (getEnvAndMetaFromBinder b))) pathsAndEnvBinders)
      pathsAndEnvBinders' = pathsAndEnvBinders ++ dependencies
      allEnvNames = fmap fst pathsAndEnvBinders'
   in do
        mapM_ (saveDocsForEnvBinder ctx allEnvNames) pathsAndEnvBinders'
        when
          generateIndex
          ( writeFile
              (dir </> title ++ "_index.html")
              (projectIndexPage ctx allEnvNames)
          )
        putStrLn ("Generated docs to '" ++ dir ++ "'")
  where
    getDependenciesForEnvs = Prelude.concat . Prelude.map getEnvDependencies
    getEnvDependencies (SymPath ps p, e) =
      let envs =
            Prelude.map
              (\(n, b) -> (SymPath (ps ++ [p]) n, b))
              ( Prelude.filter
                  (\(_, Binder _ x) -> isMod x)
                  ( Prelude.filter
                      shouldEmitDocsForBinder
                      (Map.toList (envBindings e))
                  )
              )
       in envs
            ++ getDependenciesForEnvs (Prelude.map (\(n, Binder _ (XObj (Mod env _) _ _)) -> (n, env)) envs)

-- | This function expects a binder that contains an environment, anything else is a runtime error.
getEnvAndMetaFromBinder :: Binder -> (Env, MetaData)
getEnvAndMetaFromBinder envBinder =
  case envBinder of
    Binder meta (XObj (Mod env _) _ _) -> (env, meta)
    _ -> error "Binder's not a module. This should be detected in 'commandSaveDocsInternal'."

projectIndexPage :: Project -> [SymPath] -> String
projectIndexPage ctx moduleNames =
  let logo = projectDocsLogo ctx
      url = projectDocsURL ctx
      css = projectDocsStyling ctx
      htmlHeader = H.toHtml $ projectTitle ctx
      htmlDoc = commonmarkToHtml [optSafe] $ Text.pack $ projectDocsPrelude ctx
      html = renderHtml $
        H.docTypeHtml $
          do
            headOfPage css
            H.body $
              H.div ! A.class_ "content" $
                H.a ! A.href (H.stringValue url) $
                  do
                    H.div ! A.class_ "logo" $
                      do
                        H.img ! A.src (H.stringValue logo) ! A.alt "Logo"
                        moduleIndex moduleNames
                    H.div $
                      do
                        H.h1 htmlHeader
                        H.preEscapedToHtml htmlDoc
   in html

headOfPage :: String -> H.Html
headOfPage css =
  H.head $
    do
      H.meta ! A.charset "UTF-8"
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0"
      H.link ! A.rel "stylesheet" ! A.href (H.stringValue css)

getModuleName :: Env -> String
getModuleName env = fromMaybe "Global" (envModuleName env)

saveDocsForEnvBinder :: Project -> [SymPath] -> (SymPath, Binder) -> IO ()
saveDocsForEnvBinder ctx moduleNames (envPath, envBinder) =
  do
    let moduleName = show envPath
        dir = projectDocsDir ctx
        fullPath = dir </> moduleName ++ ".html"
        string = renderHtml (envBinderToHtml envBinder ctx (show envPath) moduleNames)
    createDirectoryIfMissing False dir
    writeFile fullPath string

envBinderToHtml :: Binder -> Project -> String -> [SymPath] -> H.Html
envBinderToHtml envBinder ctx moduleName moduleNames =
  let (env, meta) = getEnvAndMetaFromBinder envBinder
      title = projectTitle ctx
      css = projectDocsStyling ctx
      url = projectDocsURL ctx
      logo = projectDocsLogo ctx
      moduleDescription = case Meta.get "doc" meta of
        Just (XObj (Str s) _ _) -> s
        Nothing -> ""
        _ -> error "moduledescription"
      moduleDescriptionHtml = commonmarkToHtml [optSafe] $ Text.pack moduleDescription
   in H.docTypeHtml $
        do
          headOfPage css
          H.body $
            H.div ! A.class_ "content" $
              do
                H.div ! A.class_ "logo" $
                  do
                    H.a ! A.href (H.stringValue url) $
                      H.img ! A.src (H.stringValue logo)
                    --span_ "CARP DOCS FOR"
                    H.div ! A.class_ "title" $ H.toHtml title
                    moduleIndex moduleNames
                H.h1 (H.toHtml moduleName)
                H.div ! A.class_ "module-description" $ H.preEscapedToHtml moduleDescriptionHtml
                mapM_ (binderToHtml moduleName . snd) (Prelude.filter shouldEmitDocsForBinder (Map.toList (envBindings env)))

shouldEmitDocsForBinder :: (String, Binder) -> Bool
shouldEmitDocsForBinder (_, Binder meta _) =
  not (metaIsTrue meta "hidden")

moduleIndex :: [SymPath] -> H.Html
moduleIndex moduleNames =
  H.div ! A.class_ "index" $ grouped moduleNames
  where
    grouped names = H.ul $ mapM_ gen (order names)
    gen (m, subs) =
      H.li $
        do
          moduleLink m
          grouped subs
    order [] = []
    order (m : mods) =
      let (isIn, isNotIn) = List.partition (symBelongsToMod m) mods
       in (m, isIn) : order isNotIn
    symBelongsToMod (SymPath xs x) (SymPath ys y) =
      List.isPrefixOf (xs ++ [x]) (ys ++ [y])

moduleLink :: SymPath -> H.Html
moduleLink p@(SymPath _ name) =
  H.a ! A.href (H.stringValue (show p ++ ".html")) $ H.toHtml name

binderToHtml :: String -> Binder -> H.Html
binderToHtml moduleName (Binder meta xobj) =
  let name = getSimpleName xobj
      maybeNameAndArgs = getSimpleNameWithArgs xobj
      description = getBinderDescription xobj
      typeSignature = case xobjTy xobj of
        Just t -> show (beautifyType t) -- NOTE: This destroys user-defined names of type variables!
        Nothing -> ""
      isDeprecated = case Meta.get "deprecated" meta of
        Just (XObj (Bol True) _ _) -> True
        Just (XObj (Str _) _ _) -> True
        _ -> False
      deprecationStr = case Meta.get "deprecated" meta of
        Just (XObj (Str s) _ _) -> commonmarkToHtml [optSafe] $ Text.pack s
        _ -> ""
      docString = case Meta.get "doc" meta of
        Just (XObj (Str s) _ _) -> s
        Just found -> pretty found
        Nothing -> ""
      htmlDoc = commonmarkToHtml [optSafe] $ Text.pack docString
   in H.div ! A.class_ "binder" $
        do
          H.a ! A.class_ "anchor" ! A.href (H.stringValue ("#" ++ name)) $
            H.h3 ! A.id (H.stringValue name) $
              do
                if isMod xobj
                  then H.a ! A.href (H.stringValue (moduleName ++ "." ++ pretty xobj ++ ".html")) $ H.toHtml (pretty xobj)
                  else H.toHtml name
                when isDeprecated $
                  H.span ! A.class_ "deprecation-notice" $
                    H.toHtml ("deprecated" :: String)
          H.div ! A.class_ "description" $ H.toHtml description
          H.p ! A.class_ "sig" $ H.toHtml typeSignature
          case maybeNameAndArgs of
            Just nameAndArgs -> H.pre ! A.class_ "args" $ H.toHtml nameAndArgs
            Nothing -> H.span $ H.toHtml ("" :: String)
          H.p ! A.class_ "doc" $ H.preEscapedToHtml htmlDoc
          when isDeprecated $
            H.div ! A.class_ "deprecation-text" $
              H.preEscapedToHtml deprecationStr

--p_ (toHtml (description))
