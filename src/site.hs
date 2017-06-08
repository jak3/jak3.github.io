--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import        Hakyll

import        Control.Monad         (forM_)
import        Data.Monoid           ((<>))
import        Data.List             (isInfixOf)
import        Data.Map              (keys, elems, lookup)
import        Data.Maybe            (mapMaybe)
import        Text.Pandoc
import        System.FilePath.Posix (takeBaseName, takeDirectory, (</>),
                                      splitFileName)
--import        Data.Ord              (comparing)
--import        System.Locale         (defaultTimeLocale)

import        Config
import        Multilang
--import        Abbreviations         (abbreviationFilter)
--import        JFilters              (blogFigure, blogImage, highlight)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

    match "static/css/*" $ do
        route   idRoute
        compile $ compressCssCompiler >>= relativizeUrls

    -- Default index page (a version index-LANGUAGE must exist)
    match "index.html" $ indexBehavior Italian

    forM_ langs $ \lang -> do
      let slang = show lang

      match (fromGlob $ "index-" ++ slang ++ ".html"    )  $ indexBehavior lang
      match (fromGlob $ "mica/enote/" ++ slang ++ "/*"  )  $ postBehavior  lang
      match (fromGlob $ "mica/global/" ++ slang ++ "/*" )  $ globalBehavior lang

      create [fromFilePath ("gen/" ++ slang ++ "/archive.html")]  (archiveBehavior          lang)
      create [fromFilePath ("gen/" ++ slang ++ "/rss.xml")]       (feedBehavior renderRss   lang)
      create [fromFilePath ("gen/" ++ slang ++ "/atom.xml")]      (feedBehavior renderAtom  lang)

    match "templates/*" $ compile templateCompiler
    match "templates/*/*.html" $ compile templateCompiler


-----------------------------------------------------------------------------{{{
-- Utils

--- apply a filter before render
applyFilter :: (Monad m, Functor f) => (String -> String) -> f String -> m (f String)
applyFilter transformator s = return $ fmap transformator s

-- }}}


-----------------------------------------------------------------------------{{{
-- Ctx

-- replace mappend with mconcat see src/Hakyll/Web/Feed.hs
languageContext l = map (\ (k, v) -> constField k v)
                    $ zip (keys dbTranslations) $ mapMaybe (Data.Map.lookup l) (elems dbTranslations)

postCtx :: Context String
postCtx =
    dateField "created" "%d %b %Y" `mappend`
    modificationTimeField "modified" "%d %b %Y" `mappend`
    defaultContext

defaultCtxWithLanguage :: Language -> Context String
defaultCtxWithLanguage l = mconcat $ languageContext l ++ [defaultContext]

postCtxWithLanguage :: Language -> Context String
postCtxWithLanguage l = mconcat $ [
                                    dateField "created" "%d %b %Y",
                                    modificationTimeField "modified" "%d %b %Y",
                                    defaultCtxWithLanguage l
                                  ]

indexCtx l posts = mconcat $ [
                                listField "posts" postCtx (return posts),
                                defaultCtxWithLanguage l
                             ]

-- }}}


-----------------------------------------------------------------------------{{{
-- Simplify URL

--- replace url of the form foo/bar/index.html by foo/bar
removeIndexHtml :: Item String -> Compiler (Item String)
removeIndexHtml item = return $ fmap (withUrls removeIndexStr) item

removeIndexStr :: String -> String
removeIndexStr url = case splitFileName url of
  (dir, "index.html") | isLocal dir -> dir
  _                                 -> url
  where isLocal uri = not ("://" `isInfixOf` uri)

-- }}}


-----------------------------------------------------------------------------{{{
-- Behavior

indexBehavior :: Language -> Rules ()
indexBehavior l = do
  route idRoute
  compile $ do
      posts <- recentFirst =<< loadAll (fromGlob $ "mica/enote/" ++ (show l) ++ "/*")
      let ctx = indexCtx l posts

      getResourceBody
          >>= applyAsTemplate ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls


postBehavior :: Language -> Rules ()
postBehavior l = do
  route   $ setExtension "html"
  compile $ pandocCompilerWith withLinkAtt withToc
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate (fromFilePath $ "templates/" ++ (show l) ++ "/post.html") (postCtxWithLanguage l)
      >>= loadAndApplyTemplate "templates/default.html"                                  (postCtxWithLanguage l)
      >>= relativizeUrls
      >>= removeIndexHtml
  where
    withLinkAtt = defaultHakyllReaderOptions
      { readerDefaultImageExtension = "+link_attributes"
      }
    withToc = defaultHakyllWriterOptions
        { writerTableOfContents = True
        , writerTemplate        = Just "$toc$\n$body$"
        }

globalBehavior :: Language -> Rules ()
globalBehavior l = do
  route   $ setExtension "html"
  compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" (defaultCtxWithLanguage l)
      >>= relativizeUrls

archiveBehavior :: Language -> Rules ()
archiveBehavior language = do
    route idRoute
    compile $ do
        posts <- recentFirst =<< loadAll (fromGlob $ "mica/enote/" ++ (show language) ++ "/*")
        --let ctx = listField "posts" postCtx (return posts)
        let ctx = indexCtx language posts

        makeItem ""
            >>= loadAndApplyTemplate langTemplate ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls
      where
        langTemplate = fromFilePath $ "templates/" ++ (show language) ++ "/archive.html"


feedBehavior :: (FeedConfiguration
                  -> Context String
                  -> [Item String]
                  -> Compiler (Item String)) -> Language -> Rules ()
feedBehavior render language = do
    route idRoute
    compile $
        loadAllSnapshots (fromGlob $ "mica/enote/" ++ (show language) ++ "/*") "content"
        >>= fmap (take 10) . recentFirst
        >>= mapM (applyFilter protectCDATA)
        >>= render (feedConfig language) feedCtx
      where

        feedCtx :: Context String
        feedCtx = postCtx `mappend` bodyField "description"

        protectCDATA :: String -> String
        protectCDATA = replaceAll "]]>" (const "]]&gt;")


-- }}}
