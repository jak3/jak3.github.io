--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import        Hakyll

--import        Control.Monad         (forM,forM_)
--import        Data.Monoid           (mappend, (<>), mconcat)
import        Data.List             (isInfixOf)
--import        Data.Ord              (comparing)
--import        System.Locale         (defaultTimeLocale)
import Text.Pandoc
import        System.FilePath.Posix (takeBaseName, takeDirectory, (</>),
                                      splitFileName)

--import        Abbreviations         (abbreviationFilter)
--import        Config                (feedConfiguration)
--import        Multilang             (multiContext)
--import        JFilters              (blogFigure, blogImage, highlight)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

    match (     "static/font/*"
          .||.  "static/js/**") staticBehavior


    match "static/css/*" $ do
        route   idRoute
        compile $ compressCssCompiler >>= relativizeUrls

    match (foldr (.||.) "" $ map (fromGlob . ("mica/global/*/" ++)) ["about.rst", "contact.rst"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "mica/enote/*/*" $ do
        route   $ setExtension "html"
        compile $ pandocCompilerWith withLinkAtt defaultHakyllWriterOptions
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls
            >>= removeIndexHtml

    create ["archivio-it.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "mica/enote/it/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archivio"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index-it.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "mica/enote/it/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "mica/enote/en/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler
  where
    withLinkAtt = defaultHakyllReaderOptions
      { readerDefaultImageExtension = "+link_attributes"
      }


--------------------------------------------------------------------------------
-- Utils

-- Simply copy in the right place
staticBehavior :: Rules ()
staticBehavior = do
  route   idRoute
  compile copyFileCompiler

--------------------------------------------------------------------------------
-- Ctx
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

--------------------------------------------------------------------------------
-- Simplify URL

-- replace a foo/bar.md by foo/bar/index.html
-- this way the url looks like: foo/bar in most browsers
niceRoute :: Routes
niceRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
      where p = toFilePath ident

-- replace url of the form foo/bar/index.html by foo/bar
removeIndexHtml :: Item String -> Compiler (Item String)
removeIndexHtml item = return $ fmap (withUrls removeIndexStr) item

removeIndexStr :: String -> String
removeIndexStr url = case splitFileName url of
  (dir, "index.html") | isLocal dir -> dir
  _                                 -> url
  where isLocal uri = not ("://" `isInfixOf` uri)

