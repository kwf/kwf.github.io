--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import qualified Hakyll.Core.Identifier as Ident
import           System.FilePath
import           Data.List

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match ("images/*"    .||.
           "pdf/*"       .||.
           "bibtex/*"    .||.
           "disk/*"      .||.
           "scripts/*"   .||.
           "keybase.txt" .||.
           "CNAME"       .||.
           "favicon.ico") $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    let toplevel = [ "publications.html" ]

    match (fromList toplevel) $ do
        route   $ cleanRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls
            >>= cleanIndexUrls

    let standalone = [ "jazz.html" ]

    match (fromList standalone) $ do
         route   $ cleanRoute
         compile $ getResourceString
            >>= relativizeUrls
            >>= cleanIndexUrls

    match ("posts/*") $ do
        route   $ cleanRoute `composeRoutes` gsubRoute "posts/" (const "")
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls
            >>= cleanIndexUrls

    create ["archive.html"] $ do
        route   $ cleanRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
            let itemCtx = postCtx `mappend` bodyField "content"
            let archiveCtx =
                    listField "posts" itemCtx (return posts) `mappend`
                    constField "title" "Blog Archives"       `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls
                >>= cleanIndexUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "very.science"        `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls
                >>= cleanIndexUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    teaserField "teaser" "content" `mappend`
    defaultContext

--------------------------------------------------------------------------------
-- Clean URLs ala <https://www.rohanjain.in/hakyll-clean-urls/>

cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
    createIndexRoute ident =
      takeDirectory p </>  -- comment this out to put things at the root
      takeBaseName p </> "index.html"
      where p = toFilePath ident

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pattern replacement)
    where
      pattern = "/index.html"
      replacement = const "/"

cleanIndex :: String -> String
cleanIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
  where idx = "index.html"

--------------------------------------------------------------------------------
-- Lift a bunch of static things to the top level

liftStaticDir dir =
  match (fromList [fromFilePath (dir ++ "/*")]) $ do
    route (gsubRoute (dir ++ "/") (const ""))
    compile copyFileCompiler
