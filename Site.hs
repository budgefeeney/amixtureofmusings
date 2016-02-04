{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid ((<>))
import GHC.IO.Encoding
import Hakyll hiding (pandocCompiler)
import Site.Archive
import Site.Configuration
import Site.Index
import Site.Meta
import Site.Pagination
import Site.Pandoc

staticFile :: Pattern -> Rules ()
staticFile pattern =
  match pattern $ do
    route idRoute
    compile copyFileCompiler

main :: IO ()
main = do
  -- Fix encoding on Windows
  setLocaleEncoding utf8

  -- Retrieve current year to put in the footer
  yearContext <- getYearContext
  let siteContext = yearContext <> defaultContext
  let fullContext = constField "title" "A Mixture of Musings" <> siteContext

  -- Run Hakyll
  hakyllWith config $ do

    match "css/*.css" $ do
      route   idRoute
      compile compressCssCompiler

    sequence_ $ fmap staticFile
      [ "scripts/*", "images/*", "favicon.png", ".htaccess" ]

    match "templates/*" $ compile templateCompiler

    paginate <- buildPaginate "posts/*.md"
    paginateRules paginate $ \i _ -> do
      route dateRoute
      compile $ do
        let ctx = paginatePostsContext paginate i <> siteContext
        pandocCompiler
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/post.html" ctx
          >>= stripIndexSuffix

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    -- Drafts are not included by the paginate above, handle them manually.
    match "posts/*.md" $ do
      route dateRoute
      compile $
        pandocCompiler
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/post.html" ((postContextWithTags tags) <> siteContext)
          >>= stripIndexSuffix


    postIndex "posts/*.md" 5 fullContext

    create ["archive/index.html"] $ do
      route idRoute
      compile $ do
        let ctx = archiveContext "posts/*" <> fullContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/archive.html" ctx
          >>= stripIndexSuffix

    tagsRules tags $ \tag pattern -> do
      let title = "Posts tagged \"" ++ tag ++ "\""
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll pattern
        let ctx = constField "title" title
                  <> listField "posts" postContext (return posts)
                  <> fullContext

        makeItem ""
          >>= loadAndApplyTemplate "templates/tag.html" ctx
          >>= relativizeUrls

    create ["feed.xml"] $ do
      route idRoute
      compile $ do
        let feedContext = bodyField "description" <> defaultContext
        loadAllSnapshots "posts/*" "content"
          >>= filterDraftItems
          >>= fmap (take 10) . recentFirst
          >>= renderAtom feedConfig feedContext
