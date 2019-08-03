{-# LANGUAGE OverloadedStrings #-}

import           Data.Maybe      (fromMaybe)
import           Data.Monoid     ((<>))
import           Hakyll
import           System.Process  (readProcess)
import           Text.Pandoc

import           BibParse
import           GHC.IO.Encoding


main :: IO ()
main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8
  hakyll $ do

    -- static content
    mapM_ (`match` (route idRoute >> compile copyFileCompiler))
          [   "CNAME"
            , "css/*"
            , "css/layouts/*"
            , "img/*"
            , "js/*"
            , "media/*"
          ]

    -- Static pages
    match ("pages/*.markdown" .||. "pages/*.md" .||. "pages/*.org") $ do
        route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
        compile $
            pageCompiler
                >>= loadAndApplyTemplate "templates/default.html" builtPageCtx
                >>= relativizeUrls
    match "templates/*" $ compile templateCompiler

    -- robots, etc.
    match "assets/txt/*" $ do
        route $ gsubRoute "assets/txt/" (const "")
        compile copyFileCompiler

    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            -- posts <- recentFirst =<< loadAll "posts/*"
            -- pages <- loadAll "pages/*"
            -- let allPages = pages ++ posts
            let sitemapCtx = builtPageCtx <> lastGitModification
            makeItem ""
                >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle = "JAIF 2019"
      , feedDescription = "JAIF 2019"
      , feedAuthorName = "Damien Couroussé"
      , feedAuthorEmail = "damien.courousse@cea.fr"
      , feedRoot = "http://jaif2019.github.io"
    }

-- Auxiliary compilers
--    the main page compiler
pageCompiler :: Compiler (Item String)
pageCompiler = do
    bibFile <- getUnderlying >>= \i -> getMetadataField i "biblio"
    case bibFile of
         Just f  -> bibtexCompiler f
         Nothing -> pandocCompiler

--    Biblio
bibtexCompiler :: String -> Compiler (Item String)
bibtexCompiler bibFile = do
    cslFile <- getUnderlying >>= \i -> getMetadataField i "csl"
    csl <- load (fromFilePath $ "assets/csl/" ++ fromMaybe "chicago.csl" cslFile)
    bib <- load (fromFilePath $ "assets/bib/" ++ bibFile)
    getResourceBody
        >>= appendCitations bib
        >>= readPandocBiblio
            (def {readerExtensions = pandocExtensions })
            csl bib
        >>= \x -> return $ writePandoc x

appendCitations :: Item Biblio -> Item String -> Compiler (Item String)
appendCitations bib t = pure (processCitations refs <$> t)
    where Biblio refs = itemBody bib

-- Context builders
builtPageCtx :: Context String
builtPageCtx =  constField "siteroot" (feedRoot feedConfiguration)
             <> listField "entries" builtPageCtx (loadAll $ "pages/*" .||. "posts/*")
             <> dateField "date" "%A, %e %B %Y"
             <> dateField "isodate" "%F"
             <> gitDate
             <> gitCommit
             <> lastGitModification
             <> defaultContext

postCtx :: Context String
postCtx =  dateField "date" "%B %e, %Y"
        <> dateField "dateArchive" "%b %e"
        <> modificationTimeField "mtime" "%F"
        <> defaultContext

-- | Extracts git commit info and render some html code for the page footer.
--
-- Adapted from
-- - Jorge.Israel.Peña at https://github.com/blaenk/blaenk.github.io
-- - Miikka Koskinen at http://vapaus.org/text/hakyll-configuration.html
gitTagWith
  :: String -- ^ the Context key
  -> String -- ^ the git log format string
  -> Context String
gitTagWith key logFormat = field key $ \item -> do
  let fp = toFilePath $ itemIdentifier item
  unsafeCompiler $
    readProcess "git" ["log", "-1", "HEAD", "--pretty=format:" ++ logFormat, fp] ""

gitDate, gitCommit :: Context String
gitDate = gitTagWith "gitdate" "%aD"
gitCommit = gitTagWith "gitcommit" "%h"

-- | Extract the last modification date from the git commits
lastGitModification :: Context a
lastGitModification = field "lastgitmod" $ \item -> do
  let fp = toFilePath $ itemIdentifier item
  unsafeCompiler $
    readProcess "git" ["log", "-1", "HEAD", "--pretty=format:%ad", "--date=short", fp] ""
