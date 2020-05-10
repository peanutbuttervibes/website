--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid          (mappend)
import           Hakyll
import           Hakyll.Core.Compiler (unsafeCompiler)
import           KaTeX.KaTeXify       (kaTeXifyIO)


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "assets/images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "assets/katex/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "assets/katex/fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "assets/katex/contrib/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "assets/css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "content/posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "content/uml/*" $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "content/posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "content/posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Welcome to my website!" `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "uml.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "content/uml/*"
            let umlCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title"
                    "Solutions for Understanding Machine Learning" `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate umlCtx
                >>= loadAndApplyTemplate "templates/default.html" umlCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
pandocMathCompiler :: Compiler (Item String)
pandocMathCompiler = do
    identifier <- getUnderlying
    s <- getMetadataField identifier "katex"
    case s of
        Just _ ->
            pandocCompilerWithTransformM
                defaultHakyllReaderOptions defaultHakyllWriterOptions
                (unsafeCompiler . kaTeXifyIO)
        Nothing -> pandocCompiler
