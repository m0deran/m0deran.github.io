--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Data.List
import           Control.Applicative
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    postList <- sortRecentFirst =<< getMatches "posts/*"
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    tagsRules tags $ \tag pattern -> do
        let title = "topic: " ++ tag
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title
                      `mappend` listField "posts" (postCtxTags tags) (return posts)
                      `mappend` defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tags.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "posts/*" $ do
        let ctx =
              field "next" (nextPostURL postList) <>
              field "prev" (prevPostURL postList) <>
              postCtxTags tags
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
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
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext

postCtxTags :: Tags -> Context String
postCtxTags tags = tagsField "tags" tags <> postCtx

findPostUrl :: ([Identifier] -> Identifier -> Maybe Identifier)
             -> [Identifier] -> Item String
             -> Compiler String
findPostUrl p posts post = do
    let id = itemIdentifier post
    case p posts id of
        Just m  -> maybe empty toUrl <$> getRoute m
        Nothing -> empty


prevPostURL :: [Identifier] -> Item String -> Compiler String
prevPostURL = findPostUrl lookupPrev


nextPostURL :: [Identifier] -> Item String -> Compiler String
nextPostURL = findPostUrl lookupNext


lookupPrev :: Eq a => [a] -> a -> Maybe a
lookupPrev ids id = case elemIndex id ids of
                        Just i -> if i >= (length ids - 1) then Nothing else Just $ ids!!(i+1)
                        Nothing -> Nothing

lookupNext :: Eq a => [a] -> a -> Maybe a
lookupNext ids id = case elemIndex id ids of
                        Just i -> if i <= 0 then Nothing else Just $ ids!!(i-1)
                        Nothing -> Nothing
