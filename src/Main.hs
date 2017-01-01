--
-- Web-site of the Megaparsec project.
--
-- Copyright © 2015–2017 Megaparsec contributors
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
-- * Redistributions of source code must retain the above copyright notice,
--   this list of conditions and the following disclaimer.
--
-- * Redistributions in binary form must reproduce the above copyright
--   notice, this list of conditions and the following disclaimer in the
--   documentation and/or other materials provided with the distribution.
--
-- * Neither the name of the copyright holder nor the names of contributors
--   may be used to endorse or promote products derived from this software
--   without specific prior written permission.
--
-- This software is provided by the copyright holders "as is" and any
-- express or implied warranties, including, but not limited to, the implied
-- warranties of merchantability and fitness for a particular purpose are
-- disclaimed. In no event shall the copyright holders be liable for any
-- direct, indirect, incidental, special, exemplary, or consequential
-- damages (including, but not limited to, procurement of substitute goods
-- or services; loss of use, data, or profits; or business interruption)
-- however caused and on any theory of liability, whether in contract,
-- strict liability, or tort (including negligence or otherwise) arising in
-- any way out of the use of this software, even if advised of the
-- possibility of such damage.

{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid ((<>))
import Hakyll

----------------------------------------------------------------------------
-- Constants

menu :: [(String, String)]
menu =
  [ ("Tutorials", "/tutorials.html")
  , ("Hackage",   "https://hackage.haskell.org/package/megaparsec")
  , ("GitHub",    "https://github.com/mrkkrp/megaparsec")
  ]

tutorialCode :: String
tutorialCode =
  "https://github.com/mrkkrp/megaparsec-site/tree/master/tutorial-code/"

----------------------------------------------------------------------------
-- Main

main :: IO ()
main = hakyll $ do

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "js/*" $ do
    route idRoute
    compile copyFileCompiler

  match "tutorials/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/tutorial.html" tutorialContext
      >>= loadAndApplyTemplate "templates/default.html"  tutorialContext
      >>= relativizeUrls

  create ["tutorials.html"] $ do
    route idRoute
    compile $ do
      ts <- recentFirst =<< loadAll "tutorials/*"
      let tutorialsContext =
            listField "tutorials" datedContext (return ts) <>
            constField "title" "Tutorials" <>
            menuContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/tutorials.html" tutorialsContext
        >>= loadAndApplyTemplate "templates/default.html"   tutorialsContext
        >>= relativizeUrls

  match "index.md" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" datedContext
      >>= relativizeUrls

  match "404.md" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" datedContext
      >>= relativizeUrls

  match "templates/*" $ compile templateCompiler

----------------------------------------------------------------------------
-- Contexts

datedContext :: Context String
datedContext = dateField "date" "%B %e, %Y" <> menuContext

tutorialContext :: Context String
tutorialContext = codeLink <> datedContext
  where
    codeLink = field "code-link" $ \item -> do
      file <- getMetadataField' (itemIdentifier item) "code"
      return (tutorialCode <> file)

menuContext :: Context String
menuContext = listField "menu-items" iContext (return is) <> defaultContext
  where is = Item "item" <$> menu
        iContext = field "item-title" (return . fst . itemBody) <>
                   field "item-url"   (return . snd . itemBody)
