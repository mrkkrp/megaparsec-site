-- -*- Mode: Haskell; -*-
--
-- Web-site of Megaparsec project.
--
-- Copyright © 2015 Megaparsec contributors
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
      >>= loadAndApplyTemplate "templates/tutorial.html" datedContext
      >>= loadAndApplyTemplate "templates/default.html"  datedContext
      >>= relativizeUrls

  create ["tutorials.html"] $ do
    route idRoute
    compile $ do
      ts <- recentFirst =<< loadAll "tutorials/*"
      let tutorialsContext =
            listField "tutorials" datedContext (return ts) <>
            constField "title" "Tutorials" <>
            defaultContext
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

datedContext :: Context String
datedContext = dateField "date" "%B %e, %Y" <> defaultContext
