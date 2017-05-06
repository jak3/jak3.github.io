-- Thanks to Yann Esposito (https://github.com/yogsototh/yblog.git)
--
module Config where

import Hakyll (FeedConfiguration(..))

--------------------------------------------------------------------------------
-- Important all item of lang must have a length of 2.
langs :: [String]
langs = ["en","it"]

--------------------------------------------------------------------------------
fstlang :: String
fstlang = head langs

--------------------------------------------------------------------------------
feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
  { feedTitle = "micae"
  , feedDescription = "GM's e-notecard system"
  , feedAuthorName = "Giacomo Mantani"
  , feedAuthorEmail = "name.surname on gmail"
  , feedRoot = "http://"
  }
