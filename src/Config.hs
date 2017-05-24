module Config where

import Multilang
import Hakyll (FeedConfiguration(..))

feedConfig :: Language -> FeedConfiguration
feedConfig lang = FeedConfiguration
  { feedTitle = "condensō [" ++ (show lang) ++ "]"
  , feedDescription = "GM's e-notecard system"
  , feedAuthorName = "Giacomo Mantani"
  , feedAuthorEmail = "name.surname on gmail"
  , feedRoot = "https://jak3.github.io"
  }
