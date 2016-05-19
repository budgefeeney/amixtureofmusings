{-- |
  This module holds the configuration for the Mixture-of-Musings
  website and feed, as used by Hakyll
-}
module Site.Configuration (config, feedConfig) where

import Hakyll

ignoreFile' :: String -> Bool
ignoreFile' ".htaccess" = False
ignoreFile' path        = ignoreFile defaultConfiguration path

config :: Configuration
config = defaultConfiguration { ignoreFile = ignoreFile'
                              , previewHost = "0.0.0.0"
                              , deployCommand = "rsync -avz -e ssh _site bfeeney@amixtureofmusings.com:/var/www/html"
                              }

-- Details for the atom feed.
feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
  { feedTitle       = "A Mixture of Musings"
  , feedDescription = "Bryan Feeney’s blog on tech, programming and machine-learning"
  , feedAuthorName  = "Bryan Feeney"
  , feedAuthorEmail = "bryan@amixtureofmusings.com"
  , feedRoot        = "http://amixtureofmusings.com"
  }
