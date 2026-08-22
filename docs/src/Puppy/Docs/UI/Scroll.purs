module Puppy.Docs.UI.Scroll (contentId, reset) where

import Prelude

import Effect (Effect)

-- | The element the page scrolls inside. Named so that `reset` can find it.
contentId :: String
contentId = "content"

-- | Put the content column back at the top, as a new page should be.
foreign import resetImpl :: String -> Effect Unit

reset :: Effect Unit
reset = resetImpl contentId
