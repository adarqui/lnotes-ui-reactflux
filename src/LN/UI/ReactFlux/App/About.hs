{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module LN.UI.ReactFlux.App.About (
  view
) where



import           React.Flux  hiding (view)
import qualified React.Flux  as RF

import           LN.UI.ReactFlux.Types
import           LN.UI.ReactFlux.Helpers.ReactFluxView



view :: HTMLView_
view =
  defineViewWithSKey "about" () go
  where
  go _ = div_ $ p_ $ elemText "LN is a full stack haskell system which powers adarq.org"
