{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.App.Portal (
  view_,
  view
) where



import           React.Flux                 hiding (view)
import qualified React.Flux                 as RF

import           LN.UI.Helpers.ReactFluxDOM (ahref)
import           LN.UI.Router               (CRUD (..), Route (..),
                                             RouteWith (..), routeWith')
import           LN.UI.Types                (HTMLEvent_)



view_ :: HTMLEvent_
view_ =
  RF.view view () mempty



view :: ReactView ()
view = defineView "portal" $ \_ ->
  div_ $ p_ $ do
    h1_ "Portal"
    ol_ $ do
      li_ $ ahref $ routeWith' (Organizations Index)
      li_ $ ahref $ routeWith' (Users Index)
