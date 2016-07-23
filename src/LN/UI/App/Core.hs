{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.App.Core (
  Store (..),
  defaultStore,
  Action (..),
  store,
  view_,
  view,
  initRouter
) where



import           Control.Concurrent              (forkIO)
import           Control.DeepSeq                 (NFData)
import           Control.Monad                   (void)
import           Control.Monad.IO.Class          (liftIO)
import           Data.List                       ((\\))
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Monoid                     ((<>))
import           Data.Rehtie                     (rehtie)
import           Data.Text                       (Text)
import           Data.Typeable                   (Typeable)
import           GHC.Generics                    (Generic)
import           React.Flux                      hiding (view)
import qualified React.Flux                      as RF
import           React.Flux.Router.WebRoutes     (initRouterRaw'ByteString)

import           LN.Api                          (getMe', getUserSanitizedPacks_ByUsersIds')
import           LN.T.Pack.Sanitized.User        (UserSanitizedPackResponse (..), UserSanitizedPackResponses (..))
import           LN.T.User                       (UserResponse (..))
import qualified LN.UI.App.About                 as About
import qualified LN.UI.App.Breadcrumbs           as Breadcrumbs
import           LN.UI.App.Core.Shared           (Action (..), Store (..),
                                                  defaultStore)
import qualified LN.UI.App.Home                  as Home
import qualified LN.UI.App.Organizations         as Organizations
import qualified LN.UI.App.Users as Users
import qualified LN.UI.App.Portal                as Portal
import           LN.UI.Helpers.HaskellApiHelpers (rd)
import           LN.UI.Helpers.ReactFluxDOM      (ahref, ahrefName)
import           LN.UI.Router
import           LN.UI.State.PageInfo            (PageInfo, defaultPageInfo)



instance StoreData Store where
  type StoreAction Store = Action
  transform action st@Store{..} = do
    case action of
      Init            -> action_init
      SetRoute route  -> action_set_route route
      SyncUsers users -> action_sync_users users
      _               -> pure st
    where
    action_init = do
      putStrLn "Init"
      lr <- rd getMe'
      rehtie lr (const $ pure st) $ \user_pack ->
        pure $ st{ _me = Just user_pack }

    action_set_route route_with@(RouteWith route params) = do

      putStrLn $ show route_with

      case route_with of

        RouteWith Home _                       -> pure ()
        RouteWith About _                      -> pure ()
        RouteWith Portal _                     -> pure ()
        RouteWith (Organizations crud) params  -> void $ forkIO $ executeAction $ SomeStoreAction Organizations.store $ Organizations.Init crud params
        RouteWith (Users Index) params         -> void $ forkIO $ executeAction $ SomeStoreAction Users.store $ Users.Init params
        RouteWith _ _                          -> pure ()

      pure $ st{ _route = route_with }

    -- | We maintain a global Map of users for quick access
    --
    action_sync_users users = do
      let users_difference = Map.keys _users \\ users
      lr <- rd $ getUserSanitizedPacks_ByUsersIds' users_difference
      rehtie lr (const $ pure st) $ \UserSanitizedPackResponses{..} -> do
        pure $ st{
          _users = Map.union _users (Map.fromList $ map (\pack -> (userSanitizedPackResponseUserId pack, pack)) userSanitizedPackResponses)
        }


store :: ReactStore Store
store = mkStore defaultStore



view_ :: ReactElementM eventHandler ()
view_ =
  RF.view view () mempty



view :: ReactView ()
view =
  defineControllerView "core" store $ \st _ ->
    defaultLayout st (renderRouteView st)



initRouter :: IO ()
initRouter =
  initRouterRaw'ByteString (Just go) go
  where
  go = \raw_uri -> do
    print raw_uri
    routeAlterStore $ toRouteWithHash raw_uri
    where
      routeAlterStore action =
        -- Update Store with our new route
        liftIO $ alterStore store $ SetRoute action



defaultLayout :: Store -> ReactElementM ViewEventHandler () -> ReactElementM ViewEventHandler ()
defaultLayout st@Store{..} page =
  div_ $ do
    navBar _me
    Breadcrumbs.view_ _route
    div_ page



navBar :: Maybe UserResponse -> ReactElementM ViewEventHandler ()
navBar m_user_pack =
  div_ $ do
    ul_ $ do
      li_ $ ahref $ routeWith' Home
      li_ $ ahref $ routeWith' About
      li_ $ ahref $ routeWith' Portal
      li_ $ do
        case m_user_pack of
          Nothing               -> ahref $ routeWith' Login
          Just UserResponse{..} -> ahrefName ("Logout: " <> userResponseName) $ routeWith' Logout



renderRouteView :: Store -> ReactElementM ViewEventHandler ()
renderRouteView Store{..} = do
  div_ $ do
    case _route of
      RouteWith Home _                        -> Home.view_
      RouteWith About _                       -> About.view_
      RouteWith Portal _                      -> Portal.view_
      RouteWith (Organizations crud) params   -> Organizations.view_ crud
      RouteWith (Users Index) params          -> Users.view_
      RouteWith (Users crud) params           -> p_ $ elemText "Users crud"
      RouteWith _ _                           -> p_ $ elemText "Unknown"
