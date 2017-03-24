{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.ReactFlux.App.Profile (
  viewIndex,
  viewEditZ
) where



import           Control.Concurrent                    (forkIO)
import           Control.DeepSeq                       (NFData)
import           Control.Monad                         (forM_, void)
import           Control.Monad.Trans.Either            (EitherT, runEitherT)
import           Data.Ebyam                            (ebyam)
import           Data.Map                              (Map)
import qualified Data.Map                              as Map
import           Data.Monoid                           ((<>))
import           Data.Rehtie                           (rehtie)
import qualified Data.Text                             as Text
import           Data.Text                             (Text)
import           Data.Typeable                         (Typeable)
import           GHC.Generics                          (Generic)
import           Haskell.Helpers.Either                (mustPassT)
import           React.Flux                            hiding (view)
import qualified React.Flux                            as RF
import qualified Web.Bootstrap3                        as B

import           LN.Sanitize.Internal                  (toSafeName)
import           LN.T.Pack.Sanitized.User
import           LN.T.Profile
import           LN.T.Size                             (Size (..))
import           LN.T.User                             (UserSanitizedResponse (..))
import qualified LN.UI.Core.App.Profile                as Profile
import           LN.UI.Core.Helpers.DataList           (deleteNth)
import           LN.UI.Core.Helpers.DataText           (tshow)
import           LN.UI.Core.Helpers.DataTime           (prettyUTCTime)
import           LN.UI.Core.Helpers.HaskellApiHelpers  (rd)
import           LN.UI.Core.Helpers.Map                (idmapFrom)
import           LN.UI.Core.PageInfo                   (PageInfo (..),
                                                        defaultPageInfo,
                                                        pageInfoFromParams,
                                                        paramsFromPageInfo)
import           LN.UI.Core.Router                     (CRUD (..), Params,
                                                        Route (..),
                                                        RouteWith (..),
                                                        TyCRUD (..),
                                                        emptyParams, linkName,
                                                        routeWith, routeWith')
import           LN.UI.ReactFlux.Access
import           LN.UI.ReactFlux.App.Core.Shared
import qualified LN.UI.ReactFlux.App.Delete            as Delete
import qualified LN.UI.ReactFlux.App.Gravatar          as Gravatar
import           LN.UI.ReactFlux.App.Loader            (Loader (..))
import qualified LN.UI.ReactFlux.App.Loader            as Loader
import qualified LN.UI.ReactFlux.App.NotFound          as NotFound
import           LN.UI.ReactFlux.App.PageNumbers       (runPageInfo)
import qualified LN.UI.ReactFlux.App.PageNumbers       as PageNumbers
import           LN.UI.ReactFlux.Helpers.ReactFluxDOM  (ahref, ahrefName,
                                                        className_, classNames_)
import           LN.UI.ReactFlux.Helpers.ReactFluxView (defineViewWithSKey)
import           LN.UI.ReactFlux.Types
import           LN.UI.ReactFlux.View.Button
import           LN.UI.ReactFlux.View.Field
import           LN.UI.ReactFlux.View.Internal         (showTagsSmall)



viewIndex
  :: UserId
  -> Loader (Maybe UserSanitizedPackResponse)
  -> HTMLView_

viewIndex !me_id' !l_m_user' = do
  defineViewWithSKey "users-profile-index-1" (me_id', l_m_user') $ \(me_id, l_m_user) -> do
    viewIndex_ me_id l_m_user



viewIndex_
  :: UserId
  -> Loader (Maybe UserSanitizedPackResponse)
  -> HTMLView_

viewIndex_ !me_id' !l_m_user' = do
  defineViewWithSKey "users-profile-index-2" (me_id', l_m_user') $ \(me_id, l_m_user) -> do
    Loader.maybeLoader1 l_m_user (go me_id)
    where
    go :: UserId -> UserSanitizedPackResponse -> HTMLView_
    go me_id UserSanitizedPackResponse{..} = do
      let
        UserSanitizedResponse{..} = userSanitizedPackResponseUser
        ProfileResponse{..}       = userSanitizedPackResponseProfile

      div_ $ do
        viewIndexHeader userSanitizedPackResponseUser

        viewEditProfile me_id userSanitizedPackResponseUser

        div_ ["key" $= "profile-fields", className_ B.pageHeader] $ do
          h4_ ["key" $= "profile-gender-h4"] $ do
            elemText "Gender: "
            small_ ["key" $= "profile-gender-sm"] $ elemShow profileResponseGender

          h4_ ["key" $= "profile-birthday-h4"] $ do
            elemText "Birthday:"
            small_ ["key" $= "profile-birthday-sm"] $ elemText $ prettyUTCTime profileResponseBirthdate

          h4_ ["key" $= "profile-website-h4"] $ do
            elemText "Website: "
            small_ ["key" $= "profile-website-sm"] $ elemText $ do
              ebyam profileResponseWebsite "No website." id

          h4_ ["key" $= "profile-location-h4"] $ do
            elemText "Location: "
            small_ ["key" $= "profile-location-sm"] $ elemText $ do
              ebyam profileResponseLocation "No Location." id

          h4_ ["key" $= "profile-signature-h4"] $ do
            elemText "Signature: "
            small_ ["key" $= "profile-signature-sm"] $ elemText $ do
              ebyam profileResponseSignature "No signature." id

          h4_ ["key" $= "profile-debug-h4"] $ do
            elemText "Debug: "
            small_ ["key" $= "profile-debug-sm"] $ elemShow profileResponseDebug

    go _ _ = NotFound.view



-- | Simply create a profile header
--
viewIndexHeader :: UserSanitizedResponse -> HTMLView_
viewIndexHeader !user' =
  defineViewWithSKey "profile-index-header" user' go
  where
  go UserSanitizedResponse{..} = do
    cldiv_ B.containerFluid $ do
      cldiv_ B.pageHeader $ do
        h1_ [className_ B.textCenter] $ elemText userSanitizedResponseDisplayName



-- | Simply adds an "Edit profile" link, if we have access
--
viewEditProfile :: UserId -> UserSanitizedResponse -> HTMLView_
viewEditProfile !me_id' !lookup_user' =
  defineViewWithSKey "profile-edit" (me_id', lookup_user') go
  where
  go :: (UserId, UserSanitizedResponse) -> HTMLView_
  go (me_id, UserSanitizedResponse{..}) = do
    ifte_Self me_id userSanitizedResponseId
      (ahref $ routeWith' $ UsersProfile userSanitizedResponseName EditZ)
      mempty



viewEditZ
  :: Loader (Maybe UserSanitizedPackResponse)
  -> Maybe ProfileRequest
  -> HTMLView_

viewEditZ !l_m_user_pack' !m_request' =
  Loader.maybeLoader1 l_m_user_pack' $ \user_pack -> do
    case (m_request') of
      Just request -> go TyUpdate user_pack request
      _ -> mempty

  where
  go !tycrud' !user_pack' !request' = do
    defineViewWithSKey "users-profile-edit-1" (tycrud', user_pack', request') $ \(tycrud, user_pack, request) -> do

      let
        UserSanitizedPackResponse {..} = user_pack
        ProfileResponse{..}            = userSanitizedPackResponseProfile
        ProfileRequest{..}             = request

      div_ $ do
        h1_ $ elemText $ linkName tycrud <> " User Profile"

        mandatoryGenderField profileRequestGender
          (dispatch . Profile.setGender request)

        optionalLabelField "Website" profileRequestWebsite
          (dispatch . Profile.setWebsite request)
          (dispatch $ Profile.clearWebsite request)

        optionalLabelField "Location" profileRequestLocation
          (dispatch . Profile.setLocation request)
          (dispatch $ Profile.clearLocation request)

        optionalLabelField "Signature" profileRequestSignature
          (dispatch . Profile.setSignature request)
          (dispatch $ Profile.clearSignature request)

        mandatoryBooleanYesNoField "Debug" profileRequestDebug False
          (dispatch . Profile.setDebug request)

        createButtonsCreateEditCancel
          (Just profileResponseId)
          (dispatch Save)
          (const $ dispatch Save)
          (routeWith' Home)
