{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE PartialTypeSignatures   #-}
module Handler.EditFGTemplate where

import Import
import Yesod.WebSockets
import Utils.FrameCreator
import Utils.PatternManage
import Utils.Misc
import Common
import Message

import qualified Data.Map as Map
import Control.Monad
import Data.Conduit
import qualified Data.Conduit.List
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Diagrams.Prelude (Diagram)
import Diagrams.Backend.Rasterific (Rasterific)
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Data.Aeson
import qualified Control.Lens

getEditForeGroundR :: ForeGroundTemplateDBId -> Handler Html
getEditForeGroundR fgtID = do
  $logInfo $ "Edit foreground template"

  fgt <- runDB $ get fgtID
  let
    fgtData :: Maybe ForeGroundData
    fgtData = join $ decodeStrict <$>
              foreGroundTemplateDBData <$> fgt
    layerData = ((\(ForeGroundData l _) -> l) <$> fgtData)
    maskParam = ((\(ForeGroundData _ m) -> m) <$> fgtData)
    pats = (map fst) <$> layerData
  dias <- liftIO $ mapM getPatternsDiaScaled pats
  backImg <- liftIO $ getBackgroundImage
  $logInfo $ (tshow $ isJust (join dias))
  $logInfo $ (tshow $ fgtData)
  mapM webSockets (webSocketServer fgtID <$> backImg <*> layerData <*> maskParam <*> (join dias))
  $logInfo $ "Not doing websocket"
  redirect HomeR

webSocketServer fgtID backImg fgtData' maskParam dias' = do
  $logInfo $ "Doing edit foreground template"

  fgtDataRef <- liftIO $ newIORef fgtData'
  maskParamRef <- liftIO $ newIORef maskParam
  diasRef <- liftIO $ newIORef dias'
  maskParamRef <- liftIO $ newIORef maskParam
  imgDataRef <- liftIO $ newIORef ""
  isZoom <- liftIO $ newIORef False
  showPreview <- liftIO $ newIORef False

  sourceWS $$ Data.Conduit.List.mapMaybeM
    (handleRequest fgtDataRef diasRef maskParamRef imgDataRef
     isZoom showPreview)
    =$= sinkWSBinary
  where
    mylift :: Handler a -> ReaderT r (HandlerT App IO) a
    mylift a = lift $ a
    handleRequest ::
         IORef (NonEmpty (PatternName, ForeGroundParams))
      -> IORef (NonEmpty (Diagram Rasterific))
      -> IORef (MaskParams)
      -> IORef (BSL.ByteString)
      -> IORef (Bool)
      -> IORef (Bool)
      -> BSL.ByteString
      -> ReaderT r (HandlerT App IO) (Maybe BSL.ByteString)
    handleRequest fgtDataRef diasRef maskParamRef imgDataRef isZoom showPreview req' = do
      $logInfo $ T.pack $ "Doing Edit" ++ show req'
      case decode req' of
        Just (Edit layerId params) -> do
          d <- liftIO $ readIORef fgtDataRef
          dias <- liftIO $ readIORef diasRef
          m <- liftIO $ readIORef maskParamRef
          z <- liftIO $ readIORef isZoom
          p <- liftIO $ readIORef showPreview
          -- Better method?
          let newD = Control.Lens.imap
                (\i a@(p,_) -> if (i + 1) == layerId
                  then (p,params)
                  else a) d

              resImg =
                renderAndGetPngData backImg z p m newD dias

          liftIO $ writeIORef imgDataRef resImg
          liftIO $ writeIORef fgtDataRef newD
          $logInfo "Done EditLayer"
          return (Just resImg)

        Just (AddLayer pat) -> do
          d <- liftIO $ readIORef fgtDataRef
          dias <- liftIO $ readIORef diasRef
          m <- liftIO $ readIORef maskParamRef
          z <- liftIO $ readIORef isZoom
          p <- liftIO $ readIORef showPreview
          dia <- liftIO $ getPatternDiaScaled pat

          let newD = appendNE d (pat, defFGParams)
              defFGParams = def :: ForeGroundParams

              newDias = appendNE dias <$> dia

              resImg =
                renderAndGetPngData backImg z p m newD <$> newDias

          forM resImg $ const $ do
            liftIO $ mapM (writeIORef diasRef) newDias
            liftIO $ writeIORef fgtDataRef newD

          $logInfo "Done addLayer"
          liftIO $ mapM (writeIORef imgDataRef) resImg
          return resImg

        Just (DeleteLayer layerId) -> do
          d <- liftIO $ readIORef fgtDataRef
          dias <- liftIO $ readIORef diasRef
          m <- liftIO $ readIORef maskParamRef
          z <- liftIO $ readIORef isZoom
          p <- liftIO $ readIORef showPreview

          let newD = NE.fromList $
                (NE.take (layerId - 1) d) ++ (NE.drop layerId d)

              newDias = NE.fromList $
                (NE.take (layerId - 1) dias) ++ (NE.drop layerId dias)

              resImg =
                renderAndGetPngData backImg z p m newD newDias

          liftIO $ writeIORef diasRef newDias
          liftIO $ writeIORef fgtDataRef newD

          return $ Just resImg

        Just SaveFG -> do
          d <- liftIO $ readIORef fgtDataRef
          imgData <- liftIO $ readIORef imgDataRef
          mylift $ runDB $ update fgtID
            [ForeGroundTemplateDBData =. (BSL.toStrict $ encode d)]
          liftIO $ savePng (Just (fgtemplatesDir, tshow (fromSqlKey fgtID))) $
            BSL.toStrict imgData
          return Nothing

        Just ToggleZoom -> do
          modifyIORef isZoom not
          d <- liftIO $ readIORef fgtDataRef
          dias <- liftIO $ readIORef diasRef
          m <- liftIO $ readIORef maskParamRef
          z <- liftIO $ readIORef isZoom
          p <- liftIO $ readIORef showPreview
          let resImg = renderAndGetPngData backImg z p m d dias
          return (Just resImg)

        Just TogglePreview -> do
          modifyIORef showPreview not
          p <- liftIO $ readIORef showPreview
          when p (writeIORef isZoom False)

          d <- liftIO $ readIORef fgtDataRef
          dias <- liftIO $ readIORef diasRef
          z <- liftIO $ readIORef isZoom
          m <- liftIO $ readIORef maskParamRef
          let resImg = renderAndGetPngData backImg z p m d dias
          return (Just resImg)

        Just (EditMask maskParams) -> do
          writeIORef maskParamRef maskParams
          p <- liftIO $ readIORef showPreview
          d <- liftIO $ readIORef fgtDataRef
          dias <- liftIO $ readIORef diasRef
          z <- liftIO $ readIORef isZoom
          m <- liftIO $ readIORef maskParamRef
          let resImg = renderAndGetPngData backImg z p m d dias
          return (Just resImg)

        _ -> return Nothing

renderAndGetPngData backImg z p maskParams d dias = encodePng $
  if p
    then createFrame backImg resImg maskParams 600
    else resImg
  where
    resDia = fgDrawFun dias (NE.map snd d)
    resImg = renderFun resDia 600
    renderFun =
      if z || p
        then renderWithAA
        else render
    fgDrawFun =
      if z && not p
        then getQuarterForeGround
        else getForeGround

-- editMaskWebSocketWidget appSt fgd = do
--   $logInfo $ "edit mask: Websocket version"

--   maskRef <- liftIO $ do
--     fg <- readMVar (foreGround fgd)

--     let (_,_,_,msk) =
--           getMask (foreGroundDia fg)
--             previewSize (MaskParams 2 2)
--     newIORef $
--       Mask (MaskParams 2 2) msk

--   sourceWS $$ Data.Conduit.List.mapMaybeM
--       (handleRequest maskRef)
--     =$= sinkWSBinary
--   where
--     handleRequest :: (MonadIO m, MonadLogger m)
--       => IORef Mask
--       -> BS.ByteString
--       -> m (Maybe BS.ByteString)
--     handleRequest maskRef req' = do
--       case decodeStrict' req' of
--         Just (ClientReqSaveMask) -> do
--           liftIO $ do
--             msk <- liftIO $ readIORef maskRef
--             tryTakeMVar (AppData.mask fgd) -- discard old
--             putMVar (AppData.mask fgd) msk
--           return Nothing

--         Just (ClientReqEditMask maskParams) -> do
--           $logInfo $ "edit mask: Valid request"
--           fg <- liftIO $ readMVar (foreGround fgd)

--           let
--               (_,_,subt,msk) =
--                 getMask (foreGroundDia fg)
--                   previewSize maskParams

--           liftIO $ liftIO $ writeIORef maskRef (Mask maskParams msk)
--           return $ Just subt

--         _ -> return Nothing
