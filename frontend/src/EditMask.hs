{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module EditMask where

import Common
import Utils

import Data.Aeson

import Data.Text (Text)
import Data.Text.Encoding as E
import Data.Monoid

import Reflex
import Reflex.Dom
import Control.Monad.IO.Class
import Data.Map (Map)
import qualified Data.Map as Map

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

editMaskWidget evMap = do
  listHoldWithKey Map.empty evMap createEditWidget
  return ()

createEditWidget _ (url, idTxt) = do

  let 
      dilateConf =
        RangeInputConfig 2 never
          (constDyn $ ("min" =: "0") <> ("max" =: "10") <> ("step" =: "1"))

      blurConf =
        RangeInputConfig 2 never
          (constDyn $ ("min" =: "0") <> ("max" =: "10") <> ("step" =: "1"))

  inputs <- el "table" $ do
    el "tr" $ text "Dilate"
    d <- el "tr" $ do
      rangeInput dilateConf 

    el "tr" $ text "Blur"
    b <- el "tr" $ do
      rangeInput blurConf 

    save <- button "Save Mask"
    return (d,b,save)

  let eventMessage = getEventMessage inputs

  ws <- webSocket ("ws://" <> url <> "/edit/mask/" <> idTxt) $ def & webSocketConfig_send .~ eventMessage

  let
    myImgUrl =
      ffor (_webSocket_recv ws)
        (\bs -> liftIO $ createObjectURL bs)

  urlEv <- performEvent myImgUrl
  urlDyn <- holdDyn "dummy" urlEv
  let dynAttr = ffor urlDyn (\u -> ("src" =: u))
  el "div" $ elDynAttr "img" dynAttr $ return ()

  return ()

getEventMessage :: (Reflex t) =>
     (RangeInput t, RangeInput t, Event t ())
  -> Event t [ByteString]
getEventMessage (dilate, blur, save) =
  leftmost $ [saveEv, editEv]
  where
    anyEditEvent = leftmost $ 
      fmap _rangeInput_input
        [dilate, blur]
     
    enc mes = (:[]) <$> BSL.toStrict <$> encode <$> mes
    
    saveEv = enc $ fmap (const ClientReqSaveMask) save
    editEv = enc $ tagDyn message anyEditEvent

    message = ClientReqEditMask
      <$> (ceiling <$> _rangeInput_value dilate)
      <*> (ceiling <$> _rangeInput_value blur)
