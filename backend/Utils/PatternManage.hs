module Utils.PatternManage where

import Import
import System.Directory
import System.FilePath.Posix
import Diagrams.TwoD.Image
import Diagrams.Backend.Rasterific
import Diagrams.Prelude hiding (render)
import qualified Diagrams.TwoD.Size as Dia
import Message
import Common

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import System.Random

import qualified Data.Text as T
import Debug.Trace

-- getPatternList :: IO (Response)
getPatternList = do
  let path = "./" ++ T.unpack patternsDir
  groups <- listDirectory $ path

  v <- PatternList <$> forM groups
    (\folder -> do
      files <- listDirectory (path </> folder)
      return (T.pack folder, map T.pack $
                filter (\f -> takeExtension f == ".png") files))
  return (Debug.Trace.trace (show v) v)

getPatternDia :: PatternName -> IO (Maybe (Diagram Rasterific))
getPatternDia (d,f) = do
  e <- loadImageEmb (T.unpack ("./" <> patternsDir <> d <> "/" <> f))
  return $ case e of
    Left _ -> Nothing
    Right dimg -> Just $ image dimg

getPatternsDia :: NonEmpty PatternName ->
  IO (Maybe (NonEmpty (Diagram Rasterific)))
getPatternsDia pats = do
  dias <- mapM getPatternDia $ (Debug.Trace.trace $ "Find Pats:" ++ show pats) (NE.toList pats)
  let patsFound = Debug.Trace.trace ("Pats Found:" ++ show val)  val
      val = all isJust dias
      p = NE.fromList $ catMaybes dias

  if patsFound
    then return $ Just p
    else return Nothing

scaledImgWidth = 50

getPatternsDiaScaled :: NonEmpty PatternName ->
  IO (Maybe (NonEmpty (Diagram Rasterific)))
getPatternsDiaScaled pats = do
  dias <- getPatternsDia pats

  let getScaledDias dia =
        if scaling < 1.0
          then scale scaling dia
          else dia
        where
          dw = (Dia.width dia)
          dh = (Dia.height dia)

          scaling = scaledImgWidth/dw

  return $ (fmap getScaledDias) <$> dias

savePng ::
     Maybe (Text, Text) -- File name
  -> ByteString
  -> IO (Text)  -- File name
savePng names bs = do
  fullFileName <- case names of
    Just (dir', fileName') -> do

      let fullFileName = dir ++ fileName
          dir = T.unpack dir'
          fileName = (T.unpack fileName') ++ ".png"
      createDirectoryIfMissing True ("./" ++ dir)

      return (fullFileName)
    Nothing -> do
      rnd <- randomRIO (0,maxBound::Int)
      let fullFileName = (T.unpack previewDir) ++ fileName
          fileName = show rnd ++ ".png"
      return fullFileName
  writeFile ("./" ++ fullFileName) bs
  return $ T.pack $ fullFileName
