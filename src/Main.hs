-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
----------------------------------------------------------------------------

import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Data.Bits
import Data.Foldable
import Data.Traversable
import Foreign
import Foreign.C.String
import Graphics.Rendering.FreeType.Internal
import Graphics.Rendering.FreeType.Internal.Bitmap
import Graphics.Rendering.FreeType.Internal.Face
import Graphics.Rendering.FreeType.Internal.GlyphSlot
import Graphics.Rendering.FreeType.Internal.PrimitiveTypes
import System.Environment

liftErr :: (MonadError String m) => FT_Error -> m ()
liftErr e
  | e == 0 = pure ()
  | otherwise = throwError $ "FreeType error: " ++ show e

wrapErr :: (MonadIO m,MonadError String m) => IO FT_Error -> m ()
wrapErr a = liftIO a >>= liftErr

main :: IO ()
main = do
  [ttfPath,ptStr,dpiXStr,dpiYStr,paddingStr] <- getArgs
  let pt = read ptStr
      dpiX = read dpiXStr
      dpiY = read dpiYStr
      padding = read paddingStr
  r <- alloca $ \ftlibPtr ->
    alloca $ \facePtr -> runEitherT $ do
      wrapErr $ ft_Init_FreeType ftlibPtr
      ftlib <- liftIO $ peek ftlibPtr
      wrapErr . withCString ttfPath $ \cttfPath ->
        ft_New_Face ftlib cttfPath 0 facePtr
      face <- liftIO $ peek facePtr
      wrapErr $ ft_Set_Char_Size face 0 (pt*64) dpiX dpiY
      alphabet <- liftIO $ fmap lines getContents
      bitmaps <- for alphabet $ \line -> do
        for line $ \c -> do
          liftIO . putStrLn $ "processing char: " ++ show c
          wrapErr $ ft_Load_Char face (fromIntegral $ fromEnum c) (ft_LOAD_RENDER .|. ft_LOAD_MONOCHROME)
          glph <- liftIO . peek $ glyph face
          btmp <- liftIO . peek $ bitmap glph
          unless (pixel_mode btmp == 1) . throwError $ show c ++ " is not encoded as a monochrome bitmap"
          pixels <- liftIO $ extractBitmap btmp
          liftIO $ do
            top <- fmap fromIntegral $ peek $ bitmap_top glph
            left <- peek $ bitmap_left glph
            let rws = fromIntegral $ rows btmp
                w = fromIntegral $ width btmp
            putStrLn $ "rows: " ++ show rws
            putStrLn $ "width: " ++ show w
            putStrLn $ "top: " ++ show top
            putStrLn $ "left: " ++ show left
            pure (rws,w,pixels)
      let (maxRow,_,maxG) = maximumBy (\(a,_,_) (b,_,_) -> compare a b) (concat bitmaps)
          (_,maxWidth,maxGW) = maximumBy (\(_,a,_) (_,b,_) -> compare a b) (concat bitmaps)
          bitmaps' = map (mergeBitmapLine (maxRow + padding) . map (resize (maxRow + padding) (maxWidth + padding))) bitmaps
      for_ bitmaps' $ \line -> do
        for_ line $ \c -> do
          liftIO . putStr $ unlines c
          pure ()
      liftIO $ putStrLn ""
  case r of
    Left err -> putStrLn err
    Right () -> putStrLn "a plus dans l'bus"

unpackPixels :: Word8 -> [Char]
unpackPixels p = map (\x -> if x == 0 then '.' else 'X')
  [
    p .&. 128
  , p .&. 64
  , p .&. 32
  , p .&. 16
  , p .&. 8
  , p .&. 4
  , p .&. 2
  , p .&. 1
  ]

extractBitmap :: FT_Bitmap -> IO [[Char]]
extractBitmap btmp = go 0 (buffer btmp)
  where
    rws = rows btmp
    w = width btmp
    go col buf
      | col < rws = do
          line <- fmap (take (fromIntegral w) . concatMap unpackPixels) $ peekArray (ceiling $ fromIntegral w / 8) (castPtr buf)
          nextLines <- go (succ col) (buf `advancePtr` fromIntegral (pitch btmp))
          pure $ line : nextLines
      | otherwise = pure []

resize :: Int -> Int -> (Int,Int,[[Char]]) -> [[Char]]
resize maxRow maxWidth (rws,w,pixels) = pixels' ++ replicate (maxRow - rws) (replicate maxWidth '.')
  where
    pixels' = map (++ pad) pixels
    pad = replicate (maxWidth - w) '.'

-- Merge a line of bitmaps as one.
mergeBitmapLine :: Int -> [[[Char]]] -> [[[Char]]]
mergeBitmapLine _ [] = []
mergeBitmapLine remainingRows line
  | remainingRows > 0 = [concatMap head line] : mergeBitmapLine (pred remainingRows) (map tail line)
  | otherwise = line
