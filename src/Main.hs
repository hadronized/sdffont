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

import Codec.Picture
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Data.Bits
import Data.Foldable
import Data.Traversable
import Data.Vector.Storable ( fromList )
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
  [ttfPath,ptStr,dpiXStr,dpiYStr,paddingStr,output] <- getArgs
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
      fc <- liftIO $ peek facePtr
      wrapErr $ ft_Set_Char_Size fc 0 (pt*64) dpiX dpiY
      alphabet <- liftIO $ fmap lines getContents
      let alphabetSize = length alphabet
      bitmaps <- for alphabet $ \line -> do
        for line $ \c -> do
          liftIO . putStrLn $ "processing char: " ++ show c
          wrapErr $ ft_Load_Char fc (fromIntegral $ fromEnum c) (ft_LOAD_RENDER .|. ft_LOAD_MONOCHROME)
          glph <- liftIO . peek $ glyph fc
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
            pure (rws,w, pixels)
      let (maxRow,_,maxG) = maximumBy (\(a,_,_) (b,_,_) -> compare a b) (concat bitmaps)
          (_,maxWidth,maxGW) = maximumBy (\(_,a,_) (_,b,_) -> compare a b) (concat bitmaps)
          bitmaps' = map (mergeBitmapLine (maxRow + padding) . map (resize (maxRow + padding) (maxWidth + padding))) bitmaps
      liftIO $ savePngImage output (ImageY8 $ turnToFontmap (length alphabet * (maxRow + padding)) (length (alphabet !! 0) * (maxWidth + padding)) bitmaps')
  case r of
    Left err -> putStrLn err
    Right () -> putStrLn "a plus dans l'bus"

unpackPixels :: Word8 -> [Word8]
unpackPixels p = map (\x -> if x == 0 then 0 else 255)
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

extractBitmap :: FT_Bitmap -> IO [[Word8]]
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

resize :: Int -> Int -> (Int,Int,[[Word8]]) -> [[Word8]]
resize maxRow maxWidth (rws,w,pixels) = pixels' ++ replicate (maxRow - rws) (replicate maxWidth 0)
  where
    pixels' = map (++ pad) pixels
    pad = replicate (maxWidth - w) 0

-- FIXME
-- Merge a line of bitmaps as one.
mergeBitmapLine :: Int -> [[[Word8]]] -> [[[Word8]]]
mergeBitmapLine _ [] = []
mergeBitmapLine remainingRows line
  | remainingRows > 0 = [concatMap head line] : mergeBitmapLine (pred remainingRows) (map tail line)
  | otherwise = []

-- Build an image out of the rendered glyphs
turnToFontmap :: Int -> Int -> [[[[Word8]]]] -> Image Pixel8
turnToFontmap rowNb w glyphs = Image w rowNb (fromList . concat . concat $ concat glyphs)
