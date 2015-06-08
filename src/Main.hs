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

import qualified Codec.Picture as JP
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
import qualified Graphics.Rendering.FreeType.Internal as FT
import qualified Graphics.Rendering.FreeType.Internal.Bitmap as FT
import qualified Graphics.Rendering.FreeType.Internal.Face as FT
import qualified Graphics.Rendering.FreeType.Internal.GlyphSlot as FT
import qualified Graphics.Rendering.FreeType.Internal.PrimitiveTypes as FT
import System.Environment

-- Get the alphabet from MonadIO m.
--
-- Because of the format of the input, it’s possible that holes exist in the
-- alphabet, as following:
--
--  abcdef
--  ghijkl
--  mno
--
-- In that case, the last line has three symbols missing to complete the line.
-- That function is not responsible of taking care of any kind of padding.
-- See 'padAlphabet' for such a use.
getAlphabet :: (MonadIO m) => m [[Char]]
getAlphabet = liftIO $ fmap lines getContents

-- Pad the alphabet by inserting '\0' at end of incomplete lines.
padAlphabet :: [[Char]] -> [[Char]]
padAlphabet alpha = map fillGaps alpha
  where
    fillGaps line = line ++ replicate (maxWidth - length line) '\0'
    maxWidth = maximum $ map length alpha

liftErr :: (MonadError String m) => FT.FT_Error -> m ()
liftErr e
  | e == 0 = pure ()
  | otherwise = throwError $ "FreeType error: " ++ show e

wrapErr :: (MonadIO m,MonadError String m) => IO FT.FT_Error -> m ()
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
      wrapErr $ FT.ft_Init_FreeType ftlibPtr
      ftlib <- liftIO $ peek ftlibPtr
      wrapErr . withCString ttfPath $ \cttfPath ->
        FT.ft_New_Face ftlib cttfPath 0 facePtr
      face <- liftIO $ peek facePtr
      wrapErr $ FT.ft_Set_Char_Size face 0 (pt*64) dpiX dpiY
      alphabet <- fmap padAlphabet getAlphabet
      let alphabetSize = length alphabet
      bitmaps <- for alphabet $ \line -> do
        for line $ \c -> do
          liftIO . putStrLn $ "processing char: " ++ show c
          wrapErr $ FT.ft_Load_Char face (fromIntegral $ fromEnum c) (FT.ft_LOAD_RENDER .|. FT.ft_LOAD_MONOCHROME)
          glyph <- liftIO . peek $ FT.glyph face
          bitmap <- liftIO . peek $ FT.bitmap glyph
          unless (FT.pixel_mode bitmap == 1) . throwError $ show c ++ " is not encoded as a monochrome bitmap"
          pixels <- liftIO $ extractBitmap bitmap
          liftIO $ do
            top <- fmap fromIntegral $ peek $ FT.bitmap_top glyph
            left <- peek $ FT.bitmap_left glyph
            let rows = fromIntegral $ FT.rows bitmap
                width = fromIntegral $ FT.width bitmap
            putStrLn $ "rows: " ++ show rows
            putStrLn $ "width: " ++ show width
            putStrLn $ "top: " ++ show top
            putStrLn $ "left: " ++ show left
            pure (rows,width, pixels)
      let (maxRow,_,maxG) = maximumBy (\(a,_,_) (b,_,_) -> compare a b) (concat bitmaps)
          (_,maxWidth,maxGW) = maximumBy (\(_,a,_) (_,b,_) -> compare a b) (concat bitmaps)
          bitmaps' = map (mergeBitmapLine (maxRow + padding) . map (resize (maxRow + padding) (maxWidth + padding))) bitmaps
      liftIO $ JP.savePngImage output (JP.ImageY8 $ turnToFontmap (length alphabet * (maxRow + padding)) (length (alphabet !! 0) * (maxWidth + padding)) bitmaps')
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

extractBitmap :: FT.FT_Bitmap -> IO [[Word8]]
extractBitmap bitmap = go 0 (FT.buffer bitmap)
  where
    rows = FT.rows bitmap
    width = FT.width bitmap 
    go col buf
      | col < rows = do
          line <- fmap (take (fromIntegral width) . concatMap unpackPixels) $ peekArray (ceiling $ fromIntegral width / 8) (castPtr buf)
          nextLines <- go (succ col) (buf `advancePtr` fromIntegral (FT.pitch bitmap))
          pure $ line : nextLines
      | otherwise = pure []

resize :: Int -> Int -> (Int,Int,[[Word8]]) -> [[Word8]]
resize maxRow maxWidth (rows,width,pixels) = pixels' ++ replicate (maxRow - rows) (replicate maxWidth 0)
  where
    pixels' = map (++ pad) pixels
    pad = replicate (maxWidth - width) 0

-- FIXME
-- Merge a line of bitmaps as one.
mergeBitmapLine :: Int -> [[[Word8]]] -> [[[Word8]]]
mergeBitmapLine _ [] = []
mergeBitmapLine remainingRows line
  | remainingRows > 0 = [concatMap head line] : mergeBitmapLine (pred remainingRows) (map tail line)
  | otherwise = []

-- Build an image out of the rendered glyphs
turnToFontmap :: Int -> Int -> [[[[Word8]]]] -> JP.Image JP.Pixel8
turnToFontmap rows width glyphs = JP.Image width rows (fromList . concat . concat $ concat glyphs)
