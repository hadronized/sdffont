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
import Data.Monoid
import Data.Traversable
import Data.Vector as V ( Vector, (!), cons, convert, fromList, singleton )
import qualified Data.Vector as V ( concat, concatMap, head, replicate, tail )
import qualified Data.Vector.Storable as SV ( Vector, concat )
import Foreign
import Foreign.C.String
import qualified Graphics.Rendering.FreeType.Internal as FT
import qualified Graphics.Rendering.FreeType.Internal.Bitmap as FT
import qualified Graphics.Rendering.FreeType.Internal.Face as FT
import qualified Graphics.Rendering.FreeType.Internal.GlyphSlot as FT
import qualified Graphics.Rendering.FreeType.Internal.PrimitiveTypes as FT
import System.Environment

-- Pixel in the fontmap. [Pixel] is a line of pixels, but Vector Pixel might be
-- prefered though for O(1) on random access.
type Pixel = Word8

-- A bitmap.
type Bitmap = Vector (Vector Pixel)

-- A glyph is represented as several pixel lines along with information about
-- size and offset (width,rows,top,left,pixels).
type Glyph = (Int,Int,Int,Int,Bitmap)

-- A fontmap is encoded as several glyph lines.
type Fontmap = Vector (Vector Bitmap)

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

-- Lift a FreeType error into 'MonadError String'.
liftErr :: (MonadError String m) => FT.FT_Error -> m ()
liftErr e
  | e == 0 = pure ()
  | otherwise = throwError $ "FreeType error: " ++ show e

-- That function can be used to turn FreeType functions that return
-- errors into 'MonadError String m' actions.
wrapErr :: (MonadIO m,MonadError String m) => IO FT.FT_Error -> m ()
wrapErr a = liftIO a >>= liftErr

-- Process a font, given by a 'FilePath'. The alphabet, a '[[Char]]'', is
-- provided to select the symbols that have to be written to the output. The
-- output is expressed as a 'String' and refers to both the .png image and the
-- JSON font setup file. Are also needed the points used to render each
-- symbol, the resolution in dpi and the padding to add around each glyphs.
processFont :: FilePath
            -> [[Char]]
            -> String
            -> Int
            -> Int
            -> Int
            -> Int
            -> IO (Either String ())
processFont fontPath alphabet output pt dpix dpiy padding = do
  traverse_ putStrLn alphabet
  alloca $ \ftlibPtr ->
    alloca $ \facePtr -> runEitherT $ do
      wrapErr $ FT.ft_Init_FreeType ftlibPtr
      ftlib <- liftIO $ peek ftlibPtr
      wrapErr . withCString fontPath $ \cfontPath ->
        FT.ft_New_Face ftlib cfontPath 0 facePtr
      face <- liftIO $ peek facePtr
      wrapErr $ FT.ft_Set_Char_Size face 0 (fromIntegral pt * 64)
        (fromIntegral dpix) (fromIntegral dpiy)
      fontmap <- traverse (traverse $ createGlyph face) alphabet'
      let bitmaps = fmap (mergeBitmapLine maxRows . fmap (\(w,h,_,_,p) -> resizeBitmap w h maxWidth maxRows p)) fontmap
          fontmap' = V.concat (toList fontmap)
          (maxWidth,_,_,_,_) = maximumBy (\(a,_,_,_,_) (b,_,_,_,_) -> compare a b) fontmap'
          (_,maxRows,_,_,_) = maximumBy (\(_,a,_,_,_) (_,b,_,_,_) -> compare a b) fontmap'
      liftIO . putStrLn $ "max width: " ++ show maxWidth
      liftIO . putStrLn $ "max height: " ++ show maxRows
      liftIO . JP.savePngImage output . JP.ImageY8 $ fontmapToImage
        (maxWidth * length (head alphabet)) (maxRows * length alphabet) bitmaps
  where
    alphabet' = fromList $ (map fromList) alphabet

-- Create a new glyph by rendering it via FreeType.
createGlyph :: (MonadIO m,MonadError String m)
            => FT.FT_Face
            -> Char
            -> m Glyph
createGlyph face c = do
  liftIO . putStrLn $ "processing character " ++ show c
  wrapErr $ FT.ft_Load_Char face (fromIntegral $ fromEnum c)
    (FT.ft_LOAD_RENDER .|. FT.ft_LOAD_MONOCHROME)
  glyph <- liftIO . peek $ FT.glyph face
  bitmap <- liftIO . peek $ FT.bitmap glyph
  unless (FT.pixel_mode bitmap == 1) . throwError $ show c ++
    " is not encoded as a monochrome bitmap"
  liftIO $ do
    pixels <- extractBitmap bitmap
    top <- fmap fromIntegral . peek $ FT.bitmap_top glyph
    left <- fmap fromIntegral . peek $ FT.bitmap_left glyph
    let rows = fromIntegral $ FT.rows bitmap
        width = fromIntegral $ FT.width bitmap
    putStrLn $ "  width: " ++ show width
    putStrLn $ "  rows: " ++ show rows
    putStrLn $ "  top: " ++ show top
    putStrLn $ "  left: " ++ show left
    pure (width,rows,top,left,pixels)

-- Extract the bitmap out of a 'Glyph'.
glyphBitmap :: Glyph -> Bitmap
glyphBitmap (_,_,_,_,bitmap) = bitmap

-- Get the minimum and maximum in a foldable in a single pass using an ordering
-- function.
minmaxBy :: (Bounded a,Foldable f)
         => (a -> a -> Ordering)
         -> f a
         -> (a,a)
minmaxBy f = foldl' (\(!n,!m) a -> (min' a n,max' m a)) (maxBound,minBound)
  where
    min' !a !b = case f a b of
      LT -> a
      _  -> b
    max' !a !b = case f a b of
      GT -> a
      _  -> b

-- Get the minimum and maximum in a foldable in a single pass.
minmax :: (Bounded a,Ord a,Foldable f) => f a -> (a,a)
minmax = minmaxBy compare

-- Unpack pixels out of a byte. That function is required because FreeType
-- packs pixels in bytes. A byte then contains 8 pixels. The generated pixels
-- in an unnormalized monochrome color space (0-255) and are either 0 (white) or
-- black (255).
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

-- Given a FreeType glyph bitmap, that function extracts a Bitmap.
extractBitmap :: FT.FT_Bitmap -> IO Bitmap
extractBitmap bitmap = fmap fromList $ go 0 (FT.buffer bitmap)
  where
    rows = FT.rows bitmap
    width = FT.width bitmap
    go col buf
      | col < rows = do
          line <- fmap (fromList . take (fromIntegral width) . concatMap unpackPixels) $ peekArray (ceiling $ fromIntegral width / 8) (castPtr buf)
          nextLines <- go (succ col) (buf `advancePtr` fromIntegral (FT.pitch bitmap))
          pure $ line : nextLines
      | otherwise = pure []

-- 'resizeBitmap maxWidth maxRow bitmap' resizes 'bitmap' by resizing it
-- regarding the 'maxWidth' and 'maxRows' arguments.
resizeBitmap :: Int -> Int -> Int -> Int -> Bitmap -> Bitmap
resizeBitmap width rows maxWidth maxRow bitmap = pixels' <> V.replicate (maxRow - rows) (V.replicate maxWidth 0)
  where
    pixels' = fmap (<> pad) bitmap
    pad = V.replicate (maxWidth - width) 0

-- TODO: get rid of the singleton
-- Merge a line of Bitmaps.
mergeBitmapLine :: Int -> Vector Bitmap -> Vector Bitmap 
mergeBitmapLine remainingRows line
  | line == mempty = mempty
  | remainingRows > 0 = fmap V.head line `cons` mergeBitmapLine (pred remainingRows) (fmap V.tail line)
  | otherwise = mempty

-- Build an image out of the rendered glyphs. That function expects the width
-- and rows for a given bitmap
fontmapToImage :: Int -> Int -> Fontmap -> JP.Image JP.Pixel8
fontmapToImage width rows fontmap = JP.Image width rows (convert . V.concat . toList . V.concat . toList . V.concat $ toList fontmap)

main :: IO ()
main = do
  [fontPath,ptStr,dpiXStr,dpiYStr,paddingStr,output] <- getArgs
  let pt = read ptStr
      dpix = read dpiXStr
      dpiy = read dpiYStr
      padding = read paddingStr
  alphabet <- fmap padAlphabet getAlphabet
  putStrLn "alphabet is:"
  traverse_ (putStrLn . ("  "++)) alphabet
  r <- processFont fontPath alphabet output pt dpix dpiy padding
  case r of
    Left err -> putStrLn err
    Right () -> putStrLn "a plus dans l'bus"
