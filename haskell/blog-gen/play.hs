import Data.Maybe (listToMaybe, isNothing)
import Data.Word (Word8)

repeatedly :: Int -> a -> [a]
repeatedly n item =
  let rfn m acc =
        if m < 1
          then acc
          else rfn (m - 1) (item : acc)
   in rfn n []

repeatedly2 :: Int -> a -> [a]
repeatedly2 n item =
  if n < 1
    then []
    else item : repeatedly2 (n - 1) item

even1 :: Int -> Bool
even1 n = n == 0 || odd1 (n - 1)

odd1 :: Int -> Bool
odd1 n = n /= 0 && even1 (n - 1)

testone :: (Eq a, Num a) => a -> Bool
testone 5 = True
testone 6 = True
testone a = False

testtwo :: [a] -> [a]
testtwo [] = []
testtwo (x : xs) = xs

data Color
  = RGB Word8 Word8 Word8
  deriving (Show)

getBlue :: Color -> Word8
getBlue (RGB r g b) = b

main = putStrLn "Hello! play.hs"

data Brightness
  = Dark
  | Bright

data EightColor
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White

data AnsiColor
  = AnsiColor Brightness EightColor

ansiColorToVGA (AnsiColor Dark Black) = RGB 0 0 0
ansiColorToVGA (AnsiColor Bright Black) = RGB 85 85 85
ansiColorToVGA (AnsiColor Dark Red) = RGB 170 0 0
ansiColorToVGA color =
  case color of
    AnsiColor Bright Red -> RGB 255 85 85
    _ -> RGB 255 255 255

isBright :: AnsiColor -> Bool
isBright (AnsiColor Bright _) = True
isBright _ = False

isEmpty = isNothing . listToMaybe

isEmpty2 [] = True
isEmpty2 _ = False