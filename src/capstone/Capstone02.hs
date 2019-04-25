module Capstone03 where
import Data.List

data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
    where rotation = offset `mod` alphabetSize
          offset    = fromEnum c + halfAlphabet
          halfAlphabet = alphabetSize `div` 2

rotChar :: Char -> Char
rotChar charToEncrypt = rotN sizeOfAlphabet charToEncrypt
    where sizeOfAlphabet = 1 + fromEnum (maxBound :: Char)

message :: [FourLetterAlphabet]
message = [L1, L3, L4, L1, L1, L2]

fourLetterAlphabetEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphabetEncoder vals = map rot4l vals
    where rot4l = rotN alphaSize
          alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)

data ThreeLetterAlphabet = Alpha | Beta | Kappa deriving (Show, Enum, Bounded)

threeLetterMessage :: [ThreeLetterAlphabet]
threeLetterMessage = [Alpha, Alpha, Beta, Alpha, Kappa]

threeLetterEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterEncoder = map rot3l
   where rot3l = rotN alphaSize
         alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)

rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
    where rotation = offset `mod` n
          offset = if even n
                   then fromEnum c + halfN
                   else 1 + fromEnum c + halfN
          halfN = n `div` 2              

threeLetterDecoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterDecoder vals = map rot3ldecoder vals
    where rot3ldecoder = rotNdecoder alphaSize
          alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)

rotEncoder :: String -> String
rotEncoder text = map rotChar text
    where rotChar = rotN alphaSize
          alphaSize = 1 + fromEnum (maxBound :: Char)
          
rotDecoder :: String -> String
rotDecoder text = map rotCharDecoder text
    where rotCharDecoder = rotNdecoder alphaSize
          alphaSize = 1 + fromEnum (maxBound :: Char)

xorBool :: Bool -> Bool -> Bool
xorBool a b = (a || b) && (not (a && b))

xorPair :: (Bool, Bool) -> Bool
xorPair (a, b) = a `xorBool` b

xor :: [Bool] -> [Bool] -> [Bool]
xor xs ys = map xorPair (xs `zip` ys)

type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n =  (remainder == 1) : intToBits' nextValue
    where remainder = n `mod` 2
          nextValue = n `div` 2
                  
maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
    where leadingFalses = take missingBits (cycle [False])
          missingBits = maxBits - (length reversedBits)
          reversedBits = reverse (intToBits' n)

charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char)

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (2^) trueLocations)
    where trueLocations = map snd $ filter fst ((reverse bits) `zip` [0..])

bitsToChar :: Bits -> Char
bitsToChar = toEnum . bitsToInt

myPad :: String
myPad = "Shhhhhh"

myPlainText :: String
myPlainText = "Haskell"

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plainText = map (\(a, b) -> a `xor` b) (padBits `zip` plainTextBits)
    where padBits = map charToBits pad
          plainTextBits = map charToBits plainText

applyOTP :: String -> String -> String
applyOTP pad plainText = map bitsToChar bitList
    where bitList = applyOTP' pad plainText

encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad

class Chiper a where
  encode :: a -> String -> String
  decode :: a -> String -> String

data Rot = Rot

instance Chiper Rot where
  encode Rot = rotEncoder
  decode Rot = rotDecoder

data OneTimePad = OTP String

instance Chiper OneTimePad where
  encode (OTP pad) = applyOTP pad
  decode = encode

myOTP :: OneTimePad
myOTP = OTP (cycle [minBound..maxBound])

prng :: Int -> Int -> Int -> Int -> Int
prng a b maxNumber seed = (a*seed + b) `mod` maxNumber

  
