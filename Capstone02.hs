module Capstone03 where

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
