{-# LANGUAGE OverloadedStrings #-}

-- | A parser's position in the input.

module Text.Parser.Input.Position (Position, fromStart, fromEnd,
                                   offset, context, lineAndColumn) where

import Data.Char (isSpace)
import Data.String (IsString(fromString))
import qualified Data.Monoid.Factorial as Factorial
import qualified Data.Monoid.Textual as Textual
import Data.Monoid.Factorial (FactorialMonoid)
import Data.Monoid.Textual (TextualMonoid)

-- | Opaque data type that represents an input position.
data Position = PositionFromStart !Int
                -- ^ the length of the input from the start to the position
              | PositionFromEnd Int
                -- ^ the length of the input from the position to end
              deriving (Eq, Read, Show)

-- | Construct a 'Position' given the offset from the beginning of the full input.
fromStart :: Int -> Position
fromStart = PositionFromStart

-- | Construct a 'Position' given the length remaining from the position to the end of the input.
fromEnd :: Int -> Position
fromEnd = PositionFromEnd

-- | Map the position into its offset from the beginning of the full input.
--
-- > offset input . fromStart === id
offset :: FactorialMonoid s => s -> Position -> Int
offset wholeInput (PositionFromStart offset) = offset
offset wholeInput (PositionFromEnd remainderLength) = Factorial.length wholeInput - remainderLength
{-# INLINE offset #-}

-- | Given the parser input, a 'Position' within it, and desired number of context lines, returns a description of
-- the offset position in English.
context :: (Eq s, TextualMonoid s) => s -> Position -> Int -> s
context input pos contextLineCount = 
   foldMap (<> "\n") prevLines <> lastLinePadding
   <> "at line " <> fromString (show $ length allPrevLines) <> ", column " <> fromString (show $ column+1) <> "\n"
   where (allPrevLines, column) = lineAndColumn input pos
         lastLinePadding
            | (lastLine:_) <- allPrevLines, paddingPrefix <- Textual.takeWhile_ False isSpace lastLine =
                 Factorial.take column (paddingPrefix <> fromString (replicate column ' ')) <> "^\n"
            | otherwise = ""
         prevLines = reverse (take contextLineCount allPrevLines)

-- | Given the full input and an offset within it, returns all the input lines up to and including the offset
-- in reverse order, as well as the zero-based column number of the offset
lineAndColumn :: (Eq s, IsString s, FactorialMonoid s) => s -> Position -> ([s], Int)
lineAndColumn input pos = context [] (offset input pos) (Factorial.split (== "\n") input)
  where context revLines restCount []
          | restCount > 0 = (["Error: the offset is beyond the input length"], -1)
          | otherwise = (revLines, restCount)
        context revLines restCount (next:rest)
          | restCount' < 0 = (next:revLines, restCount)
          | otherwise = context (next:revLines) restCount' rest
          where nextLength = Factorial.length next
                restCount' = restCount - nextLength - 1
