module Language.Nabla.SourceSpan  where

import qualified Data.Text as DT

data SourceSpan = SourceSpan
  { sourceName :: FilePath
  , textSpan :: TextSpan
  }
  deriving (Eq)

data TextSpan = TextSpan
  { startPos :: TextPos
  , endPos :: TextPos
  }
  deriving (Eq)

data TextPos = TextPos
  { textLine :: !Int
  , textColumn :: !Int
  }
  deriving (Eq)

instance Show SourceSpan where
  show (SourceSpan name span) = show name <> ":" <> show span

instance Show TextSpan where
  show (TextSpan st ed) = show st <> "-" <> show ed

instance Show TextPos where
  show (TextPos line col) = show line <> ":" <> show col

textSpanPretty :: TextSpan -> DT.Text -> String
textSpanPretty (TextSpan
    (TextPos stLine stCol)
    (TextPos edLine edCol)) src = do
  let ls = map DT.unpack $ slice (stLine - 1) (edLine - 1) $ DT.lines src
  case ls of
    [l] -> do
      let lsWithUnderline = [l, underline (stCol - 1) (edCol - 1)]
      enclose lsWithUnderline
    _ -> enclose ls -- no underline when an error at many lines
  where
    -- slice 1 2 [0, 1, 2, 3] == [1, 2]
    slice :: Int -> Int -> [a] -> [a]
    slice from to xs = take (to - from + 1) (drop from xs)
    -- enclose ["abc", "def"] ==
    -- |\n
    -- | abc\n
    -- | def\n
    enclose :: [String] -> String
    enclose ls = " |\n" <> concat (map (\l -> " | " <> l <> "\n") ls)
    -- underline 3 7 == "   ^^^^"
    underline :: Int -> Int -> String
    underline st ed = take st (repeat ' ') <> take (ed - st) (repeat '^')
