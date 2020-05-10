module KaTeX.KaTeXify (kaTeXifyIO) where

import System.Process (readCreateProcess, shell)
import Text.Pandoc.Definition (MathType(..), Inline(..), Pandoc, Format(..))
import Text.Pandoc.Readers.HTML (readHtml)
import Text.Pandoc.Options (def)
import Text.Pandoc.Walk (walkM)
import Text.Pandoc.Class (PandocPure, runPure)
import Data.String.Conversions (convertString)

--------------------------------------------------------------------------------
kaTeXCmd :: MathType -> String
kaTeXCmd DisplayMath = "npx katex --display-mode"
kaTeXCmd _           = "npx katex"

rawKaTeX :: MathType -> String -> IO String
rawKaTeX mt inner = readCreateProcess (shell $ kaTeXCmd mt) inner

parseKaTeX :: String -> Maybe Inline
parseKaTeX str =
   case runPure $ readHtml def (convertString str) of
      Right _    -> Just (RawInline (Format "html") str)
      otherwise -> Nothing

kaTeXify :: Inline -> IO Inline
kaTeXify orig@(Math mt str) =
   do
      s <- fmap parseKaTeX $ rawKaTeX mt str
      case s of
         Just inl   -> return inl
         Nothing    -> return orig
kaTeXify x = return x

--------------------------------------------------------------------------------
kaTeXifyIO :: Pandoc -> IO Pandoc
kaTeXifyIO = walkM kaTeXify
