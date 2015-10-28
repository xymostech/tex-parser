{-# LANGUAGE Rank2Types #-}

module TeX.Parser.NumberExpander
( expandNumbers
)
where

import TeX.Category
import TeX.Count
import TeX.Parser.Parser
import TeX.Parser.Prim
import TeX.Parser.Util
import TeX.Token

expandNumbers :: Expander -> TeXParser [Token]
expandNumbers expand = do
  _ <- exactToken (ControlSequence "number")
  num <- count expand
  return $ map (\c -> CharToken c Other) $ printCount num
