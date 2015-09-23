{-# LANGUAGE Rank2Types #-}

module TeX.Parser.Expand
( expand
)
where

import Text.Parsec

import TeX.Parser.MacroParser
import TeX.Parser.Parser
import TeX.Token

type TokenExpander = Expander -> TeXParser [Token]

expanders :: [TokenExpander]
expanders = [expandMacro]

runExpanders :: [TokenExpander] -> TeXParser [Token]
runExpanders [] = fail "all expanders failed"
runExpanders (expander:rest) =
  expander expand <|> runExpanders rest

doExpand :: TeXParser ()
doExpand = (runExpanders expanders) >>= prependTokens >> (doExpand <|> return ())

expand :: Expander
expand parser =
  (try $ doExpand >> parser) <|> parser <?> "expand"
