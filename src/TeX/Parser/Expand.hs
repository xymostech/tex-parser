module TeX.Parser.Expand
( expand
)
where

import Text.Parsec

import TeX.Parser.Parser
import TeX.Parser.MacroParser
import TeX.Token

expanders :: [TeXParser [Token]]
expanders = [expandMacros]

runExpanders :: [TeXParser [Token]] -> TeXParser [Token]
runExpanders [] = fail "all expanders failed"
runExpanders (expander:rest) =
  expander <|> runExpanders rest

doExpand :: TeXParser ()
doExpand = (runExpanders expanders) >>= prependTokens >> (doExpand <|> return ())

expand :: TeXParser a -> TeXParser a
expand parser =
  (try $ doExpand >> parser) <|> parser <?> "expand"
