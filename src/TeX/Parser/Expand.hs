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

expand :: TeXParser ()
expand =
  option () $ (runExpanders expanders) >>= prependTokens >> expand
