module TeX.Parser.Prim
where

import Prelude ( Maybe(Just, Nothing)
               , show, otherwise
               , (==), (<*), (++)
               )
import Text.Parsec (tokenPrim, getState, putState, (<?>))

import TeX.Parser.Parser
import TeX.Token
import TeX.Category

runGrouped :: TeXParser a -> TeXParser a
runGrouped p = do
  oldState <- getState
  p <* (putState oldState)

tokenWithFunc :: (Token -> Maybe Token) -> TeXParser Token
tokenWithFunc func =
  tokenPrim showToken nextPos func
  where
    showToken = show
    nextPos pos _ _ = pos

controlSequence :: TeXParser Token
controlSequence =
  tokenWithFunc testControlSequence <?> "control sequence"
  where
    testControlSequence esc@(ControlSequence _) = Just esc
    testControlSequence _ = Nothing

exactToken :: Token -> TeXParser Token
exactToken tok =
  tokenWithFunc testToken <?> ("token " ++ show tok)
  where
    testToken tok'
      | tok == tok' = Just tok
      | otherwise = Nothing

categoryToken :: Category -> TeXParser Token
categoryToken cat =
  tokenWithFunc testToken <?> ("token of category " ++ show cat)
  where
    testToken tok@(CharToken _ cat')
      | cat == cat' = Just tok
      | otherwise = Nothing
    testToken _ = Nothing
