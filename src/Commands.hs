{-# LANGUAGE PatternSynonyms #-}

module Commands
    (pattern Plus
    , pattern Minus
    , pattern Mult
    , pattern Div
    , pattern Mod
    , pattern Not
    , pattern GreaterThan
    , pattern MoveRight
    , pattern MoveLeft
    , pattern MoveUp
    , pattern MoveDown
    , pattern MoveRandom
    , pattern HorizontalIf
    , pattern VerticalIf
    , pattern StringMode
    , pattern Duplicate
    , pattern Swap
    , pattern PopDiscard
    , pattern PopPrintInteger
    , pattern PopPrintChar
    , pattern Bridge
    , pattern Put
    , pattern Get
    , pattern PromptInteger
    , pattern PromptChar
    , pattern Halt
    , pattern Noop
    ) where

pattern Plus :: Char
pattern Plus = '+'

pattern Minus :: Char
pattern Minus = '-'

pattern Mult :: Char
pattern Mult = '*'

pattern Div :: Char
pattern Div = '/'

pattern Mod :: Char
pattern Mod = '%'

pattern Not :: Char
pattern Not = '!'

pattern GreaterThan :: Char
pattern GreaterThan = '`'

pattern MoveRight :: Char
pattern MoveRight = '>'

pattern MoveLeft :: Char
pattern MoveLeft = '<'

pattern MoveUp :: Char
pattern MoveUp = '^'

pattern MoveDown :: Char
pattern MoveDown = 'v'

pattern MoveRandom :: Char
pattern MoveRandom = '?'

pattern HorizontalIf :: Char
pattern HorizontalIf = '_'

pattern VerticalIf :: Char
pattern VerticalIf = '|'

pattern StringMode :: Char
pattern StringMode = '"'

pattern Duplicate :: Char
pattern Duplicate = ':'

pattern Swap :: Char
pattern Swap = '\\'

pattern PopDiscard :: Char
pattern PopDiscard = '$'

pattern PopPrintInteger :: Char
pattern PopPrintInteger = '.'

pattern PopPrintChar :: Char
pattern PopPrintChar = ','

pattern Bridge :: Char
pattern Bridge = '#'

pattern Put :: Char
pattern Put = 'p'

pattern Get :: Char
pattern Get = 'g'

pattern PromptInteger :: Char
pattern PromptInteger = '&'

pattern PromptChar :: Char
pattern PromptChar = '~'

pattern Halt :: Char
pattern Halt = '@'

pattern Noop :: Char
pattern Noop = ' '
