module Music.Note
(
    Note,
    Accidental,
    Key
)
where

data Note = A | B | C | D | E | F | G deriving Show
data Accidental = Sharp | Flat
data Key = Key Note (Maybe Accidental)