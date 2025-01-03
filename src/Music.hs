module Music
( 
    Octave(..),
    BaseNote(..),
    Accidental(..),
    Note(..),
    toSemitones
) 
where

data Octave = O0 | O1 | O2 | O3 | O4 | O5 | O6 | O7 | O8 | O9 | O10 | O11 | O12
    deriving (Eq, Show, Enum, Bounded)

data BaseNote = A | B | C | D | E | F | G
    deriving (Eq, Show)

data Accidental = Sharp | Flat | DoubleSharp | DoubleFlat | HalfSharp | HalfFlat
    deriving (Eq, Show)

data Note = Note BaseNote Octave (Maybe Accidental)
    deriving (Eq, Show)

octaveToSemitones :: Octave -> Float
octaveToSemitones oct = 12 * fromIntegral(fromEnum oct)

baseNoteToSemitones :: BaseNote -> Float
baseNoteToSemitones note = case note of
    A -> 0
    B -> 2
    C -> -9
    D -> -7
    E -> -5
    F -> -4
    G -> -2

-- TODO! make this take an Accidental then handle maybe cases elsewhere
accidentalToSemitones :: Maybe Accidental -> Float
accidentalToSemitones Nothing = 0
accidentalToSemitones (Just acc) = case acc of
    HalfSharp -> 0.5
    HalfFlat -> -0.5
    Sharp -> 1
    Flat -> -1
    DoubleSharp -> 2
    DoubleFlat -> -2

-- float semitone definition.. cancer or cured..
-- TODO! just make this int man and define everything in quatertones 
toSemitones :: Note -> Float
toSemitones (Note base octave accidental) = baseSemitones + octSemitones + accSemitones
    where
        baseSemitones = baseNoteToSemitones base
        octSemitones = octaveToSemitones octave
        accSemitones = accidentalToSemitones accidental