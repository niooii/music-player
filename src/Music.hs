module Music
( 
    Octave(..),
    BaseNote(..),
    Accidental(..),
    Note(..),
    BPM,
    Beats,
    toQuatertones
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

type BPM = Float

-- Relative to the BPM, eg. 1 beat = whole note, 1/16 beat = sixteenth
type Beats = Float

type Quatertones = Int

octaveToQuatertones :: Octave -> Quatertones
octaveToQuatertones oct = 24 * fromEnum oct

baseNoteToQuatertones :: BaseNote -> Quatertones
baseNoteToQuatertones note = case note of
    A -> 0
    B -> 4
    C -> -18
    D -> -14
    E -> -10
    F -> -8
    G -> -4

accidentalToQuatertones :: Accidental -> Quatertones
accidentalToQuatertones acc = case acc of
    HalfSharp -> 1
    HalfFlat -> -1
    Sharp -> 2
    Flat -> -2
    DoubleSharp -> 4
    DoubleFlat -> -4

toQuatertones :: Note -> Quatertones
toQuatertones (Note base octave accidental) = baseSemitones + octSemitones + accSemitones
    where
        baseSemitones = baseNoteToQuatertones base
        octSemitones = octaveToQuatertones octave
        accSemitones = maybe 0 accidentalToQuatertones accidental