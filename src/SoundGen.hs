import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import System.Process
import Text.Printf
import Data.List (transpose)
import Music

type Seconds = Float
type Samples = Float
type Hz = Float

-- constant variables for now
volume :: Float
volume = 0.5

outputFilePath :: FilePath
outputFilePath = "out.bin"

sampleRate :: Float
sampleRate = 48000.0

-- Get the frequency of a note (TODO! key signatures?)
toFreq     :: Note -> Hz
toFreq note = 55.0 * 2 ** (1/24 * fromIntegral (toQuatertones note))

toSeconds          :: BPM -> Beats -> Seconds
toSeconds bpm beats = beatDuration * beats
    where
        beatDuration = 60.0 / bpm

-- Generate a single wave at a given frequency (TODO! attack/release)
genWave        :: Hz -> Seconds -> [Float]
genWave hz secs = map ((* (volume / 3)) . sin . (* step)) [0.0 .. sampleRate * secs]
    where
        step = hz * 2 * pi / sampleRate

-- Test stuff
notes :: [Note]
notes = [Note A O3 Nothing, Note C O4 Nothing, Note E O4 Nothing, Note G O4 Nothing]

durations :: [Beats]
durations = [1/3, 2/3, 1, 4/3]

wave :: [Float]
wave = map sum $ transpose $ zipWith genWave (map toFreq notes) (map (toSeconds 120) durations)

saveSounds   :: FilePath -> IO ()
saveSounds fp = BL.writeFile fp $ BB.toLazyByteString $ foldMap BB.floatLE wave

playTest :: IO ()
playTest = do
    saveSounds outputFilePath
    _ <- runCommand $ printf "ffplay -showmode 1 -f f32le -ar %f %s -autoexit" sampleRate outputFilePath
    return ()