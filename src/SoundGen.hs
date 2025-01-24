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
volume = 1

tempBpm :: Float
tempBpm = 60

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
durations = [1/3, 2/3, 1, 4/3]

-- Generate a wave for a note and n beats
wave    :: Note -> Beats -> [Float]
wave n b = genWave (toFreq n) $ toSeconds tempBpm b

-- Generates a wave for multiple notes, each with their own beats
waves      :: [Note] -> [Beats] -> [Float]
waves ns bs = map sum $ transpose $ zipWith wave ns bs

-- Test stuff
notes :: [Note]
notes = [Note A O3 Nothing, Note C O4 Nothing, Note E O4 Nothing, Note G O4 Nothing]

durations :: [Beats]

test :: [Float]
test = waves notes durations

saveSounds   :: FilePath -> IO ()
saveSounds fp = BL.writeFile fp $ BB.toLazyByteString $ foldMap BB.floatLE test

playTest :: IO ()
playTest = do
    saveSounds outputFilePath
    _ <- runCommand $ printf "ffplay -showmode 1 -f f32le -ar %f %s -autoexit" sampleRate outputFilePath
    return ()