import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import System.Process
import Text.Printf
import Data.List (transpose)
import Music

type Seconds = Float
type Samples = Float
type Hz = Float

volume :: Float
volume = 0.5

outputFilePath :: FilePath
outputFilePath = "out.bin"

sampleRate :: Float
sampleRate = 48000.0

noteFreq :: Note -> Hz
noteFreq note = 55.0 * 2 ** (toSemitones note/12)

-- generate a single wave at a frequency
makeWave :: Note -> Seconds -> [Float]
makeWave note secs = map ((* (volume / 3)) . sin . (* step)) [0.0 .. sampleRate * secs]
    where
    hz = noteFreq note
    step = hz * 2 * pi / sampleRate

notes :: [Note]
notes = [Note A O3 (Just HalfSharp), Note C O4 (Just HalfSharp), Note E O4 (Just HalfSharp), Note G O4 (Just HalfSharp)]

durations :: [Seconds]
durations = [4, 3, 3, 3]

wave :: [Float]
wave = map sum $ transpose $ map (uncurry makeWave) $ zip notes durations

saveSounds :: FilePath -> IO ()
saveSounds fp = BL.writeFile fp $ BB.toLazyByteString $ foldMap BB.floatLE wave

playTest :: IO ()
playTest = do
    saveSounds outputFilePath
    _ <- runCommand $ printf "ffplay -showmode 1 -f f32le -ar %f %s -autoexit" sampleRate outputFilePath
    return ()