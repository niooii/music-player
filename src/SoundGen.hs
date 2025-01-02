import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import System.Process
import Text.Printf
import Data.List (transpose)

type Seconds = Float
type Samples = Float
type Hz = Float

volume :: Float
volume = 0.5

outputFilePath :: FilePath
outputFilePath = "out.bin"

sampleRate :: Float
sampleRate = 48000.0

-- generate a single wave at a frequency
makeWave :: Hz -> [Float]
makeWave hz = map ((* (volume / 3)) . sin . (* step)) [0.0 .. sampleRate * duration]
  where
    step = hz * 2 * pi / sampleRate
    duration = 4.0

frequencies :: [Float]
frequencies = [440, 523]

wave :: [Float]
wave = map sum $ transpose $ map makeWave frequencies

saveSounds :: FilePath -> IO ()
saveSounds fp = BL.writeFile fp $ BB.toLazyByteString $ foldMap BB.floatLE wave

playTest :: IO ()
playTest = do
    saveSounds outputFilePath
    _ <- runCommand $ printf "ffplay -showmode 1 -f f32le -ar %f %s -autoexit" sampleRate outputFilePath
    return ()