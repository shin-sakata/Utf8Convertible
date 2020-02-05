import Data.Text (Text)
import Data.Utf8Convertible
import Prelude

main :: IO ()
main = printGenericText ("こんにちわ" :: String)

printText :: Text -> IO ()
printText = putStrLn . convert

printGenericText :: (ConvertTo Text a) => a -> IO ()
printGenericText txt = printText $ convert txt