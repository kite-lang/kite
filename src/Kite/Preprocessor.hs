module Kite.Preprocessor (preprocess, preprocessFile) where

import qualified Language.Preprocessor.Cpphs as Cpphs
import System.Directory
import System.FilePath.Posix
import System.IO
import System.IO.Temp

data Encoding = Encoding (Maybe (Handle -> IO ()))

useEncoding h (Encoding x) = maybe (return ()) ($ h) x

readFileEncoding enc file = do
    h <- if file == "-" then return stdin else openFile file ReadMode
    useEncoding h enc
    hGetContents h

options file = do
  cwd <- getCurrentDirectory
  let full = dropFileName $ cwd </> file
  case Cpphs.parseOptions [file, "-I" ++ full, "--noline"] of
    Right opts -> return opts
    Left err -> error err

include file = do
  enc <- readFileEncoding (Encoding Nothing) file
  opts <- options file
  Cpphs.runCpphs opts file enc

preprocess source =
  withSystemTempFile "kite" $ \tmpFile hFile -> do
    let temp = tmpFile ++ ".tmp" -- avoid locked file
    writeFile temp source
    res <- include temp
    removeFile temp
    return res

preprocessFile = include
