module Options where

import CodeGeneration.JavascriptCode

-- | Program options
data Options = Options {

  -- | Input HTML
  input :: FilePath,

  -- | Function that writes the output
  outputWriter :: String -> IO (),

  -- | Javascript writing logic
  javascript :: JavascriptCode

}
