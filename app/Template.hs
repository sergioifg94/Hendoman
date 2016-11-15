module Template (withTemplate) where

import Text.Regex

-- | Read a template from a file and return the result of placing a string into
--   the template
withTemplate :: String       -- ^ Path of the template file
             -> String       -- ^ Code to be placed in the template
             -> IO String    -- ^ IO Action that reads the template and returns the code
withTemplate templatePath output = subTemplate output <$> readFile templatePath


subTemplate :: String   -- ^ String to be placed in the template
            -> String   -- ^ Template string
            -> String   -- ^ Result of placing the string in the template
subTemplate = flip $ subRegex templateRegex


templateRegex :: Regex
templateRegex = mkRegex "{{code}}"
