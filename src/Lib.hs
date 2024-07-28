module Lib (enumerate, normalize, tokenize, normalizeTokenize) where
-- import qualified Data.Text.ICU as ICU
import qualified Data.Text as Text

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

normalize :: Text.Text -> Text.Text
normalize = Text.toLower

tokenize :: Text.Text -> [String]
tokenize = map Text.unpack . Text.words

normalizeTokenize :: Text.Text -> [(Int, String)]
normalizeTokenize = enumerate . tokenize . normalize

-- loc :: ICU.LocaleName
-- loc = ICU.Locale "en_US"

-- breaker :: ICU.Breaker ICU.Word
-- breaker = ICU.breakWord loc

-- normalizeICU :: Text.Text -> Text.Text
-- normalizeICU = ICU.nfkd

-- tokenizeICU :: Text.Text -> [String]
-- tokenizeICU s = map (Text.unpack . ICU.brkBreak) $ ICU.breaks breaker s

-- normalizeTokenizeICU :: Text.Text -> [(Int, String)]
-- normalizeTokenizeICU = enumerate . tokenizeICU . normalizeICU