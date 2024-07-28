module Index (indexFolder) where
import qualified System.Directory
import qualified Lib
import qualified IndexState
import qualified Data.Text.IO as Text.IO
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as B
import System.Exit (exitFailure)

-- indexFolder :: FilePath -> FilePath -> IO (Maybe IndexState.IndexState)
indexFolder :: FilePath -> FilePath -> IO IndexState.IndexState
indexFolder dir indexingStateBinary = do
    exists <- System.Directory.doesDirectoryExist dir
    let state0 = IndexState.newIndexState
    if exists then do
        filesList <- System.Directory.listDirectory dir
        state1 <- indexFiles state0 $ Lib.enumerate $ map ((dir ++ "/") ++) filesList

        putStrLn "\nSerializing to file..."
        let bytes = Binary.encode state1
        putStrLn $ "Serialized at \"" ++ indexingStateBinary ++ "\" with size: " ++ show (B.length bytes) ++ " bytes\n"
        B.writeFile indexingStateBinary bytes

        return state1
        -- return $ Just state1
    else do
        putStrLn $ "Directory not found: " ++ dir
        exitFailure
        -- return state0
        -- return Nothing

indexFiles :: IndexState.IndexState -> [(Int, FilePath)] -> IO IndexState.IndexState
indexFiles state [] = return state
indexFiles state0 ((docId, f):files) = do
    contents <- Text.IO.readFile f
    let tokens = Lib.normalizeTokenize contents
    let state1 = IndexState.insertDoc state0 docId f
    let state2 = processTokens state1 docId tokens
    putStrLn $ "Doc ID: " ++ show docId ++ " (" ++ show (length tokens) ++" tokens) -> " ++ f
    indexFiles state2 files

processTokens :: IndexState.IndexState -> Int -> [(Int, String)] -> IndexState.IndexState
processTokens state _ [] = state
processTokens state0 docId ((pos, t):tokens) = processTokens state2 docId tokens
    where
        state1 = IndexState.insertPlIfAbsent state0 t
        pl0 = IndexState.getPl state1 t
        pl1 = IndexState.appendPos pl0 docId pos
        state2 = IndexState.insertPl state1 t pl1