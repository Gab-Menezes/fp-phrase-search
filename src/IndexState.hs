{-# LANGUAGE DeriveGeneric #-}

module IndexState (
    newPostingsList, 
    newIndexState, 
    setDocIdToPath, 
    setTokenToPl,
    insertDoc,
    insertPl,
    insertPlIfAbsent,
    getPl,
    appendPos,
    IndexState,
    docIdToPath,
    tokenToPl,
    PostingsList,
    docIds,
    positions,
    numberOfTokens,
    sortedTokensFreq,
    sortedTokensFreqPerDoc
) where
import qualified Data.Binary as Binary
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Maybe
import GHC.Generics (Generic)
import Data.List (sortBy)
import Data.Ord (Down(Down))

data PostingsList = PostingsList {
    docIds :: [Int],
    positions :: [[Int]]
} deriving (Show, Generic)

instance Binary.Binary PostingsList

appendPos :: PostingsList -> Int -> Int -> PostingsList
appendPos (PostingsList [] []) docId pos = PostingsList [docId] [[pos]]
appendPos (PostingsList (lastDocId:ids) (lastPos:poss)) docId pos 
    | lastDocId == docId = PostingsList (lastDocId : ids) ((pos : lastPos): poss)
    | docId > lastDocId = PostingsList (docId : lastDocId : ids) ([pos] : lastPos: poss)
    | otherwise = error "Can't append smaller doc id"
appendPos (PostingsList (_:_) []) _ _ = error "Doc Ids length is bigger than Positions"
appendPos (PostingsList [] (_:_)) _ _ = error "Positions length is bigger than Doc Ids"

sumFreq :: PostingsList -> Int
sumFreq pl = sum $ map length $ positions pl

data IndexState = IndexState {
    docIdToPath :: IntMap.IntMap FilePath,
    tokenToPl :: Map.Map String PostingsList
} deriving (Show, Generic)

instance Binary.Binary IndexState

newPostingsList :: PostingsList
newPostingsList = PostingsList [] []

newIndexState :: IndexState
newIndexState = IndexState IntMap.empty Map.empty

setDocIdToPath :: IndexState -> IntMap.IntMap FilePath -> IndexState
setDocIdToPath state newDocIdToPath = IndexState newDocIdToPath $ tokenToPl state

setTokenToPl :: IndexState -> Map.Map String PostingsList -> IndexState
setTokenToPl state = IndexState $ docIdToPath state

insertDoc :: IndexState -> Int -> FilePath -> IndexState
insertDoc state docId path = setDocIdToPath state (IntMap.insert docId path $ docIdToPath state)

insertPl :: IndexState -> String -> PostingsList -> IndexState
insertPl state token pl = setTokenToPl state (Map.insert token pl $ tokenToPl state)

insertPlIfAbsent :: IndexState -> String -> IndexState
insertPlIfAbsent state token = setTokenToPl state (Map.alter (Just . Data.Maybe.fromMaybe newPostingsList) token $ tokenToPl state)

getPl :: IndexState -> String -> PostingsList
getPl state token = tokenToPl state Map.! token

listOfTokens :: IndexState -> [(String, PostingsList)]
listOfTokens state = Map.toList $ tokenToPl state

listOfDocs :: IndexState -> [(Int, String)]
listOfDocs state = IntMap.toList $ docIdToPath state

numberOfTokens :: IndexState -> Int
numberOfTokens state = Map.size $ tokenToPl state

sortTokensFreq :: [(String, Int)] -> [(String, Int)]
sortTokensFreq = sortBy (\(_, l0) (_, l1) -> compare (Down l0) (Down l1))

sortedTokensFreq :: IndexState -> [(String, Int)]
sortedTokensFreq state = sortTokensFreq $ map (\x -> (fst x, sumFreq $ snd x)) $ listOfTokens state

sortedTokensFreqPerDoc :: IndexState -> [(Int, [(String, Int)])]
sortedTokensFreqPerDoc state = innerSortedTokensFreqPerDoc (listOfDocs state) (listOfTokens state)

innerSortedTokensFreqPerDoc :: [(Int, String)] -> [(String, PostingsList)] -> [(Int, [(String, Int)])]
innerSortedTokensFreqPerDoc [] _ = []
innerSortedTokensFreqPerDoc ((docId, _):ls) pls = (docId, sortTokensFreq $ accumulateTokensFreqPerDoc docId pls) : innerSortedTokensFreqPerDoc ls pls

accumulateTokensFreqPerDoc :: Int -> [(String, PostingsList)] -> [(String, Int)]
accumulateTokensFreqPerDoc _ [] = []
accumulateTokensFreqPerDoc docId ((token, pl):pls) 
    | s == 0 = accumulateTokensFreqPerDoc docId pls
    | otherwise = (token, s) : accumulateTokensFreqPerDoc docId pls
    where
        ds = docIds pl
        ps = positions pl
        zipped = zip ds ps
        filtered = filter ((== docId) . fst) zipped
        s = sum $ map (length . snd) filtered