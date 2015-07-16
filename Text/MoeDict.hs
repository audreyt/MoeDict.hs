{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, TemplateHaskell, NamedFieldPuns, RecordWildCards, FlexibleInstances #-}
{-# OPTIONS_GHC -v0 #-}
module Text.MoeDict where
import Data.String
import Data.Text (Text)
import Data.Text.Encoding as E
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.Aeson
import Data.Aeson.TH
import Data.Maybe (catMaybes)
import Control.Applicative
import Data.HashMap.Strict ((!))
import qualified Data.ByteString.Lazy as B
import Data.Map.Strict (Map)
import Data.List (sortBy)
import Data.Function (on)

type Str = Text
data Pronounciation = Pronounciation { bopomofo :: Str, bopomofo2 :: Str, pinyin :: Str } deriving (Show, Eq, Ord)
$(deriveJSON defaultOptions ''Pronounciation)
newtype Quote = Quote Str deriving (Show, IsString, FromJSON, ToJSON, Eq, Ord)
data Radical = Radical
    { letter :: Char
    , strokeCount :: Count
    , nonRadicalStrokeCount :: Count
    , shapeDescription :: Maybe Text -- "解形"
    } deriving (Show, Ord, Eq)
newtype Example = Example Str deriving (Show, IsString, FromJSON, ToJSON, Eq, Ord)
newtype Title = Title { titleText :: Str } deriving (Show, IsString, FromJSON, ToJSON, Ord, Eq)
newtype Link = Link Str deriving (Show, IsString, FromJSON, ToJSON, Eq, Ord)
newtype Count = Count Int deriving (Show, Ord, Eq, FromJSON, ToJSON, Enum)
data Part = Preposition | Pronoun | Adverb | Particle | Verb | Noun | Adjective | Exclamation | Onomatopoeia | Affix | Conjunction | Note
    | Slang | Loanword | Derivative deriving (Show, Eq, Ord)
data POS = POS { label :: Text, part :: Part } deriving (Show, Eq, Ord)
data Ref = Ref { refLabel :: Text, refText :: Text, refRelationship :: Relationship } deriving (Show, Eq, Ord)
data Relationship = Composition | Synonym | Antonym deriving (Show, Eq, Ord)

instance FromJSON POS where
    parseJSON (String s) = maybe (fail $ show s) (pure . POS s) $ lookup s
        [ ("介", Preposition), ("代", Pronoun), ("副", Adverb)
        , ("助", Particle), ("動", Verb), ("名", Noun)
        , ("形", Adjective), ("歎", Exclamation), ("狀", Onomatopoeia)
        , ("綴", Affix), ("連", Conjunction), ("辨似", Note)
        , ("俚", Slang), ("外", Loanword), ("衍", Derivative)
        ]
    parseJSON x = fail $ show x

instance FromJSON Ref where
    parseJSON (String s) = maybe (fail $ show s) (pure . Ref s) $ lookup s
        [ ("孳", Composition), ("同", Synonym), ("反", Antonym)
        ]
    parseJSON x = fail $ show x


data Entry = Entry
    { title      :: Title
    , radical    :: Maybe Radical
    , heteronyms :: [Heteronym]
    , references :: [Ref]
    } deriving (Show, Eq, Ord)
data Heteronym = Heteronym
    { pronounciation  :: Pronounciation
    , definitions     :: [Definition]
    } deriving (Show, Eq, Ord)
data Definition = Definition
    { definition  :: Text
    , pos         :: Maybe POS
    , examples    :: [Example]
    , quotes      :: [Quote]
    , links       :: [Link]
    , antonyms    :: [Title]
    , synonyms    :: [Title]
    } deriving (Show, Eq, Ord)
instance FromJSON (Maybe Entry) where
    parseJSON j = Just <$> parseJSON j <|> return Nothing
instance FromJSON (Maybe Heteronym) where
    parseJSON j = Just <$> parseJSON j <|> return Nothing
instance FromJSON Entry where
    parseJSON (Object o) = do
        title       <- o .: "title"
        referencea  <- maybeList <$> o .:? "references"
        heteronyms  <- catMaybes <$> o .: "heteronyms"
        rv          <- o .:? "radical"
        radical     <- case rv of
            Nothing -> return Nothing
            Just letter -> Just <$> do
                strokeCount             <- o .: "stroke_count"
                nonRadicalStrokeCount   <- o .: "non_radical_stroke_count"
                shapeDescription        <- o .:? "shape_description"
                return Radical{..}
        return Entry{..}
instance FromJSON Definition where
    parseJSON (Object o) = do
        definition <- o .: "def"
        pos       <- o .:? "type"
        examples  <- maybeList <$> o .:? "example"
        quotes    <- maybeList <$> o .:? "quote"
        links     <- maybeList <$> o .:? "link"
        antonyms  <- maybeTitles <$> o .:? "antonyms"
        synonyms  <- maybeTitles <$> o .:? "synonyms"
        return Definition {..}
        where
        maybeList Nothing = []
        maybeList (Just xs) = xs
        maybeTitles Nothing = []
        maybeTitles (Just xs) = Title <$> (T.splitOn "," xs)
instance ToJSON Definition where
    toJSON Definition {..} = error "NYI"
    
instance FromJSON Heteronym where
    parseJSON json@(Object o) = do
        pronounciation <- parseJSON json
        definitions    <- o .: "definitions"
        return $ Heteronym { pronounciation, definitions }
instance ToJSON Heteronym where
    toJSON Heteronym {..} = object
        [ ("pronounciation" .= pronounciation)
        , ("definitions" .= definitions)
        ]

parseMoeDictFile :: FilePath -> IO [Entry]
parseMoeDictFile fn = do
    decoded <- eitherDecode <$> B.readFile fn
    either fail (return . catMaybes) decoded

---
type RadicalLetter = Char
type PinYin = T.Text
data HeadWord = HeadWord
    { headChar    :: !Char
    , headSound   :: !PinYin
    } deriving (Show, Eq, Ord)
type EntryMap = Map HeadWord [Entry]
data Cluster = Cluster
    { radicalLetter  :: RadicalLetter
    , headWord  :: !HeadWord
    , clusterEntries :: [Entry]
    } deriving (Show)
entriesToMap :: [Entry] -> Map RadicalLetter EntryMap
entriesToMap entries = Map.fromList $ ms
    where
    entriesSplitted = concatMap splitHeteronym entries
    entriesByHeteronym = [(entryHead e, [e]) | e <- entriesSplitted]
    cs :: [Cluster]
    cs = map es2cluster $ Map.toList $ Map.fromListWith (++) entriesByHeteronym
    ms = map cs2map $ Map.elems $ Map.fromListWith (++) [ (radicalLetter c, [c]) | c <- cs ]
    cs2map :: [Cluster] -> (RadicalLetter, EntryMap)
    cs2map clusters@(Cluster{radicalLetter}:_) = (radicalLetter, Map.fromList $ map (\x -> (headWord x, clusterEntries x)) clusters)
    cs2map _ = error "impossible"
    es2cluster :: (HeadWord, [Entry]) -> Cluster
    es2cluster (headWord, es) =
        case es' of
            Entry{radical=Just Radical{letter}}:_ ->
                Cluster { radicalLetter = letter, headWord, clusterEntries = es' }
            _ -> Cluster { radicalLetter = '?', headWord, clusterEntries = es' }
        where
        es' = sortBy (compare `on` title) es

splitHeteronym :: Entry -> [Entry]
splitHeteronym Entry{..} = [ Entry{title, radical, heteronyms=[h]} | h <- heteronyms ]

entryHead :: Entry -> HeadWord
entryHead Entry{..} = HeadWord {..}
    where
    headChar = T.head (titleText title)
    headSound =
        T.dropWhileEnd (== 'r') $ -- 兒化韻
        T.takeWhile (/= ' ') $
        T.dropWhile (> '\255') $
        pinyin (pronounciation (head heteronyms))
