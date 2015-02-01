{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, TemplateHaskell, NamedFieldPuns, RecordWildCards #-}
module MoeDict where
import Data.String
import Data.Text
import Data.Text.Encoding as E
import Data.Aeson
import Data.Aeson.TH
import Control.Applicative
import Data.HashMap.Strict ((!))
import qualified Data.ByteString.Lazy as B

type Str = Text
data Pronounciation = Pronounciation { bopomofo :: Str, bopomofo2 :: Str, pinyin :: Str } deriving (Show)
$(deriveJSON defaultOptions ''Pronounciation)
newtype Quote = Quote Str deriving (Show, IsString, FromJSON, ToJSON)
data Radical = Radical
    { letter :: Char
    , strokeCount :: Count
    , nonRadicalStrokeCount :: Count
    } deriving (Show, Ord, Eq)
newtype Example = Example Str deriving (Show, IsString, FromJSON, ToJSON)
newtype Title = Title Str deriving (Show, IsString, FromJSON, ToJSON)
newtype Link = Link Str deriving (Show, IsString, FromJSON, ToJSON)
newtype Count = Count Int deriving (Show, Ord, Eq, FromJSON, ToJSON)
data PartOfSpeech = Preposition | Pronoun | Adverb | Particle | Verb | Noun | Adjective | Exclamation | Onomatopoeia | Affix | Conjunction | Note deriving (Show)

instance FromJSON PartOfSpeech where
    parseJSON (String s) = maybe (fail $ show s) pure $ lookup s
        [ ("介", Preposition), ("代", Pronoun), ("副", Adverb)
        , ("助", Particle), ("動", Verb), ("名", Noun)
        , ("形", Adjective), ("歎", Exclamation), ("狀", Onomatopoeia)
        , ("綴", Affix), ("連", Conjunction), ("辨似", Note)
        ]
    parseJSON x = fail $ show x

data Entry = Entry
    { title      :: Title
    , radical    :: Maybe Radical
    , heteronyms :: [Heteronym]
    } deriving (Show)
data Heteronym = Heteronym
    { pronounciation  :: Pronounciation
    , definitions     :: [Definition]
    } deriving (Show)
data Definition = Definition
    { def         :: Text
    , examples    :: [Example]
    , quotes      :: [Quote]
    , links       :: [Link]
    , antonyms    :: [Title]
    , synonyms    :: [Title]
    } deriving (Show)
instance FromJSON Entry where
    parseJSON (Object o) = do
        title       <- o .: "title"
        heteronyms  <- o .: "heteronyms"
        rv          <- o .:? "radical"
        radical     <- case rv of
            Nothing -> return Nothing
            Just letter -> Just <$> do
                strokeCount             <- o .: "stroke_count"
                nonRadicalStrokeCount   <- o .: "non_radical_stroke_count"
                return Radical{..}
        return Entry{..}
instance FromJSON Definition where
    parseJSON (Object o) = do
        def       <- o .: "def"
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
        maybeTitles (Just xs) = Title <$> (splitOn "," xs)
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
main = do
    decoded <- eitherDecode <$> B.readFile "sample.json"
    print (decoded :: Either String [Entry])
