--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import           Control.Exception (try, SomeException)
import           Control.Monad (liftM)
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.List (sortBy, groupBy, concatMap)
import           Data.Function (on)
import           Data.Monoid (mappend)
import           Data.Text as T (Text, pack, unpack)
import qualified Data.Map as M
import           Data.Time
import           GHC.Generics
import           Hakyll
import           Network.HTTP.Conduit
import           System.FilePath (takeBaseName)


--------------------------------------------------------------------------------
-- Meeting data structure matching the JSON API
data Meeting = Meeting
    { meetingName :: Text
    , day :: Maybe Int
    , time :: Maybe Text
    , location :: Maybe Text
    , formatted_address :: Maybe Text
    , types :: Maybe [Text]
    , notes :: Maybe Text
    } deriving (Show, Generic)

instance FromJSON Meeting where
    parseJSON = withObject "Meeting" $ \o -> Meeting
        <$> o .: "name"
        <*> o .:? "day"
        <*> o .:? "time"
        <*> o .:? "location"
        <*> o .:? "formatted_address"
        <*> o .:? "types"
        <*> o .:? "notes"

-- Day names for display
dayNames :: M.Map Int String
dayNames = M.fromList
    [ (0, "Sunday")
    , (1, "Monday") 
    , (2, "Tuesday")
    , (3, "Wednesday")
    , (4, "Thursday")
    , (5, "Friday")
    , (6, "Saturday")
    ]

-- Format time from 24hr to 12hr format
formatTime12Hour :: String -> String
formatTime12Hour time = 
    case splitAt 2 time of
        (hourStr, colonAndMin) | length hourStr == 2 && take 1 colonAndMin == ":" ->
            case (reads hourStr :: [(Int, String)], reads (drop 1 colonAndMin) :: [(Int, String)]) of
                ([(hour, "")], [(minute, "")]) -> 
                    let hour12 = if hour == 0 then 12 
                                else if hour > 12 then hour - 12 
                                else hour
                        ampm = if hour < 12 then "AM" else "PM"
                        minuteStr = if minute < 10 then "0" ++ show minute else show minute
                    in show hour12 ++ ":" ++ minuteStr ++ " " ++ ampm
                _ -> time
        _ -> time

-- Fetch Seattle meetings with timestamp
fetchSeattleMeetingsWithTime :: IO (Either String ([Meeting], String))
fetchSeattleMeetingsWithTime = do
    currentTime <- getCurrentTime
    let timeStr = formatTime defaultTimeLocale "%B %e, %Y at %l:%M %p %Z" currentTime
    result <- try $ do
        request <- parseRequest "https://cors-proxy-xi-ten.vercel.app/api/proxy?url=https://www.seattleaa.org/wp-content/tsml-cache-cbdb25180b.json"
        manager <- newManager tlsManagerSettings
        response <- httpLbs request manager
        let body = responseBody response
        case eitherDecode body of
            Left err -> return $ Left $ "JSON decode error: " ++ err
            Right meetings -> return $ Right (meetings, timeStr)
    case result of
        Left e -> return $ Left $ "Network error: " ++ show (e :: SomeException)
        Right parseResult -> return parseResult

-- Fetch New York meetings with timestamp
fetchNewYorkMeetingsWithTime :: IO (Either String ([Meeting], String))
fetchNewYorkMeetingsWithTime = do
    currentTime <- getCurrentTime
    let timeStr = formatTime defaultTimeLocale "%B %e, %Y at %l:%M %p %Z" currentTime
    result <- try $ do
        request <- parseRequest "https://cors-proxy-xi-ten.vercel.app/api/proxy?url=https://www.nyintergroup.org/wp-content/tsml-cache-7fd5dcc047.json"
        manager <- newManager tlsManagerSettings
        response <- httpLbs request manager
        let body = responseBody response
        case eitherDecode body of
            Left err -> return $ Left $ "JSON decode error: " ++ err
            Right meetings -> return $ Right (meetings, timeStr)
    case result of
        Left e -> return $ Left $ "Network error: " ++ show (e :: SomeException)
        Right parseResult -> return parseResult

-- Group meetings by day and sort by time
groupMeetingsByDay :: [Meeting] -> M.Map Int [Meeting]
groupMeetingsByDay meetings = 
    let validMeetings = filter (\m -> case day m of Just _ -> True; Nothing -> False) meetings
        getMeetingDay m = case day m of Just d -> d; Nothing -> 0
        getMeetingTime m = case time m of Just t -> t; Nothing -> ""
        -- Use foldr to build a proper Map grouping, not consecutive grouping
        groupedMap = foldr (\meeting acc -> 
                            let dayNum = getMeetingDay meeting
                            in M.insertWith (++) dayNum [meeting] acc
                           ) M.empty validMeetings
        -- Sort meetings within each day by time
        sortedGroups = M.map (sortBy (compare `on` getMeetingTime)) groupedMap
    in sortedGroups

-- Format meetings as HTML
formatMeetingsHTML :: [Meeting] -> String
formatMeetingsHTML meetings = 
    let groupedMeetings = groupMeetingsByDay meetings
        dayOrder = [0,1,2,3,4,5,6]
    in concatMap (formatDay groupedMeetings) dayOrder
  where
    formatDay :: M.Map Int [Meeting] -> Int -> String
    formatDay groupedMeetings dayNum = 
        case M.lookup dayNum groupedMeetings of
            Nothing -> ""
            Just dayMeetings -> 
                let dayName = M.findWithDefault "Unknown" dayNum dayNames
                    dayId = "day-" ++ show dayNum
                    meetingCount = length dayMeetings
                    displayName = dayName ++ " (" ++ show meetingCount ++ " meetings)"
                in "<h2>" ++ dayName ++ " (" ++ show meetingCount ++ " meetings)</h2>\n" ++ 
                   "<div class=\"day-meetings\">\n" ++ 
                   concatMap formatMeeting dayMeetings ++ "</div>\n"
    
    formatMeeting :: Meeting -> String
    formatMeeting meeting = 
        "<div class=\"meeting\">\n" ++
        "  <h3>" ++ T.unpack (meetingName meeting) ++ "</h3>\n" ++
        "  <p>Time: " ++ (case time meeting of Nothing -> ""; Just t -> formatTime12Hour (T.unpack t)) ++ "</p>\n" ++
        "  <p>Location: " ++ (case location meeting of Nothing -> ""; Just loc -> T.unpack loc) ++ "</p>\n" ++
        "  <p>Address: " ++ (case formatted_address meeting of Nothing -> ""; Just addr -> T.unpack addr) ++ "</p>\n" ++
        "  <p>Types: " ++ (case types meeting of Nothing -> ""; Just ts -> show ts) ++ "</p>\n" ++
        (case notes meeting of Nothing -> ""; Just n -> "  <p>Notes: " ++ T.unpack n ++ "</p>\n") ++
        "</div>\n\n"

-- Seattle meetings compiler
seattleMeetingsCompiler :: Compiler (Item String)
seattleMeetingsCompiler = do
    meetingsResult <- unsafeCompiler fetchSeattleMeetingsWithTime
    case meetingsResult of
        Left err -> makeItem $ "<p>Error loading Seattle meetings: " ++ err ++ "</p>"
        Right (meetings, _) -> makeItem $ formatMeetingsHTML meetings

-- Seattle timestamp compiler
seattleTimestampCompiler :: Compiler (Item String)
seattleTimestampCompiler = do
    meetingsResult <- unsafeCompiler fetchSeattleMeetingsWithTime
    case meetingsResult of
        Left _ -> makeItem "Unknown"
        Right (_, timeStr) -> makeItem timeStr

-- New York meetings compiler
newYorkMeetingsCompiler :: Compiler (Item String)
newYorkMeetingsCompiler = do
    meetingsResult <- unsafeCompiler fetchNewYorkMeetingsWithTime
    case meetingsResult of
        Left err -> makeItem $ "<p>Error loading New York meetings: " ++ err ++ "</p>"
        Right (meetings, _) -> makeItem $ formatMeetingsHTML meetings

-- New York timestamp compiler
newYorkTimestampCompiler :: Compiler (Item String)
newYorkTimestampCompiler = do
    meetingsResult <- unsafeCompiler fetchNewYorkMeetingsWithTime
    case meetingsResult of
        Left _ -> makeItem "Unknown"
        Right (_, timeStr) -> makeItem timeStr

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown", "about.md", "contact.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- Create Seattle meetings data
    create ["seattle-meetings.html"] $ do
        route idRoute
        compile seattleMeetingsCompiler

    -- Create Seattle timestamp data
    create ["seattle-timestamp.html"] $ do
        route idRoute
        compile seattleTimestampCompiler

    -- Create New York meetings data
    create ["newyork-meetings.html"] $ do
        route idRoute
        compile newYorkMeetingsCompiler

    -- Create New York timestamp data
    create ["newyork-timestamp.html"] $ do
        route idRoute
        compile newYorkTimestampCompiler

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
            -- Load the appropriate meetings and timestamp data based on the post
            postPath <- getUnderlying
            let postName = takeBaseName $ toFilePath postPath
            
            (meetingsItem, timestampItem) <- case postName of
                "seattle-meetings" -> do
                    m <- load "seattle-meetings.html"
                    t <- load "seattle-timestamp.html"
                    return (m, t)
                "new-york-meetings" -> do
                    m <- load "newyork-meetings.html"
                    t <- load "newyork-timestamp.html"
                    return (m, t)
                _ -> do
                    -- Default case for other posts
                    m <- makeItem ""
                    t <- makeItem ""
                    return (m, t)
            
            let meetingsHtml = itemBody meetingsItem
                timestampStr = itemBody timestampItem
            
            body <- itemBody <$> getResourceBody
            let processedBody = replaceAll "LAST_UPDATED_TIME" (const timestampStr) body
                finalBody = case postName of
                    "seattle-meetings" -> replaceAll "SEATTLE_MEETINGS_DATA" (const meetingsHtml) processedBody
                    "new-york-meetings" -> replaceAll "NEW_YORK_MEETINGS_DATA" (const meetingsHtml) processedBody
                    _ -> processedBody
            
            makeItem finalBody
                >>= renderPandoc
                >>= loadAndApplyTemplate "templates/post.html"    postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext