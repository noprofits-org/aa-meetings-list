--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import           Control.Exception (try, SomeException)
import           Control.Monad (liftM)
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char (toLower)
import           Data.List (sortBy, groupBy, concatMap, nub)
import           Data.Function (on)
import           Data.Maybe (catMaybes, isJust, fromMaybe)
import           Data.Monoid (mappend)
import           Data.Text as T (Text, pack, unpack)
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Time
import           Data.Time.Calendar.WeekDate (toWeekDate)
import           GHC.Generics
import           Hakyll
import           Network.HTTP.Conduit
import           System.FilePath (takeBaseName)


--------------------------------------------------------------------------------
-- Meeting data structure matching the JSON API
-- Enhanced to capture all available fields from TSML API
data Meeting = Meeting
    { -- Core fields (keeping original names for backward compatibility)
      meetingName :: Text
    , day :: Maybe Int
    , time :: Maybe Text
    , location :: Maybe Text
    , formatted_address :: Maybe Text
    , types :: Maybe [Text]
    , notes :: Maybe Text
    
    -- Additional core meeting information
    , meetingId :: Maybe Int
    , slug :: Maybe Text
    , endTime :: Maybe Text
    , timeFormatted :: Maybe Text
    , updated :: Maybe Text
    
    -- Enhanced location data
    , locationId :: Maybe Int
    , locationUrl :: Maybe Text
    , locationNotes :: Maybe Text
    , latitude :: Maybe Double
    , longitude :: Maybe Double
    , approximate :: Maybe Text
    
    -- Geographic organization
    , region :: Maybe Text
    , regions :: Maybe [Text]
    , subRegion :: Maybe Text
    , regionId :: Maybe Int
    , district :: Maybe Text
    , districtId :: Maybe Int
    
    -- Meeting characteristics
    , attendanceOption :: Maybe Text
    , conferenceUrl :: Maybe Text
    , url :: Maybe Text
    , editUrl :: Maybe Text
    
    -- Group and administrative
    , group :: Maybe Text
    , groupId :: Maybe Int
    , author :: Maybe Text
    , entity :: Maybe Text
    , entityEmail :: Maybe Text
    , entityUrl :: Maybe Text
    , entityPhone :: Maybe Text
    , entityLocation :: Maybe Text
    
    -- Source tracking
    , feedbackEmails :: Maybe Value  -- Can be Object or Array
    , dataSourceName :: Maybe Text
    , sourceFormattedAddress :: Maybe Text
    , sourceRegion :: Maybe Text
    , sourceSubRegion :: Maybe Text
    , sourceSlug :: Maybe Text
    } deriving (Show, Generic)

instance FromJSON Meeting where
    parseJSON = withObject "Meeting" $ \o -> Meeting
        -- Core fields (original)
        <$> o .: "name"
        <*> o .:? "day"
        <*> o .:? "time"
        <*> o .:? "location"
        <*> o .:? "formatted_address"
        <*> o .:? "types"
        <*> o .:? "notes"
        
        -- Additional core meeting information
        <*> o .:? "id"
        <*> o .:? "slug"
        <*> o .:? "end_time"
        <*> o .:? "time_formatted"
        <*> o .:? "updated"
        
        -- Enhanced location data
        <*> o .:? "location_id"
        <*> o .:? "location_url"
        <*> o .:? "location_notes"
        <*> o .:? "latitude"
        <*> o .:? "longitude"
        <*> o .:? "approximate"
        
        -- Geographic organization
        <*> o .:? "region"
        <*> o .:? "regions"
        <*> o .:? "sub_region"
        <*> o .:? "region_id"
        <*> o .:? "district"
        <*> o .:? "district_id"
        
        -- Meeting characteristics
        <*> o .:? "attendance_option"
        <*> o .:? "conference_url"
        <*> o .:? "url"
        <*> o .:? "edit_url"
        
        -- Group and administrative
        <*> o .:? "group"
        <*> o .:? "group_id"
        <*> o .:? "author"
        <*> o .:? "entity"
        <*> o .:? "entity_email"
        <*> o .:? "entity_url"
        <*> o .:? "entity_phone"
        <*> o .:? "entity_location"
        
        -- Source tracking
        <*> o .:? "feedback_emails"
        <*> o .:? "data_source_name"
        <*> o .:? "source_formatted_address"
        <*> o .:? "source_region"
        <*> o .:? "source_sub_region"
        <*> o .:? "source_slug"

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
-- Helper functions for future chunking phases

-- Extract unique sub-regions from meetings
getUniqueSubRegions :: [Meeting] -> [Text]
getUniqueSubRegions meetings = 
    let subRegions = catMaybes $ map subRegion meetings
    in nub subRegions

-- Extract unique districts from meetings
getUniqueDistricts :: [Meeting] -> [Text]
getUniqueDistricts meetings = 
    let districts = catMaybes $ map district meetings
    in nub districts

-- Extract all unique meeting types
getMeetingTypes :: [Meeting] -> [Text]
getMeetingTypes meetings = 
    let allTypes = concat $ catMaybes $ map types meetings
    in nub allTypes

-- Group meetings by sub-region
groupBySubRegion :: [Meeting] -> M.Map Text [Meeting]
groupBySubRegion meetings = 
    let meetingsWithSubRegion = filter (isJust . subRegion) meetings
    in foldr (\meeting acc -> 
                case subRegion meeting of
                    Just sr -> M.insertWith (++) sr [meeting] acc
                    Nothing -> acc
             ) M.empty meetingsWithSubRegion

-- Group meetings by attendance option (online, in_person, inactive)
groupByAttendanceOption :: [Meeting] -> M.Map Text [Meeting]
groupByAttendanceOption meetings = 
    let meetingsWithOption = filter (isJust . attendanceOption) meetings
    in foldr (\meeting acc -> 
                case attendanceOption meeting of
                    Just ao -> M.insertWith (++) ao [meeting] acc
                    Nothing -> acc
             ) M.empty meetingsWithOption

-- Filter meetings that are wheelchair accessible
filterAccessibleMeetings :: [Meeting] -> [Meeting]
filterAccessibleMeetings meetings = 
    filter (\m -> case types m of
                    Just ts -> "X" `elem` ts  -- X = Wheelchair Accessible
                    Nothing -> False
          ) meetings

-- Filter online meetings
filterOnlineMeetings :: [Meeting] -> [Meeting]
filterOnlineMeetings meetings = 
    filter (\m -> case types m of
                    Just ts -> "ONL" `elem` ts  -- ONL = Online
                    Nothing -> False
          ) meetings

-- Get meetings with missing critical data
getMeetingsWithMissingData :: [Meeting] -> [(Meeting, [String])]
getMeetingsWithMissingData meetings = 
    let checkMeeting m = 
            let missing = catMaybes 
                    [ if isJust (day m) then Nothing else Just "day"
                    , if isJust (time m) then Nothing else Just "time"
                    , if isJust (location m) || isJust (conferenceUrl m) then Nothing else Just "location/conference_url"
                    , if isJust (formatted_address m) || isJust (conferenceUrl m) then Nothing else Just "address/conference_url"
                    ]
            in if null missing then Nothing else Just (m, missing)
    in catMaybes $ map checkMeeting meetings

-- Count meetings by various criteria for reporting
generateMeetingStats :: [Meeting] -> M.Map String Int
generateMeetingStats meetings = M.fromList
    [ ("total", length meetings)
    , ("with_sub_region", length $ filter (isJust . subRegion) meetings)
    , ("with_district", length $ filter (isJust . district) meetings)
    , ("online", length $ filterOnlineMeetings meetings)
    , ("wheelchair_accessible", length $ filterAccessibleMeetings meetings)
    , ("with_conference_url", length $ filter (isJust . conferenceUrl) meetings)
    , ("with_coordinates", length $ filter (\m -> isJust (latitude m) && isJust (longitude m)) meetings)
    , ("missing_critical_data", length $ getMeetingsWithMissingData meetings)
    ]

-- Debug function to show sample of enhanced data
debugShowEnhancedFields :: Meeting -> String
debugShowEnhancedFields m = unlines
    [ "Meeting: " ++ T.unpack (meetingName m)
    , "  ID: " ++ show (meetingId m)
    , "  Sub-region: " ++ maybe "N/A" T.unpack (subRegion m)
    , "  District: " ++ maybe "N/A" T.unpack (district m)
    , "  Group: " ++ maybe "N/A" T.unpack (group m)
    , "  Attendance: " ++ maybe "N/A" T.unpack (attendanceOption m)
    , "  Conference URL: " ++ maybe "N/A" T.unpack (conferenceUrl m)
    , "  Coordinates: " ++ case (latitude m, longitude m) of
        (Just lat, Just lng) -> show lat ++ ", " ++ show lng
        _ -> "N/A"
    ]

-- Additional helper functions for Phase 2 chunking

-- Normalize text for URLs (lowercase, replace spaces with hyphens)
normalizeForUrl :: Text -> String
normalizeForUrl t = map (\c -> if c == ' ' then '-' else c) $ map toLower $ T.unpack t

-- Group meetings by sub-region then by day
groupBySubRegionAndDay :: [Meeting] -> M.Map (Text, Int) [Meeting]
groupBySubRegionAndDay meetings =
    let validMeetings = filter (\m -> isJust (subRegion m) && isJust (day m)) meetings
    in foldr (\meeting acc ->
                case (subRegion meeting, day meeting) of
                    (Just sr, Just d) -> M.insertWith (++) (sr, d) [meeting] acc
                    _ -> acc
             ) M.empty validMeetings

-- Get meeting type descriptions
getMeetingTypeDescription :: Text -> Text
getMeetingTypeDescription code = case code of
    "O" -> "Open"
    "C" -> "Closed"
    "ONL" -> "Online"
    "X" -> "Wheelchair Accessible"
    "W" -> "Women"
    "M" -> "Men"
    "YP" -> "Young People"
    "BA" -> "Babysitting Available"
    "TC" -> "Temporarily Closed"
    "B" -> "Big Book"
    "12x12" -> "12 Steps & 12 Traditions"
    "D" -> "Discussion"
    "SP" -> "Speaker"
    "LIT" -> "Literature"
    "MED" -> "Meditation"
    _ -> code

-- Format enhanced meeting as HTML with all available data
formatEnhancedMeetingHTML :: Meeting -> String
formatEnhancedMeetingHTML meeting = 
    "<div class=\"meeting enhanced\">\n" ++
    "  <h3>" ++ T.unpack (meetingName meeting) ++ "</h3>\n" ++
    "  <div class=\"meeting-details\">\n" ++
    formatTime meeting ++
    formatLocation meeting ++
    formatOnlineInfo meeting ++
    formatTypes meeting ++
    formatGroupInfo meeting ++
    formatNotes meeting ++
    "  </div>\n" ++
    "</div>\n"
  where
    formatTime m = case (time m, timeFormatted m, endTime m) of
        (Just t, Just tf, Just et) -> "    <p class=\"meeting-time\"><strong>Time:</strong> " ++ 
                                      T.unpack tf ++ " - " ++ formatTime12Hour (T.unpack et) ++ "</p>\n"
        (Just t, Just tf, Nothing) -> "    <p class=\"meeting-time\"><strong>Time:</strong> " ++ T.unpack tf ++ "</p>\n"
        (Just t, Nothing, _) -> "    <p class=\"meeting-time\"><strong>Time:</strong> " ++ formatTime12Hour (T.unpack t) ++ "</p>\n"
        _ -> ""
    
    formatLocation m = case (location m, formatted_address m, locationNotes m) of
        (Just loc, Just addr, Just notes) -> 
            "    <p class=\"meeting-location\"><strong>Location:</strong> " ++ T.unpack loc ++ "</p>\n" ++
            "    <p class=\"meeting-address\"><strong>Address:</strong> " ++ T.unpack addr ++ "</p>\n" ++
            "    <p class=\"location-notes\"><strong>Location Notes:</strong> " ++ T.unpack notes ++ "</p>\n"
        (Just loc, Just addr, Nothing) -> 
            "    <p class=\"meeting-location\"><strong>Location:</strong> " ++ T.unpack loc ++ "</p>\n" ++
            "    <p class=\"meeting-address\"><strong>Address:</strong> " ++ T.unpack addr ++ "</p>\n"
        _ -> ""
    
    formatOnlineInfo m = case (attendanceOption m, conferenceUrl m) of
        (Just "online", Just url) -> 
            "    <p class=\"online-info\"><strong>Online Meeting:</strong> <a href=\"" ++ 
            T.unpack url ++ "\" target=\"_blank\">Join Online</a></p>\n"
        _ -> ""
    
    formatTypes m = case types m of
        Just ts -> "    <p class=\"meeting-types\"><strong>Types:</strong> " ++ 
                  (unwords $ map (\t -> T.unpack (getMeetingTypeDescription t)) ts) ++ "</p>\n"
        Nothing -> ""
    
    formatGroupInfo m = case (group m, district m) of
        (Just g, Just d) -> "    <p class=\"group-info\"><strong>Group:</strong> " ++ T.unpack g ++ 
                           " | <strong>District:</strong> " ++ T.unpack d ++ "</p>\n"
        (Just g, Nothing) -> "    <p class=\"group-info\"><strong>Group:</strong> " ++ T.unpack g ++ "</p>\n"
        (Nothing, Just d) -> "    <p class=\"group-info\"><strong>District:</strong> " ++ T.unpack d ++ "</p>\n"
        _ -> ""
    
    formatNotes m = case notes m of
        Just n -> "    <p class=\"meeting-notes\"><strong>Notes:</strong> " ++ T.unpack n ++ "</p>\n"
        Nothing -> ""

-- Count meetings for a sub-region grouped by day
countMeetingsBySubRegionDay :: Text -> [Meeting] -> M.Map Int Int
countMeetingsBySubRegionDay subRegionName meetings =
    let subRegionMeetings = filter (\m -> subRegion m == Just subRegionName) meetings
        grouped = groupMeetingsByDay subRegionMeetings
    in M.map length grouped

-- Get today's day of week (0 = Sunday, 6 = Saturday)
getTodayDayOfWeek :: IO Int
getTodayDayOfWeek = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let localTime = utcToLocalTime timezone now
        todayDate = localDay localTime
        (_, _, weekDay) = toWeekDate todayDate
    -- Convert from ISO weekday (1=Monday) to our format (0=Sunday)
    return $ if weekDay == 7 then 0 else weekDay

-- Filter meetings for today
getTodaysMeetings :: [Meeting] -> IO [Meeting]
getTodaysMeetings meetings = do
    todayNum <- getTodayDayOfWeek
    return $ filter (\m -> day m == Just todayNum) meetings

-- Generate JSON-LD structured data for a meeting
generateMeetingJsonLD :: Meeting -> String -> String
generateMeetingJsonLD meeting baseUrl = 
    let meetingUrl = baseUrl ++ "/meetings/" -- Add specific meeting URL if available
        startTime = case timeFormatted meeting of
                      Just tf -> T.unpack tf
                      Nothing -> case time meeting of
                                   Just t -> formatTime12Hour (T.unpack t)
                                   Nothing -> ""
        eventName = T.unpack (meetingName meeting)
        locationName = case location meeting of
                         Just loc -> T.unpack loc
                         Nothing -> ""
        streetAddress = case formatted_address meeting of
                          Just addr -> T.unpack addr
                          Nothing -> ""
        isOnline = case attendanceOption meeting of
                     Just "online" -> True
                     _ -> False
        onlineUrl = case conferenceUrl meeting of
                      Just url -> T.unpack url
                      Nothing -> ""
    in "{\n" ++
       "  \"@context\": \"https://schema.org\",\n" ++
       "  \"@type\": \"Event\",\n" ++
       "  \"name\": \"" ++ eventName ++ "\",\n" ++
       "  \"startTime\": \"" ++ startTime ++ "\",\n" ++
       "  \"eventAttendanceMode\": \"" ++ (if isOnline then "OnlineEventAttendanceMode" else "OfflineEventAttendanceMode") ++ "\",\n" ++
       (if isOnline 
        then "  \"location\": {\n" ++
             "    \"@type\": \"VirtualLocation\",\n" ++
             "    \"url\": \"" ++ onlineUrl ++ "\"\n" ++
             "  },\n"
        else "  \"location\": {\n" ++
             "    \"@type\": \"Place\",\n" ++
             "    \"name\": \"" ++ locationName ++ "\",\n" ++
             "    \"address\": \"" ++ streetAddress ++ "\"\n" ++
             "  },\n") ++
       "  \"organizer\": {\n" ++
       "    \"@type\": \"Organization\",\n" ++
       "    \"name\": \"Alcoholics Anonymous\"\n" ++
       "  }\n" ++
       "}"

-- Generate sitemap entries for all meeting pages
generateSitemapEntries :: [String] -> String
generateSitemapEntries urls = 
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++
    "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">\n" ++
    concatMap formatSitemapEntry urls ++
    "</urlset>\n"
  where
    formatSitemapEntry url = 
        "  <url>\n" ++
        "    <loc>" ++ url ++ "</loc>\n" ++
        "    <changefreq>weekly</changefreq>\n" ++
        "    <priority>0.8</priority>\n" ++
        "  </url>\n"

-- Generate all sub-region pages for a city
generateSubRegionPages :: String -> String -> [Meeting] -> String -> Rules ()
generateSubRegionPages cityName cityPath meetings timestamp = do
    let subRegions = getUniqueSubRegions meetings
    mapM_ (generateSubRegionPage cityName cityPath meetings timestamp) subRegions

-- Generate a single sub-region page
generateSubRegionPage :: String -> String -> [Meeting] -> String -> Text -> Rules ()
generateSubRegionPage cityName cityPath allMeetings timestamp subRegionName = do
    let urlPath = normalizeForUrl subRegionName
        pagePath = "meetings/" ++ cityPath ++ "/" ++ urlPath ++ "/index.html"
    
    create [fromFilePath pagePath] $ do
        route idRoute
        compile $ do
            let subRegionMeetings = filter (\m -> subRegion m == Just subRegionName) allMeetings
                dayCounts = countMeetingsBySubRegionDay subRegionName allMeetings
                totalCount = sum $ M.elems dayCounts
                onlineCount = length $ filter (\m -> case types m of
                                                        Just ts -> "ONL" `elem` ts
                                                        Nothing -> False) subRegionMeetings
                
                content = unlines
                    [ "<div class=\"subregion-overview\">"
                    , "  <p class=\"last-updated\">Last updated: " ++ timestamp ++ "</p>"
                    , "  <p class=\"meeting-count\">Total meetings in " ++ T.unpack subRegionName ++ ": " ++ show totalCount ++ "</p>"
                    , "  <p>Online meetings: " ++ show onlineCount ++ "</p>"
                    , "  <h2>Meetings by Day</h2>"
                    , "  <div class=\"day-list\">"
                    , unlines $ map formatDayLink [0..6]
                    , "  </div>"
                    , "</div>"
                    ]
                
                formatDayLink dayNum = 
                    let dayName = M.findWithDefault "Unknown" dayNum dayNames
                        count = M.findWithDefault 0 dayNum dayCounts
                        dayPath = map toLower dayName
                    in if count > 0
                        then "    <div class=\"day-item\"><a href=\"/meetings/" ++ cityPath ++ "/" ++ urlPath ++ "/" ++ 
                             dayPath ++ "/\">" ++ dayName ++ " (" ++ show count ++ " meetings)</a></div>"
                        else "    <div class=\"day-item disabled\">" ++ dayName ++ " (no meetings)</div>"
                
                meetingsCtx = 
                    constField "title" (T.unpack subRegionName ++ " AA Meetings") `mappend`
                    constField "description" ("Find AA meetings in " ++ T.unpack subRegionName ++ ", " ++ cityName) `mappend`
                    constField "breadcrumbs" ("<a href=\"/meetings/\">All Meetings</a> > <a href=\"/meetings/" ++ 
                                             cityPath ++ "/\">" ++ cityName ++ "</a> > " ++ T.unpack subRegionName) `mappend`
                    defaultContext
            
            makeItem content
                >>= loadAndApplyTemplate "templates/meetings-base.html" meetingsCtx
                >>= relativizeUrls
    
    -- Generate day pages for this sub-region
    generateDayPages cityName cityPath allMeetings timestamp subRegionName

-- Generate day pages for a sub-region
generateDayPages :: String -> String -> [Meeting] -> String -> Text -> Rules ()
generateDayPages cityName cityPath allMeetings timestamp subRegionName = do
    let subRegionMeetings = filter (\m -> subRegion m == Just subRegionName) allMeetings
        dayGroups = groupMeetingsByDay subRegionMeetings
        urlPath = normalizeForUrl subRegionName
    
    mapM_ (generateDayPage cityName cityPath timestamp subRegionName urlPath) $ M.toList dayGroups

-- Generate a single day page for a sub-region
generateDayPage :: String -> String -> String -> Text -> String -> (Int, [Meeting]) -> Rules ()
generateDayPage cityName cityPath timestamp subRegionName urlPath (dayNum, dayMeetings) = do
    let dayName = M.findWithDefault "Unknown" dayNum dayNames
        dayPath = map toLower dayName
        pagePath = "meetings/" ++ cityPath ++ "/" ++ urlPath ++ "/" ++ dayPath ++ "/index.html"
    
    create [fromFilePath pagePath] $ do
        route idRoute
        compile $ do
            let sortedMeetings = sortBy (compare `on` time) dayMeetings
                content = unlines
                    [ "<div class=\"meetings-list\">"
                    , "  <p class=\"last-updated\">Last updated: " ++ timestamp ++ "</p>"
                    , "  <p class=\"meeting-count\">" ++ show (length dayMeetings) ++ " meetings on " ++ dayName ++ "</p>"
                    , "  <div class=\"meetings\">"
                    , unlines $ map formatEnhancedMeetingHTML sortedMeetings
                    , "  </div>"
                    , "</div>"
                    ]
                
                meetingsCtx = 
                    constField "title" (dayName ++ " Meetings in " ++ T.unpack subRegionName) `mappend`
                    constField "description" ("AA meetings on " ++ dayName ++ " in " ++ T.unpack subRegionName ++ ", " ++ cityName) `mappend`
                    constField "breadcrumbs" ("<a href=\"/meetings/\">All Meetings</a> > <a href=\"/meetings/" ++ 
                                             cityPath ++ "/\">" ++ cityName ++ "</a> > <a href=\"/meetings/" ++
                                             cityPath ++ "/" ++ urlPath ++ "/\">" ++ T.unpack subRegionName ++ 
                                             "</a> > " ++ dayName) `mappend`
                    defaultContext
            
            makeItem content
                >>= loadAndApplyTemplate "templates/meetings-base.html" meetingsCtx
                >>= relativizeUrls

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
    
    -- Create data quality report (for debugging Phase 1)
    create ["data-quality-report.txt"] $ do
        route idRoute
        compile $ do
            seattleResult <- unsafeCompiler fetchSeattleMeetingsWithTime
            nyResult <- unsafeCompiler fetchNewYorkMeetingsWithTime
            
            let reportContent = case (seattleResult, nyResult) of
                    (Right (seattleMeetings, _), Right (nyMeetings, _)) ->
                        let seattleStats = generateMeetingStats seattleMeetings
                            nyStats = generateMeetingStats nyMeetings
                            seattleSubRegions = getUniqueSubRegions seattleMeetings
                            nySubRegions = getUniqueSubRegions nyMeetings
                            seattleDistricts = getUniqueDistricts seattleMeetings
                            nyDistricts = getUniqueDistricts nyMeetings
                            seattleTypes = getMeetingTypes seattleMeetings
                            nyTypes = getMeetingTypes nyMeetings
                            sampleMeeting = head seattleMeetings
                        in unlines
                            [ "=== AA MEETINGS DATA QUALITY REPORT ==="
                            , ""
                            , "SEATTLE STATISTICS:"
                            , show seattleStats
                            , ""
                            , "NYC STATISTICS:"
                            , show nyStats
                            , ""
                            , "SEATTLE SUB-REGIONS (" ++ show (length seattleSubRegions) ++ " total):"
                            , unlines $ map T.unpack $ take 10 seattleSubRegions
                            , ""
                            , "NYC SUB-REGIONS (" ++ show (length nySubRegions) ++ " total):"
                            , unlines $ map T.unpack $ take 10 nySubRegions
                            , ""
                            , "SEATTLE DISTRICTS (" ++ show (length seattleDistricts) ++ " total):"
                            , unlines $ map T.unpack $ take 10 seattleDistricts
                            , ""
                            , "NYC DISTRICTS (" ++ show (length nyDistricts) ++ " total):"
                            , unlines $ map T.unpack $ take 10 nyDistricts
                            , ""
                            , "MEETING TYPES (Combined):"
                            , show $ nub $ seattleTypes ++ nyTypes
                            , ""
                            , "SAMPLE ENHANCED MEETING DATA:"
                            , debugShowEnhancedFields sampleMeeting
                            , ""
                            , "FIRST 3 ONLINE MEETINGS:"
                            , unlines $ map debugShowEnhancedFields $ take 3 $ filterOnlineMeetings seattleMeetings
                            ]
                    _ -> "Error loading meeting data"
            
            makeItem reportContent

    --------------------------------------------------------------------------------
    -- Phase 2: Chunked page structure
    
    -- Create main meetings landing page
    create ["meetings/index.html"] $ do
        route idRoute
        compile $ do
            seattleResult <- unsafeCompiler fetchSeattleMeetingsWithTime
            nyResult <- unsafeCompiler fetchNewYorkMeetingsWithTime
            
            let content = case (seattleResult, nyResult) of
                    (Right (seattleMeetings, _), Right (nyMeetings, _)) ->
                        let seattleStats = generateMeetingStats seattleMeetings
                            nyStats = generateMeetingStats nyMeetings
                            totalMeetings = length seattleMeetings + length nyMeetings
                            totalOnline = (fromMaybe 0 $ M.lookup "online" seattleStats) + 
                                        (fromMaybe 0 $ M.lookup "online" nyStats)
                            totalAccessible = (fromMaybe 0 $ M.lookup "wheelchair_accessible" seattleStats) + 
                                            (fromMaybe 0 $ M.lookup "wheelchair_accessible" nyStats)
                        in unlines
                            [ "<div class=\"regions-list\">"
                            , "  <p class=\"meeting-count\">Total meetings: " ++ show totalMeetings ++ "</p>"
                            , "  <p>Online meetings: " ++ show totalOnline ++ " | Wheelchair accessible: " ++ show totalAccessible ++ "</p>"
                            , "  <h2>Select a Region</h2>"
                            , "  <div class=\"region-cards\">"
                            , "    <a href=\"/meetings/seattle/\" class=\"region-card\">"
                            , "      <h3>Seattle Area</h3>"
                            , "      <p>" ++ show (length seattleMeetings) ++ " meetings across " ++ 
                              show (length $ getUniqueSubRegions seattleMeetings) ++ " neighborhoods</p>"
                            , "      <div class=\"region-highlights\">"
                            , "        <span>🌐 " ++ show (length $ filterOnlineMeetings seattleMeetings) ++ " online</span>"
                            , "        <span>♿ " ++ show (length $ filterAccessibleMeetings seattleMeetings) ++ " accessible</span>"
                            , "      </div>"
                            , "    </a>"
                            , "    <a href=\"/meetings/new-york/\" class=\"region-card\">"
                            , "      <h3>New York City</h3>"
                            , "      <p>" ++ show (length nyMeetings) ++ " meetings across " ++ 
                              show (length $ getUniqueSubRegions nyMeetings) ++ " neighborhoods</p>"
                            , "      <div class=\"region-highlights\">"
                            , "        <span>🌐 " ++ show (length $ filterOnlineMeetings nyMeetings) ++ " online</span>"
                            , "        <span>♿ " ++ show (length $ filterAccessibleMeetings nyMeetings) ++ " accessible</span>"
                            , "      </div>"
                            , "    </a>"
                            , "  </div>"
                            , "  <h2>Quick Links</h2>"
                            , "  <ul>"
                            , "    <li><a href=\"/meetings/online/\">All Online Meetings</a></li>"
                            , "    <li><a href=\"/meetings/wheelchair-accessible/\">Wheelchair Accessible Meetings</a></li>"
                            , "  </ul>"
                            , "</div>"
                            ]
                    _ -> "<p>Error loading meeting data</p>"
            
            let meetingsCtx = 
                    constField "title" "AA Meetings Directory" `mappend`
                    constField "description" "Find AA meetings by region, online meetings, and accessible venues" `mappend`
                    defaultContext
            
            makeItem content
                >>= loadAndApplyTemplate "templates/meetings-base.html" meetingsCtx
                >>= relativizeUrls
    
    -- Create Seattle region page
    create ["meetings/seattle/index.html"] $ do
        route idRoute
        compile $ do
            result <- unsafeCompiler fetchSeattleMeetingsWithTime
            case result of
                Right (meetings, timestamp) ->
                    let subRegions = getUniqueSubRegions meetings
                        subRegionCounts = M.fromList $ map (\sr -> 
                            (sr, length $ filter (\m -> subRegion m == Just sr) meetings)) subRegions
                        sortedSubRegions = sortBy (compare `on` fst) $ M.toList subRegionCounts
                        stats = generateMeetingStats meetings
                        
                        content = unlines
                            [ "<div class=\"region-overview\">"
                            , "  <p class=\"last-updated\">Last updated: " ++ timestamp ++ "</p>"
                            , "  <p class=\"meeting-count\">Total meetings: " ++ show (length meetings) ++ "</p>"
                            , "  <p>Online meetings: " ++ show (fromMaybe 0 $ M.lookup "online" stats) ++ 
                              " | Wheelchair accessible: " ++ show (fromMaybe 0 $ M.lookup "wheelchair_accessible" stats) ++ "</p>"
                            , "  <h2>Neighborhoods</h2>"
                            , "  <div class=\"subregion-list\">"
                            , unlines $ map formatSubRegion sortedSubRegions
                            , "  </div>"
                            , "  <h2>Alternative Views</h2>"
                            , "  <div class=\"quick-access\">"
                            , "    <h3>🔍 Quick Access</h3>"
                            , "    <div class=\"quick-links\">"
                            , "      <a href=\"/meetings/today/seattle/\" class=\"quick-link today\">"
                            , "        <span class=\"icon\">📅</span>"
                            , "        Today's Seattle Meetings"
                            , "      </a>"
                            , "      <a href=\"/meetings/online/seattle/\" class=\"quick-link online\">"
                            , "        <span class=\"icon\">🌐</span>"
                            , "        Seattle Online Meetings"
                            , "      </a>"
                            , "      <a href=\"/meetings/wheelchair-accessible/seattle/\" class=\"quick-link accessible\">"
                            , "        <span class=\"icon\">♿</span>"
                            , "        Accessible Seattle Venues"
                            , "      </a>"
                            , "    </div>"
                            , "  </div>"
                            , "</div>"
                            ]
                        
                        formatSubRegion (sr, count) = 
                            "    <div class=\"subregion-item\">" ++
                            "      <a href=\"/meetings/seattle/" ++ normalizeForUrl sr ++ "/\">" ++ 
                            T.unpack sr ++ " (" ++ show count ++ " meetings)</a>" ++
                            "    </div>"
                        
                        meetingsCtx = 
                            constField "title" "Seattle AA Meetings" `mappend`
                            constField "description" "Find AA meetings in Seattle neighborhoods" `mappend`
                            constField "breadcrumbs" "<a href=\"/meetings/\">All Meetings</a> > Seattle" `mappend`
                            defaultContext
                    in do
                        makeItem content
                            >>= loadAndApplyTemplate "templates/meetings-base.html" meetingsCtx
                            >>= relativizeUrls
                Left err -> makeItem $ "<p>Error loading meetings: " ++ err ++ "</p>"
    
    -- Generate all Seattle sub-region pages
    seattleData <- preprocess $ do
        result <- fetchSeattleMeetingsWithTime
        case result of
            Right (meetings, timestamp) -> return $ Just (meetings, timestamp)
            Left _ -> return Nothing
    
    case seattleData of
        Just (meetings, timestamp) -> generateSubRegionPages "Seattle" "seattle" meetings timestamp
        Nothing -> return ()

    -- Create NYC region page
    create ["meetings/new-york/index.html"] $ do
        route idRoute
        compile $ do
            result <- unsafeCompiler fetchNewYorkMeetingsWithTime
            case result of
                Right (meetings, timestamp) ->
                    let subRegions = getUniqueSubRegions meetings
                        subRegionCounts = M.fromList $ map (\sr -> 
                            (sr, length $ filter (\m -> subRegion m == Just sr) meetings)) subRegions
                        sortedSubRegions = sortBy (compare `on` fst) $ M.toList subRegionCounts
                        stats = generateMeetingStats meetings
                        
                        content = unlines
                            [ "<div class=\"region-overview\">"
                            , "  <p class=\"last-updated\">Last updated: " ++ timestamp ++ "</p>"
                            , "  <p class=\"meeting-count\">Total meetings: " ++ show (length meetings) ++ "</p>"
                            , "  <p>Online meetings: " ++ show (fromMaybe 0 $ M.lookup "online" stats) ++ 
                              " | Wheelchair accessible: " ++ show (fromMaybe 0 $ M.lookup "wheelchair_accessible" stats) ++ "</p>"
                            , "  <h2>Neighborhoods</h2>"
                            , "  <div class=\"subregion-list\">"
                            , unlines $ map formatSubRegion sortedSubRegions
                            , "  </div>"
                            , "  <div class=\"quick-access\">"
                            , "    <h3>🔍 Quick Access</h3>"
                            , "    <div class=\"quick-links\">"
                            , "      <a href=\"/meetings/today/new-york/\" class=\"quick-link today\">"
                            , "        <span class=\"icon\">📅</span>"
                            , "        Today's NYC Meetings"
                            , "      </a>"
                            , "      <a href=\"/meetings/online/new-york/\" class=\"quick-link online\">"
                            , "        <span class=\"icon\">🌐</span>"
                            , "        NYC Online Meetings"
                            , "      </a>"
                            , "      <a href=\"/meetings/wheelchair-accessible/new-york/\" class=\"quick-link accessible\">"
                            , "        <span class=\"icon\">♿</span>"
                            , "        Accessible NYC Venues"
                            , "      </a>"
                            , "    </div>"
                            , "  </div>"
                            , "</div>"
                            ]
                        
                        formatSubRegion (sr, count) = 
                            "    <div class=\"subregion-item\">" ++
                            "      <a href=\"/meetings/new-york/" ++ normalizeForUrl sr ++ "/\">" ++ 
                            T.unpack sr ++ " (" ++ show count ++ " meetings)</a>" ++
                            "    </div>"
                        
                        meetingsCtx = 
                            constField "title" "New York City AA Meetings" `mappend`
                            constField "description" "Find AA meetings in NYC neighborhoods" `mappend`
                            constField "breadcrumbs" "<a href=\"/meetings/\">All Meetings</a> > New York City" `mappend`
                            defaultContext
                    in do
                        makeItem content
                            >>= loadAndApplyTemplate "templates/meetings-base.html" meetingsCtx
                            >>= relativizeUrls
                Left err -> makeItem $ "<p>Error loading meetings: " ++ err ++ "</p>"
    
    -- Generate all NYC sub-region pages
    nyData <- preprocess $ do
        result <- fetchNewYorkMeetingsWithTime
        case result of
            Right (meetings, timestamp) -> return $ Just (meetings, timestamp)
            Left _ -> return Nothing
    
    case nyData of
        Just (meetings, timestamp) -> generateSubRegionPages "New York City" "new-york" meetings timestamp
        Nothing -> return ()
    
    -- Create Seattle online meetings page
    create ["meetings/online/seattle/index.html"] $ do
        route idRoute
        compile $ do
            seattleResult <- unsafeCompiler fetchSeattleMeetingsWithTime
            
            let content = case seattleResult of
                    Right (seattleMeetings, timestamp) ->
                        let onlineMeetings = filterOnlineMeetings seattleMeetings
                            groupedByDay = groupMeetingsByDay onlineMeetings
                        in unlines
                            [ "<div class=\"online-meetings\">"
                            , "  <div class=\"meeting-summary\">"
                            , "    <p class=\"meeting-count\">Seattle online meetings: " ++ show (length onlineMeetings) ++ "</p>"
                            , "    <p class=\"last-updated\">Last updated: " ++ timestamp ++ "</p>"
                            , "  </div>"
                            , if not (null onlineMeetings)
                              then concatMap (formatRegionalOnlineDay groupedByDay) [0..6]
                              else "<p>No online meetings currently scheduled in Seattle.</p>"
                            , "</div>"
                            ]
                    _ -> "<p>Error loading meeting data</p>"
                
                formatRegionalOnlineDay grouped dayNum =
                    case M.lookup dayNum grouped of
                        Nothing -> ""
                        Just dayMeetings ->
                            if null dayMeetings then ""
                            else let dayName = M.findWithDefault "Unknown" dayNum dayNames
                                     sortedMeetings = sortBy (compare `on` time) dayMeetings
                                 in "<h2>" ++ dayName ++ " (" ++ show (length dayMeetings) ++ " online meetings)</h2>\n" ++
                                    "<div class=\"day-meetings\">\n" ++
                                    unlines (map formatEnhancedMeetingHTML sortedMeetings) ++
                                    "</div>\n"
                
                meetingsCtx = 
                    constField "title" "Seattle Online AA Meetings" `mappend`
                    constField "description" "Find online AA meetings in Seattle with Zoom links" `mappend`
                    constField "breadcrumbs" "<a href=\"/meetings/\">All Meetings</a> > <a href=\"/meetings/online/\">Online Meetings</a> > Seattle" `mappend`
                    defaultContext
            
            makeItem content
                >>= loadAndApplyTemplate "templates/meetings-base.html" meetingsCtx
                >>= relativizeUrls
    
    -- Create New York online meetings page
    create ["meetings/online/new-york/index.html"] $ do
        route idRoute
        compile $ do
            nyResult <- unsafeCompiler fetchNewYorkMeetingsWithTime
            
            let content = case nyResult of
                    Right (nyMeetings, timestamp) ->
                        let onlineMeetings = filterOnlineMeetings nyMeetings
                            groupedByDay = groupMeetingsByDay onlineMeetings
                        in unlines
                            [ "<div class=\"online-meetings\">"
                            , "  <div class=\"meeting-summary\">"
                            , "    <p class=\"meeting-count\">New York City online meetings: " ++ show (length onlineMeetings) ++ "</p>"
                            , "    <p class=\"last-updated\">Last updated: " ++ timestamp ++ "</p>"
                            , "  </div>"
                            , if not (null onlineMeetings)
                              then concatMap (formatRegionalOnlineDay groupedByDay) [0..6]
                              else "<p>No online meetings currently scheduled in New York City.</p>"
                            , "</div>"
                            ]
                    _ -> "<p>Error loading meeting data</p>"
                
                formatRegionalOnlineDay grouped dayNum =
                    case M.lookup dayNum grouped of
                        Nothing -> ""
                        Just dayMeetings ->
                            if null dayMeetings then ""
                            else let dayName = M.findWithDefault "Unknown" dayNum dayNames
                                     sortedMeetings = sortBy (compare `on` time) dayMeetings
                                 in "<h2>" ++ dayName ++ " (" ++ show (length dayMeetings) ++ " online meetings)</h2>\n" ++
                                    "<div class=\"day-meetings\">\n" ++
                                    unlines (map formatEnhancedMeetingHTML sortedMeetings) ++
                                    "</div>\n"
                
                meetingsCtx = 
                    constField "title" "New York City Online AA Meetings" `mappend`
                    constField "description" "Find online AA meetings in NYC with Zoom links" `mappend`
                    constField "breadcrumbs" "<a href=\"/meetings/\">All Meetings</a> > <a href=\"/meetings/online/\">Online Meetings</a> > New York City" `mappend`
                    defaultContext
            
            makeItem content
                >>= loadAndApplyTemplate "templates/meetings-base.html" meetingsCtx
                >>= relativizeUrls
    
    -- Create online meetings navigation hub
    create ["meetings/online/index.html"] $ do
        route idRoute
        compile $ do
            seattleResult <- unsafeCompiler fetchSeattleMeetingsWithTime
            nyResult <- unsafeCompiler fetchNewYorkMeetingsWithTime
            
            let content = case (seattleResult, nyResult) of
                    (Right (seattleMeetings, _), Right (nyMeetings, _)) ->
                        let allMeetings = seattleMeetings ++ nyMeetings
                            onlineMeetings = filterOnlineMeetings allMeetings
                            seattleOnline = filterOnlineMeetings seattleMeetings
                            nyOnline = filterOnlineMeetings nyMeetings
                            groupedByDay = groupMeetingsByDay onlineMeetings
                        in unlines
                            [ "<div class=\"regions-overview\">"
                            , "  <div class=\"meeting-summary\">"
                            , "    <h2>🌐 Online AA Meetings</h2>"
                            , "    <p class=\"meeting-count\">Total online meetings across all regions: " ++ show (length onlineMeetings) ++ "</p>"
                            , "    <div class=\"stats-grid\">"
                            , "      <span class=\"stat\"><strong>" ++ show (length seattleOnline) ++ "</strong><br>Seattle Online</span>"
                            , "      <span class=\"stat\"><strong>" ++ show (length nyOnline) ++ "</strong><br>NYC Online</span>"
                            , "    </div>"
                            , "  </div>"
                            , "  <h3>Select Your Region</h3>"
                            , "  <div class=\"region-cards\">"
                            , "    <a href=\"/meetings/online/seattle/\" class=\"region-card featured\">"
                            , "      <h4>🌊 Seattle Online Meetings</h4>"
                            , "      <p><strong>" ++ show (length seattleOnline) ++ " online meetings</strong> available</p>"
                            , "      <p>Join from anywhere with Zoom links and conference details</p>"
                            , "    </a>"
                            , "    <a href=\"/meetings/online/new-york/\" class=\"region-card featured\">"
                            , "      <h4>🗽 New York City Online Meetings</h4>"
                            , "      <p><strong>" ++ show (length nyOnline) ++ " online meetings</strong> available</p>"
                            , "      <p>Join from anywhere with Zoom links and conference details</p>"
                            , "    </a>"
                            , "  </div>"
                            , "  <div class=\"ai-features\">"
                            , "    <h3>💡 About Online Meetings</h3>"
                            , "    <ul>"
                            , "      <li>All meetings include direct Zoom or conference links</li>"
                            , "      <li>Meetings are organized by day and time for easy navigation</li>"
                            , "      <li>Updated regularly from official AA sources</li>"
                            , "      <li>Perfect for remote attendance or when traveling</li>"
                            , "    </ul>"
                            , "  </div>"
                            , "</div>"
                            ]
                    _ -> "<p>Error loading meeting data</p>"
                
                meetingsCtx = 
                    constField "title" "Online AA Meetings" `mappend`
                    constField "description" "Find online AA meetings with Zoom links and conference details" `mappend`
                    constField "breadcrumbs" "<a href=\"/meetings/\">All Meetings</a> > Online Meetings" `mappend`
                    defaultContext
            
            makeItem content
                >>= loadAndApplyTemplate "templates/meetings-base.html" meetingsCtx
                >>= relativizeUrls
    
    -- Create Seattle wheelchair accessible meetings page
    create ["meetings/wheelchair-accessible/seattle/index.html"] $ do
        route idRoute
        compile $ do
            seattleResult <- unsafeCompiler fetchSeattleMeetingsWithTime
            
            let content = case seattleResult of
                    Right (seattleMeetings, timestamp) ->
                        let accessibleMeetings = filterAccessibleMeetings seattleMeetings
                            groupedByDay = groupMeetingsByDay accessibleMeetings
                        in unlines
                            [ "<div class=\"accessible-meetings\">"
                            , "  <div class=\"meeting-summary\">"
                            , "    <p class=\"meeting-count\">Seattle wheelchair accessible meetings: " ++ show (length accessibleMeetings) ++ "</p>"
                            , "    <p class=\"last-updated\">Last updated: " ++ timestamp ++ "</p>"
                            , "  </div>"
                            , if not (null accessibleMeetings)
                              then concatMap (formatAccessibleDay groupedByDay "Seattle") [0..6]
                              else "<p>No wheelchair accessible meetings currently scheduled in Seattle.</p>"
                            , "</div>"
                            ]
                    _ -> "<p>Error loading meeting data</p>"
                
                formatAccessibleDay grouped regionName dayNum =
                    case M.lookup dayNum grouped of
                        Nothing -> ""
                        Just dayMeetings ->
                            if null dayMeetings then ""
                            else let dayName = M.findWithDefault "Unknown" dayNum dayNames
                                     sortedMeetings = sortBy (compare `on` time) dayMeetings
                                 in "<h2>♿ " ++ dayName ++ " (" ++ show (length dayMeetings) ++ " accessible meetings)</h2>\n" ++
                                    "<div class=\"day-meetings\">\n" ++
                                    unlines (map formatEnhancedMeetingHTML sortedMeetings) ++
                                    "</div>\n"
                
                meetingsCtx = 
                    constField "title" "Seattle Wheelchair Accessible AA Meetings" `mappend`
                    constField "description" "Find wheelchair accessible AA meetings in Seattle" `mappend`
                    constField "breadcrumbs" "<a href=\"/meetings/\">All Meetings</a> > <a href=\"/meetings/wheelchair-accessible/\">Wheelchair Accessible</a> > Seattle" `mappend`
                    defaultContext
            
            makeItem content
                >>= loadAndApplyTemplate "templates/meetings-base.html" meetingsCtx
                >>= relativizeUrls
    
    -- Create New York wheelchair accessible meetings page
    create ["meetings/wheelchair-accessible/new-york/index.html"] $ do
        route idRoute
        compile $ do
            nyResult <- unsafeCompiler fetchNewYorkMeetingsWithTime
            
            let content = case nyResult of
                    Right (nyMeetings, timestamp) ->
                        let accessibleMeetings = filterAccessibleMeetings nyMeetings
                            groupedByDay = groupMeetingsByDay accessibleMeetings
                        in unlines
                            [ "<div class=\"accessible-meetings\">"
                            , "  <div class=\"meeting-summary\">"
                            , "    <p class=\"meeting-count\">New York City wheelchair accessible meetings: " ++ show (length accessibleMeetings) ++ "</p>"
                            , "    <p class=\"last-updated\">Last updated: " ++ timestamp ++ "</p>"
                            , "  </div>"
                            , if not (null accessibleMeetings)
                              then concatMap (formatAccessibleDay groupedByDay "NYC") [0..6]
                              else "<p>No wheelchair accessible meetings currently scheduled in New York City.</p>"
                            , "</div>"
                            ]
                    _ -> "<p>Error loading meeting data</p>"
                
                formatAccessibleDay grouped regionName dayNum =
                    case M.lookup dayNum grouped of
                        Nothing -> ""
                        Just dayMeetings ->
                            if null dayMeetings then ""
                            else let dayName = M.findWithDefault "Unknown" dayNum dayNames
                                     sortedMeetings = sortBy (compare `on` time) dayMeetings
                                 in "<h2>♿ " ++ dayName ++ " (" ++ show (length dayMeetings) ++ " accessible meetings)</h2>\n" ++
                                    "<div class=\"day-meetings\">\n" ++
                                    unlines (map formatEnhancedMeetingHTML sortedMeetings) ++
                                    "</div>\n"
                
                meetingsCtx = 
                    constField "title" "New York City Wheelchair Accessible AA Meetings" `mappend`
                    constField "description" "Find wheelchair accessible AA meetings in NYC" `mappend`
                    constField "breadcrumbs" "<a href=\"/meetings/\">All Meetings</a> > <a href=\"/meetings/wheelchair-accessible/\">Wheelchair Accessible</a> > New York City" `mappend`
                    defaultContext
            
            makeItem content
                >>= loadAndApplyTemplate "templates/meetings-base.html" meetingsCtx
                >>= relativizeUrls
    
    -- Create wheelchair accessible meetings navigation hub
    create ["meetings/wheelchair-accessible/index.html"] $ do
        route idRoute
        compile $ do
            seattleResult <- unsafeCompiler fetchSeattleMeetingsWithTime
            nyResult <- unsafeCompiler fetchNewYorkMeetingsWithTime
            
            let content = case (seattleResult, nyResult) of
                    (Right (seattleMeetings, _), Right (nyMeetings, _)) ->
                        let allMeetings = seattleMeetings ++ nyMeetings
                            accessibleMeetings = filterAccessibleMeetings allMeetings
                            seattleAccessible = filterAccessibleMeetings seattleMeetings
                            nyAccessible = filterAccessibleMeetings nyMeetings
                            groupedByRegion = M.fromList 
                                [ ("Seattle", sortBy (compare `on` (\m -> (day m, time m))) seattleAccessible)
                                , ("New York City", sortBy (compare `on` (\m -> (day m, time m))) nyAccessible)
                                ]
                        in unlines
                            [ "<div class=\"regions-overview\">"
                            , "  <div class=\"meeting-summary\">"
                            , "    <h2>♿ Wheelchair Accessible AA Meetings</h2>"
                            , "    <p class=\"meeting-count\">Total wheelchair accessible meetings across all regions: " ++ show (length accessibleMeetings) ++ "</p>"
                            , "    <div class=\"stats-grid\">"
                            , "      <span class=\"stat\"><strong>" ++ show (length seattleAccessible) ++ "</strong><br>Seattle Accessible</span>"
                            , "      <span class=\"stat\"><strong>" ++ show (length nyAccessible) ++ "</strong><br>NYC Accessible</span>"
                            , "    </div>"
                            , "  </div>"
                            , "  <h3>Select Your Region</h3>"
                            , "  <div class=\"region-cards\">"
                            , "    <a href=\"/meetings/wheelchair-accessible/seattle/\" class=\"region-card featured\">"
                            , "      <h4>🌊 Seattle Accessible Meetings</h4>"
                            , "      <p><strong>" ++ show (length seattleAccessible) ++ " wheelchair accessible meetings</strong> available</p>"
                            , "      <p>All venues confirmed for wheelchair accessibility</p>"
                            , "    </a>"
                            , "    <a href=\"/meetings/wheelchair-accessible/new-york/\" class=\"region-card featured\">"
                            , "      <h4>🗽 NYC Accessible Meetings</h4>"
                            , "      <p><strong>" ++ show (length nyAccessible) ++ " wheelchair accessible meetings</strong> available</p>"
                            , "      <p>All venues confirmed for wheelchair accessibility</p>"
                            , "    </a>"
                            , "  </div>"
                            , "  <div class=\"ai-features\">"
                            , "    <h3>♿ About Accessible Meetings</h3>"
                            , "    <ul>"
                            , "      <li>All meetings marked with wheelchair accessibility have been verified</li>"
                            , "      <li>Venues include ramps, elevators, or ground-floor access</li>"
                            , "      <li>Meetings are organized by day and time for easy planning</li>"
                            , "      <li>Contact information provided for additional accessibility questions</li>"
                            , "    </ul>"
                            , "  </div>"
                            , "</div>"
                            ]
                    _ -> "<p>Error loading meeting data</p>"
                
                meetingsCtx = 
                    constField "title" "Wheelchair Accessible AA Meetings" `mappend`
                    constField "description" "Find wheelchair accessible AA meetings in Seattle and NYC" `mappend`
                    constField "breadcrumbs" "<a href=\"/meetings/\">All Meetings</a> > Wheelchair Accessible" `mappend`
                    defaultContext
            
            makeItem content
                >>= loadAndApplyTemplate "templates/meetings-base.html" meetingsCtx
                >>= relativizeUrls
    
    -- Create Seattle today's meetings page
    create ["meetings/today/seattle/index.html"] $ do
        route idRoute
        compile $ do
            seattleResult <- unsafeCompiler fetchSeattleMeetingsWithTime
            
            content <- case seattleResult of
                Right (seattleMeetings, timestamp) -> do
                    todaysMeetings <- unsafeCompiler $ getTodaysMeetings seattleMeetings
                    todayNum <- unsafeCompiler getTodayDayOfWeek
                    let dayName = M.findWithDefault "Today" todayNum dayNames
                        sortedMeetings = sortBy (compare `on` time) todaysMeetings
                    
                    return $ unlines
                        [ "<div class=\"todays-meetings\">"
                        , "  <div class=\"meeting-summary\">"
                        , "    <p class=\"meeting-count\">Seattle meetings today (" ++ dayName ++ "): " ++ show (length todaysMeetings) ++ "</p>"
                        , "    <p class=\"last-updated\">Last updated: " ++ timestamp ++ "</p>"
                        , "  </div>"
                        , if not (null todaysMeetings)
                          then "<div class=\"region-meetings\">\n" ++
                               unlines (map formatEnhancedMeetingHTML sortedMeetings) ++
                               "</div>\n"
                          else "<p>No meetings scheduled for today in Seattle.</p>"
                        , "</div>"
                        ]
                _ -> return "<p>Error loading meeting data</p>"
            
            let meetingsCtx = 
                    constField "title" "Today's Seattle AA Meetings" `mappend`
                    constField "description" "Find AA meetings happening today in Seattle" `mappend`
                    constField "breadcrumbs" "<a href=\"/meetings/\">All Meetings</a> > <a href=\"/meetings/today/\">Today's Meetings</a> > Seattle" `mappend`
                    defaultContext
            
            makeItem content
                >>= loadAndApplyTemplate "templates/meetings-base.html" meetingsCtx
                >>= relativizeUrls
    
    -- Create New York today's meetings page
    create ["meetings/today/new-york/index.html"] $ do
        route idRoute
        compile $ do
            nyResult <- unsafeCompiler fetchNewYorkMeetingsWithTime
            
            content <- case nyResult of
                Right (nyMeetings, timestamp) -> do
                    todaysMeetings <- unsafeCompiler $ getTodaysMeetings nyMeetings
                    todayNum <- unsafeCompiler getTodayDayOfWeek
                    let dayName = M.findWithDefault "Today" todayNum dayNames
                        sortedMeetings = sortBy (compare `on` time) todaysMeetings
                    
                    return $ unlines
                        [ "<div class=\"todays-meetings\">"
                        , "  <div class=\"meeting-summary\">"
                        , "    <p class=\"meeting-count\">New York City meetings today (" ++ dayName ++ "): " ++ show (length todaysMeetings) ++ "</p>"
                        , "    <p class=\"last-updated\">Last updated: " ++ timestamp ++ "</p>"
                        , "  </div>"
                        , if not (null todaysMeetings)
                          then "<div class=\"region-meetings\">\n" ++
                               unlines (map formatEnhancedMeetingHTML sortedMeetings) ++
                               "</div>\n"
                          else "<p>No meetings scheduled for today in New York City.</p>"
                        , "</div>"
                        ]
                _ -> return "<p>Error loading meeting data</p>"
            
            let meetingsCtx = 
                    constField "title" "Today's NYC AA Meetings" `mappend`
                    constField "description" "Find AA meetings happening today in New York City" `mappend`
                    constField "breadcrumbs" "<a href=\"/meetings/\">All Meetings</a> > <a href=\"/meetings/today/\">Today's Meetings</a> > New York City" `mappend`
                    defaultContext
            
            makeItem content
                >>= loadAndApplyTemplate "templates/meetings-base.html" meetingsCtx
                >>= relativizeUrls
    
    -- Create today's meetings navigation hub
    create ["meetings/today/index.html"] $ do
        route idRoute
        compile $ do
            seattleResult <- unsafeCompiler fetchSeattleMeetingsWithTime
            nyResult <- unsafeCompiler fetchNewYorkMeetingsWithTime
            
            let content = case (seattleResult, nyResult) of
                    (Right (seattleMeetings, _), Right (nyMeetings, _)) -> do
                        let allMeetings = seattleMeetings ++ nyMeetings
                        todaysMeetings <- unsafeCompiler $ getTodaysMeetings allMeetings
                        todayNum <- unsafeCompiler getTodayDayOfWeek
                        let dayName = M.findWithDefault "Today" todayNum dayNames
                            seattleToday = filter (\m -> case region m of Just "Seattle" -> True; _ -> False) todaysMeetings
                            nyToday = filter (\m -> case region m of Just _ -> True; _ -> False) todaysMeetings
                            totalCount = length todaysMeetings
                        
                        return $ unlines
                            [ "<div class=\"regions-overview\">"
                            , "  <div class=\"meeting-summary\">"
                            , "    <h2>📅 Today's AA Meetings - " ++ dayName ++ "</h2>"
                            , "    <p class=\"meeting-count\">Total meetings across all regions today: " ++ show totalCount ++ "</p>"
                            , "    <div class=\"stats-grid\">"
                            , "      <span class=\"stat\"><strong>" ++ show (length seattleToday) ++ "</strong><br>Seattle Today</span>"
                            , "      <span class=\"stat\"><strong>" ++ show (length nyToday) ++ "</strong><br>NYC Today</span>"
                            , "    </div>"
                            , "  </div>"
                            , "  <h3>Select Your Region</h3>"
                            , "  <div class=\"region-cards\">"
                            , "    <a href=\"/meetings/today/seattle/\" class=\"region-card featured\">"
                            , "      <h4>🌊 Seattle - " ++ dayName ++ "</h4>"
                            , "      <p><strong>" ++ show (length seattleToday) ++ " meetings today</strong></p>"
                            , "      <p>View all meetings happening today in Seattle</p>"
                            , "    </a>"
                            , "    <a href=\"/meetings/today/new-york/\" class=\"region-card featured\">"
                            , "      <h4>🗽 New York City - " ++ dayName ++ "</h4>"
                            , "      <p><strong>" ++ show (length nyToday) ++ " meetings today</strong></p>"
                            , "      <p>View all meetings happening today in NYC</p>"
                            , "    </a>"
                            , "  </div>"
                            , "  <div class=\"ai-features\">"
                            , "    <h3>📅 About Today's Meetings</h3>"
                            , "    <ul>"
                            , "      <li>Updated dynamically to show only today's meetings</li>"
                            , "      <li>Sorted by time for easy planning throughout the day</li>"
                            , "      <li>Includes both in-person and online meeting options</li>"
                            , "      <li>Perfect for finding a meeting right now or planning your day</li>"
                            , "    </ul>"
                            , "  </div>"
                            , "</div>"
                            ]
                    _ -> return "<p>Error loading meeting data</p>"
            
            content >>= \c -> do
                let meetingsCtx = 
                        constField "title" "Today's AA Meetings" `mappend`
                        constField "description" "Find AA meetings happening today in Seattle and NYC" `mappend`
                        constField "breadcrumbs" "<a href=\"/meetings/\">All Meetings</a> > Today's Meetings" `mappend`
                        defaultContext
                
                makeItem c
                    >>= loadAndApplyTemplate "templates/meetings-base.html" meetingsCtx
                    >>= relativizeUrls
    
    -- Generate sitemap.xml
    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            seattleResult <- unsafeCompiler fetchSeattleMeetingsWithTime
            nyResult <- unsafeCompiler fetchNewYorkMeetingsWithTime
            
            let baseUrl = "https://aa-meetings-list.vercel.app" -- Update with actual domain
                
                staticUrls = 
                    [ baseUrl ++ "/"
                    , baseUrl ++ "/meetings/"
                    , baseUrl ++ "/meetings/seattle/"
                    , baseUrl ++ "/meetings/new-york/"
                    , baseUrl ++ "/meetings/online/"
                    , baseUrl ++ "/meetings/wheelchair-accessible/"
                    , baseUrl ++ "/meetings/today/"
                    , baseUrl ++ "/about.html"
                    , baseUrl ++ "/contact.html"
                    ]
                
                generateSubRegionUrls cityPath meetings = 
                    let subRegions = getUniqueSubRegions meetings
                        subRegionUrls = map (\sr -> baseUrl ++ "/meetings/" ++ cityPath ++ "/" ++ normalizeForUrl sr ++ "/") subRegions
                        dayUrls = concatMap generateDayUrls subRegions
                        generateDayUrls sr = 
                            let srPath = normalizeForUrl sr
                                dayCounts = countMeetingsBySubRegionDay sr meetings
                                validDays = M.keys dayCounts
                            in map (\dayNum -> 
                                let dayName = map toLower $ M.findWithDefault "unknown" dayNum dayNames
                                in baseUrl ++ "/meetings/" ++ cityPath ++ "/" ++ srPath ++ "/" ++ dayName ++ "/") validDays
                    in subRegionUrls ++ dayUrls
                
                allUrls = case (seattleResult, nyResult) of
                    (Right (seattleMeetings, _), Right (nyMeetings, _)) ->
                        staticUrls ++ 
                        generateSubRegionUrls "seattle" seattleMeetings ++
                        generateSubRegionUrls "new-york" nyMeetings
                    _ -> staticUrls
            
            makeItem $ generateSitemapEntries allUrls

    -- Legacy post system removed - regional content now integrated into homepage

    match "index.html" $ do
        route idRoute
        compile $ do
            seattleResult <- unsafeCompiler fetchSeattleMeetingsWithTime
            nyResult <- unsafeCompiler fetchNewYorkMeetingsWithTime
            
            let regionalContent = case (seattleResult, nyResult) of
                    (Right (seattleMeetings, _), Right (nyMeetings, _)) ->
                        let seattleStats = generateMeetingStats seattleMeetings
                            nyStats = generateMeetingStats nyMeetings
                            totalMeetings = length seattleMeetings + length nyMeetings
                            totalOnline = (fromMaybe 0 $ M.lookup "online" seattleStats) + 
                                        (fromMaybe 0 $ M.lookup "online" nyStats)
                            totalAccessible = (fromMaybe 0 $ M.lookup "wheelchair_accessible" seattleStats) + 
                                            (fromMaybe 0 $ M.lookup "wheelchair_accessible" nyStats)
                        in unlines
                            [ "<div class=\"regions-overview\">"
                            , "  <div class=\"meeting-summary\">"
                            , "    <h3>📊 Directory Statistics</h3>"
                            , "    <div class=\"stats-grid\">"
                            , "      <span class=\"stat\"><strong>" ++ show totalMeetings ++ "</strong><br>Total Meetings</span>"
                            , "      <span class=\"stat\"><strong>" ++ show totalOnline ++ "</strong><br>Online Meetings</span>"
                            , "      <span class=\"stat\"><strong>" ++ show totalAccessible ++ "</strong><br>Accessible Venues</span>"
                            , "      <span class=\"stat\"><strong>2</strong><br>Regions</span>"
                            , "    </div>"
                            , "  </div>"
                            , ""
                            , "  <h3>🗺️ Select Your Region</h3>"
                            , "  <div class=\"region-cards\">"
                            , "    <div class=\"region-card featured\">"
                            , "      <h4><a href=\"/meetings/seattle/\">Seattle Area</a></h4>"
                            , "      <p><strong>" ++ show (length seattleMeetings) ++ " meetings</strong> across " ++ 
                              show (length $ getUniqueSubRegions seattleMeetings) ++ " neighborhoods</p>"
                            , "      <div class=\"region-highlights\">"
                            , "        <span>🌐 " ++ show (fromMaybe 0 $ M.lookup "online" seattleStats) ++ " online</span>"
                            , "        <span>♿ " ++ show (fromMaybe 0 $ M.lookup "wheelchair_accessible" seattleStats) ++ " accessible</span>"
                            , "      </div>"
                            , "    </div>"
                            , "    <div class=\"region-card featured\">"
                            , "      <h4><a href=\"/meetings/new-york/\">New York City</a></h4>"
                            , "      <p><strong>" ++ show (length nyMeetings) ++ " meetings</strong> across " ++ 
                              show (length $ getUniqueSubRegions nyMeetings) ++ " neighborhoods</p>"
                            , "      <div class=\"region-highlights\">"
                            , "        <span>🌐 " ++ show (fromMaybe 0 $ M.lookup "online" nyStats) ++ " online</span>"
                            , "        <span>♿ " ++ show (fromMaybe 0 $ M.lookup "wheelchair_accessible" nyStats) ++ " accessible</span>"
                            , "      </div>"
                            , "    </div>"
                            , "  </div>"
                            , ""
                            , "  <div class=\"quick-access-home\">"
                            , "    <h4>🚀 Quick Access</h4>"
                            , "    <div class=\"quick-links-home\">"
                            , "      <a href=\"/meetings/today/\" class=\"quick-link-home today\">"
                            , "        <span class=\"icon\">📅</span>"
                            , "        <div><strong>Today's Meetings</strong><br><small>See what's happening now</small></div>"
                            , "      </a>"
                            , "      <a href=\"/meetings/online/\" class=\"quick-link-home online\">"
                            , "        <span class=\"icon\">🌐</span>"
                            , "        <div><strong>Online Meetings</strong><br><small>Join from anywhere</small></div>"
                            , "      </a>"
                            , "      <a href=\"/meetings/wheelchair-accessible/\" class=\"quick-link-home accessible\">"
                            , "        <span class=\"icon\">♿</span>"
                            , "        <div><strong>Accessible Venues</strong><br><small>Wheelchair friendly</small></div>"
                            , "      </a>"
                            , "    </div>"
                            , "  </div>"
                            , "</div>"
                            ]
                    _ -> "<p>Loading meeting data...</p>"
                
                indexCtx = 
                    constField "regional-content" regionalContent `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
-- End of file - All legacy post system references removed