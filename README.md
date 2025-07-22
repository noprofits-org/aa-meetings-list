# AA Meetings List - Architecture Analysis & Improvement Roadmap

## Executive Summary

This Hakyll-based website aggregates AA meeting data from multiple intergroups to make it accessible to AI crawlers. Currently, the site creates massive single-page listings (NYC: 53,275 lines, Seattle: 31,141 lines) that exceed AI chat context limits. This document analyzes the current architecture and provides a roadmap for restructuring the data into smaller, more digestible chunks while maintaining discoverability.

## 1. Current Architecture Analysis

### Data Flow
```
API (TSML JSON) → Vercel Proxy → Hakyll Compiler → HTML Output
```

### Key Components

#### Meeting Data Type (site.hs:23-31)
Currently captures only 7 fields from the available ~35 fields in the API:
```haskell
data Meeting = Meeting
    { meetingName :: Text           -- Meeting name
    , day :: Maybe Int              -- Day of week (0-6)
    , time :: Maybe Text            -- Time in 24hr format
    , location :: Maybe Text        -- Location name
    , formatted_address :: Maybe Text -- Full address
    , types :: Maybe [Text]         -- Meeting types (e.g., "O", "TC", "ONL")
    , notes :: Maybe Text           -- Additional notes
    }
```

#### Data Processing Pipeline
1. **Fetch** - `fetchSeattleMeetingsWithTime` and `fetchNewYorkMeetingsWithTime` (site.hs:72-103)
   - Fetches JSON data through Vercel CORS proxy
   - Adds timestamp for "last updated" display
   - Returns tuple of `([Meeting], String)`

2. **Group** - `groupMeetingsByDay` (site.hs:106-118)
   - Groups meetings by weekday (0-6)
   - Sorts meetings within each day by time
   - Returns `Map Int [Meeting]`

3. **Format** - `formatMeetingsHTML` (site.hs:121-149)
   - Converts grouped meetings to HTML
   - Generates `<h2>` for each day with meeting count
   - Wraps meetings in `<div class="meeting">` blocks

4. **Compile** - Hakyll compilers (site.hs:152-182)
   - `seattleMeetingsCompiler` and `newYorkMeetingsCompiler` generate HTML
   - Timestamp compilers track update times
   - Post compiler injects data via `SEATTLE_MEETINGS_DATA` placeholder

### Current Output Structure
```
/posts/seattle-meetings.html
  └── All Seattle meetings grouped by weekday
  
/posts/new-york-meetings.html
  └── All NYC meetings grouped by weekday
```

## 2. Data Structure Deep Dive

### Complete Meeting Object Fields (from API analysis)

#### Core Meeting Information
- `id` (Int) - Unique meeting identifier
- `name` (String) - Meeting name
- `slug` (String) - URL-friendly name
- `day` (Int) - Day of week (0=Sunday, 6=Saturday)
- `time` (String) - Start time in 24hr format (e.g., "19:00")
- `end_time` (String) - End time
- `time_formatted` (String) - Human-readable time (e.g., "7:00 pm")

#### Location Data
- `location` (String) - Venue name
- `location_id` (Int) - Location identifier
- `formatted_address` (String) - Full address
- `latitude` (Float) - GPS latitude
- `longitude` (Float) - GPS longitude
- `approximate` (String) - "yes"/"no" for location accuracy
- `location_url` (String) - Location detail page
- `location_notes` (String) - Accessibility/parking info

#### Meeting Characteristics
- `types` ([String]) - Meeting types/flags:
  - "O" = Open, "C" = Closed
  - "TC" = Temporarily Closed
  - "ONL" = Online
  - "M" = Men, "W" = Women
  - "YP" = Young People
  - "X" = Wheelchair Accessible
  - "BA" = Babysitting Available
- `attendance_option` (String) - "online", "in_person", or "inactive"
- `conference_url` (String) - Zoom/online meeting URL
- `notes` (String) - Additional meeting notes

#### Geographic Organization
- `region` (String) - Primary region (e.g., "Seattle")
- `regions` ([String]) - All applicable regions
- `sub_region` (String) - Neighborhood/area
- `region_id` (Int) - Region identifier
- `district` (String) - AA district
- `district_id` (Int) - District identifier

#### Group & Administrative
- `group` (String) - AA group name
- `group_id` (Int) - Group identifier
- `entity` (String) - Managing organization
- `entity_email`, `entity_url`, `entity_phone` - Contact info
- `author` (String) - Meeting creator
- `updated` (String) - Last update timestamp

### Sample Meeting Objects

```json
{
  "id": 70296,
  "name": "Primary Purpose Big Book Study",
  "slug": "primary-purpose-big-book-study-2",
  "day": 0,
  "time": "19:00",
  "end_time": "20:15",
  "time_formatted": "7:00 pm",
  "formatted_address": "10213 40th Ave SW, Seattle, WA 98146",
  "types": ["C", "B"],
  "notes": "Meet in portable B",
  "location": "Hope Lutheran Church",
  "group": "Primary Purpose",
  "district": "31",
  "region": "Seattle",
  "sub_region": "White Center",
  "attendance_option": "in_person",
  "latitude": 47.510847,
  "longitude": -122.384247
}
```

## 3. Chunking Strategy Recommendations

### Hierarchical Organization Options

#### Option A: Geographic → Day → Time (Recommended)
```
/meetings/{region}/{sub-region}/{day}/{time-period}/
Example: /meetings/seattle/capitol-hill/monday/morning/
```

**Pros:**
- Natural geographic browsing
- Predictable URL structure
- Good for local searches
- Reduces page size by ~95%

**Cons:**
- Requires sub-region data extraction
- May create very small pages in some areas

#### Option B: Day → Region → Meeting Type
```
/meetings/{day}/{region}/{type}/
Example: /meetings/monday/seattle/online/
```

**Pros:**
- Simple implementation
- Good for "what's today" queries
- Leverages existing day grouping

**Cons:**
- Less intuitive navigation
- Larger page sizes

#### Option C: Meeting Type → Region → Day
```
/meetings/{type}/{region}/{day}/
Example: /meetings/online/seattle/monday/
```

**Pros:**
- Excellent for accessibility needs
- Groups online meetings together
- Good for specific need searches

**Cons:**
- Requires parsing type codes
- May duplicate meetings with multiple types

### Recommended Implementation: Hybrid Approach

1. **Primary hierarchy:** Region → Sub-region → Day
2. **Alternative indexes:** By type, by time, by accessibility
3. **Page size target:** 50-100 meetings per page (approximately 500-1000 lines of HTML)

### URL Structure
```
# Primary pages
/meetings/                          # Landing page with all regions
/meetings/seattle/                  # Seattle overview with sub-regions
/meetings/seattle/capitol-hill/     # Sub-region with day links
/meetings/seattle/capitol-hill/monday/ # Actual meetings

# Alternative indexes
/meetings/online/                   # All online meetings
/meetings/wheelchair-accessible/    # Accessible venues
/meetings/today/                    # Dynamic today's meetings
```

## 4. Implementation Plan

### Phase 1: Enhance Meeting Data Type
```haskell
data Meeting = Meeting
    { -- Existing fields
    , meetingId :: Int
    , slug :: Text
    , endTime :: Maybe Text
    , timeFormatted :: Maybe Text
    , latitude :: Maybe Double
    , longitude :: Maybe Double
    , region :: Maybe Text
    , subRegion :: Maybe Text
    , district :: Maybe Text
    , group :: Maybe Text
    , attendanceOption :: Maybe Text
    , conferenceUrl :: Maybe Text
    , locationNotes :: Maybe Text
    }
```

### Phase 2: Create New Compilers

#### Sub-region Extraction
```haskell
extractSubRegions :: [Meeting] -> [Text]
groupBySubRegion :: [Meeting] -> Map Text [Meeting]
```

#### Chunked Page Generation
```haskell
-- Generate pages for each sub-region/day combination
subRegionDayCompiler :: Text -> Text -> Int -> Compiler (Item String)
```

### Phase 3: Modify Routing

```haskell
-- Create routes for chunked pages
match "meetings/*/*/*.html" $ do
    route idRoute
    compile $ do
        region <- getRegionFromPath
        subRegion <- getSubRegionFromPath
        day <- getDayFromPath
        meetings <- loadMeetingsForChunk region subRegion day
        -- Apply templates
```

### Phase 4: Navigation Structure

1. **Region Index Pages**
   - List of sub-regions with meeting counts
   - Map visualization (optional)
   - Quick filters

2. **Breadcrumb Navigation**
   ```
   Home > Seattle > Capitol Hill > Monday
   ```

3. **Cross-references**
   - "See also: Online meetings in Capitol Hill"
   - "View all wheelchair accessible meetings"

### Phase 5: Maintain Compatibility

1. Keep existing `/posts/seattle-meetings.html` as legacy
2. Add redirect or notice about new structure
3. Implement 301 redirects after transition period

## 5. AI Accessibility Optimization

### Page Size Guidelines
- **Target:** 500-1000 lines per page (5-10KB)
- **Maximum:** 2000 lines per page (20KB)
- **Meeting count:** 50-100 meetings per page

### Metadata Structure
```html
<head>
  <meta name="description" content="AA meetings in Capitol Hill, Seattle - Monday">
  <meta property="og:type" content="website">
  <meta property="og:title" content="Monday AA Meetings in Capitol Hill">
  
  <!-- Structured data -->
  <script type="application/ld+json">
  {
    "@context": "https://schema.org",
    "@type": "ItemList",
    "name": "AA Meetings",
    "itemListElement": [...]
  }
  </script>
</head>
```

### Navigation Aids for AI

1. **Semantic HTML**
   ```html
   <nav aria-label="Meeting filters">
   <section itemscope itemtype="https://schema.org/Event">
   ```

2. **Clear Hierarchical Structure**
   ```html
   <div class="meeting-region" data-region="seattle">
     <div class="meeting-subregion" data-subregion="capitol-hill">
       <div class="meeting-day" data-day="1" data-day-name="Monday">
   ```

3. **Index Pages with Clear Links**
   ```html
   <ul class="region-list">
     <li><a href="/meetings/seattle/capitol-hill/">Capitol Hill (127 meetings)</a></li>
     <li><a href="/meetings/seattle/ballard/">Ballard (89 meetings)</a></li>
   </ul>
   ```

### Discovery Optimization

1. **Sitemap.xml** - Auto-generate with all meeting page URLs
2. **Robots.txt** - Ensure all meeting pages are crawlable
3. **RSS/Atom feeds** - For recently updated meetings
4. **JSON-LD Event markup** - For each meeting

## 6. Technical Considerations

### Build Performance
- Incremental builds for changed regions only
- Parallel compilation of region pages
- Cache meeting data with TTL

### Static Hosting
- Pre-generate all pages at build time
- Use GitHub Actions for scheduled rebuilds
- Consider CDN for faster global access

### Data Quality
- Handle missing sub-region data
- Normalize location names
- Deduplicate meetings across sources

## 7. Migration Timeline

1. **Week 1-2:** Implement enhanced Meeting type and data extraction
2. **Week 3-4:** Create chunked page compilers and templates
3. **Week 5:** Build navigation and index pages
4. **Week 6:** Testing and optimization
5. **Week 7:** Deploy with legacy support
6. **Week 8:** Monitor and adjust based on usage

## 8. Success Metrics

- Page size reduction: 95%+ (from 50K to <2K lines)
- AI crawlability: 100% of meetings accessible
- Build time: <5 minutes for full rebuild
- User navigation: 3 clicks or less to any meeting

## Conclusion

The proposed restructuring will transform the current monolithic meeting pages into a hierarchical, easily navigable structure that serves both human users and AI crawlers effectively. By implementing geographic chunking with alternative indexes, we can reduce page sizes by 95% while improving discoverability and maintaining all current functionality.