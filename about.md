---
title: About This Project
---

# AI-Accessible AA Meetings Directory

## Project Background

The Greater Seattle AA Intergroup maintains an excellent meeting directory at [seattleaa.org/meetings](https://seattleaa.org/meetings). However, this site loads meeting data dynamically using JavaScript after the page renders, making the information invisible to AI chatbots, search engines, and other automated systems.

When someone asks ChatGPT, Claude, Gemini, or other AI assistants for help finding AA meetings, these tools can't access the meeting information because it's loaded client-side rather than being present in the initial HTML.

## The Technical Problem

Modern web applications often use JavaScript frameworks that load content after the initial page render. While this creates smooth user experiences, it creates a barrier for:

- **AI Assistants**: Can't read dynamically loaded content
- **Search Engines**: Have limited ability to index JavaScript-generated content
- **Screen Readers**: May have difficulty with dynamic content
- **Web Scrapers**: Can't access data that requires JavaScript execution

## Our Solution

This project creates a static HTML version of the Seattle AA meetings directory that solves these accessibility problems:

### Static HTML Generation
We fetch the same JSON data that powers the official site and generate static HTML pages with all meeting information embedded directly in the page content.

### Automated Updates  
The system automatically rebuilds the site weekly (and on code changes) to ensure the meeting information stays current with the official database.

### AI-Friendly Format
All meeting data is structured in semantic HTML that AI assistants can easily read, search, and reference when helping people find meetings.

## Technology Stack

### Core Technologies
- **[Haskell](https://www.haskell.org/)**: Functional programming language for robust data processing
- **[Hakyll](https://jaspervdj.be/hakyll/)**: Static site generator that compiles to fast, lightweight HTML
- **[GitHub Actions](https://github.com/features/actions)**: Automated weekly builds and deployment

### Data Pipeline
- **JSON Data Source**: Seattle AA's cached meeting feed (`tsml-cache-*.json`)
- **Custom CORS Proxy**: [Enables client-side data fetching](https://hyperpolarizability.com/posts/cors-proxy.html)
- **Data Processing**: Haskell functions parse, validate, and format meeting information
- **Static Generation**: Hakyll compiles everything into optimized HTML/CSS

### Infrastructure
- **GitHub Pages**: Free, reliable hosting for the static site
- **Weekly Updates**: Automatic rebuilds ensure data freshness
- **Version Control**: All code and data processing is tracked and versioned

## How It Works

1. **Data Fetching**: During build, the system fetches the latest meeting data from Seattle AA's JSON endpoint via our CORS proxy
2. **Data Processing**: Haskell functions parse the JSON, handle missing fields, and group meetings by day
3. **HTML Generation**: Hakyll templates generate static HTML with embedded meeting information
4. **Time Formatting**: Meeting times are converted from 24-hour to 12-hour format for readability  
5. **Deployment**: GitHub Actions automatically deploys the updated site to GitHub Pages

## For Other Regions

This is an **open-source solution** that can be replicated for any region with accessible meeting data.

### Requirements
- A publicly accessible JSON feed of meeting data (or willingness to create one)
- Basic technical knowledge for setup and maintenance
- GitHub account for hosting and automation

### Getting Started
If you're interested in creating a similar directory for your area:

1. **Contact Us**: Reach out via our [Contact page](/contact.html) for guidance
2. **Check Data Availability**: Ensure your region has or can create a machine-readable meeting feed
3. **Technical Setup**: We can provide documentation and support for the technical implementation

### Contributing to Seattle's Directory
If you have meeting updates or corrections for the Seattle area, the data ultimately comes from the official Seattle AA database. Contact the Seattle AA web and tech chair through the information on our [Contact page](/contact.html).

## Open Source

This project demonstrates how technical solutions can make recovery resources more accessible. The code, documentation, and methodology are available for any region that wants to implement a similar system.

We believe that removing technical barriers to finding AA meetings can help more people access the support they need in their recovery journey.

---

*Last updated automatically with each site build. All meeting data sourced from the official Greater Seattle Intergroup database.*