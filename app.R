# 1. load packages ----
library(shiny)        # app framework
library(bslib)        # layout + theme (page_sidebar, accordion, card, navset_tab)
library(rsconnect)    # deploy to shinyapps.io
library(leaflet)      # the map
library(leaflet.extras2) # addEasyprint() = save map view button
library(sf)           # loaded, unused
library(colorspace)   # loaded, unused
library(Polychrome)   # makes the distinct colors
library(mapview)      # loaded, unused
library(dplyr)        # filter/mutate/group_by etc
library(shinyWidgets) # pickerInput (checkbox dropdown + search + select all)
library(DT)           # data table tab

# 2. load & clean data ----
# turn the raw csv into what the app needs: real dates, one ID per bird, clean colony/age labels
caracara_data <- read.csv("Caracara_Data_combined.csv")  # read.csv() = load csv into a data frame
# combined = original Caracara_Data.CSV + feeding station data (see reformat_NWI_rings.R)

caracara_data$Date <- as.Date(caracara_data$Date, format = "%d/%m/%Y")
# as.Date(x, format) = text -> real date, format is how the text is laid out (day/month/year)
# wrong format = silent NAs, no error

unique_species <- sort(unique(caracara_data$Species))
# unique() = distinct values; sort() = alphabetical
# -> choices for the species filter

# jitter so points at the exact same spot don't hide each other
# set.seed() = same random offsets every run, so points don't jump around on redraw
# done once up here (before filtering) so a point's offset sticks no matter the filter
# display only, real Latitude/Longitude untouched
set.seed(42)
caracara_data$Latitude_display <- jitter(caracara_data$Latitude, amount = 0.00005)
caracara_data$Longitude_display <- jitter(caracara_data$Longitude, amount = 0.00005)
# jitter(x, amount) = add a little random noise to every value

## build combined metal + darvic id ----
# 1 Darvic = 1 individual. if any record of a Darvic has a real Metal,
# copy that Metal onto all the other records of that Darvic
# (resightings from far away usually only read the Darvic)
metal_lookup <- caracara_data %>%
  filter(!is.na(Darvic), !Metal %in% c("", "X", "None", "?")) %>%
  # filter() = keep matching rows; is.na() = TRUE if missing; %in% = is the value in this set
  # "X"/"None"/"?"/blank all mean no real Metal was read
  distinct(Darvic, Metal)
  # distinct() = drop duplicate Darvic + Metal pairs

# check if a Darvic ever links to 2+ different Metals (hasn't happened yet)
metal_conflicts <- metal_lookup %>%
  group_by(Darvic) %>%          # group_by() = do the next step separately for each Darvic
  filter(n() > 1)                # n() = number of rows in the group
if (nrow(metal_conflicts) > 0) {  # nrow() = number of rows
  warning("Darvic codes linked to more than one Metal number: ",
          paste(unique(metal_conflicts$Darvic), collapse = ", "))
          # paste(..., collapse = ", ") = glue a vector into one string
}

metal_lookup <- metal_lookup %>%
  group_by(Darvic) %>%
  summarize(known_metal = first(Metal), .groups = "drop")
  # summarize() = one row per group; first() = first value in the group
  # (only matters if there was a conflict)

caracara_data <- caracara_data %>%
  left_join(metal_lookup, by = "Darvic") %>%
  # left_join() = stick known_metal onto every row with the same Darvic
  mutate(
    # mutate() = add/change columns
    Metal_filled = case_when(
      # case_when() = if / else if / else for a whole column, first match wins
      !Metal %in% c("", "X", "None", "?") ~ Metal,   # already has a real Metal
      !is.na(known_metal) ~ known_metal,             # else borrow it from another sighting
      TRUE ~ NA_character_                            # else don't know
    ),
    individual_id = paste0(
      # paste0() = glue strings, no separator
      ifelse(is.na(Metal_filled), "NA", Metal_filled), " - ",
      ifelse(is.na(Darvic), "NA", Darvic)
      # ifelse(test, yes, no) = if/else for a whole column
    )
  )
# individual_id = how the app identifies a bird (dropdown, color, tracks)
# never use raw Darvic or Metal alone

unique_individuals <- sort(unique(caracara_data$individual_id))

## colony & age cleanup ----
# blank -> "(Unknown)" so those rows can still be picked in the filters
caracara_data$Colony[caracara_data$Colony == ""] <- "(Unknown)"
caracara_data$Age[caracara_data$Age == ""] <- "(Unknown)"

unique_colony <- sort(unique(caracara_data$Colony))
unique_age <- sort(unique(caracara_data$Age))

date_min <- min(caracara_data$Date, na.rm = TRUE)  # na.rm = TRUE: ignore NAs
date_max <- max(caracara_data$Date, na.rm = TRUE)  # -> slider start/end

## about tab text ----
# plain HTML, nothing reactive. r"---( ... )---" = raw string, everything inside is
# taken literally so no need to escape the quotes
# styled by .about-content in app_css below
about_html <- HTML(r"---(
<div class="about-content">

  <p class="about-lead">This Shiny application supports the exploration of
  Striated caracara ring-resighting records from New Island, Falkland Islands,
  at both the individual and colony level. It is designed to clearly present
  individual movement between sightings, first/last-seen dates, and sighting
  effort across colonies, ages, and time.</p>

  <section>
    <h5>Individual ID definition</h5>
    <p>Each individual is identified by a combined Metal + Darvic ID, since
    either tag alone can be incomplete on a given sighting. The format is
    <em>Metal number - Darvic code</em>:</p>
    <dl class="id-list">
      <dt><span class="id-chip">313 - Green D19</span></dt>
      <dd>Both the Metal number and the Darvic code are known.</dd>
      <dt><span class="id-chip">NA - Green P15</span></dt>
      <dd>No Metal number has ever been recorded for that Darvic.</dd>
      <dt><span class="id-chip">302 - NA</span></dt>
      <dd>No Darvic has been read.</dd>
    </dl>
    <p>One Darvic code is treated as one individual: if a Metal number is ever
    recorded for a given Darvic (typically at ringing), that Metal is carried
    over to every other sighting of that same Darvic, including resightings
    where only the Darvic was read from a distance.</p>
  </section>

  <section>
    <h5>Map display notes</h5>
    <p>Points recorded at the exact same location (common in feeding-station
    watches, where several birds are seen together) are offset slightly on the
    map so they don't overlap and hide each other. This offset is for display
    only &mdash; underlying coordinates are unaffected. Individuals are
    distinguished by marker color; all species share the same marker shape.</p>
  </section>

  <section>
    <h5>Authors and contributors</h5>
    <p>Feiyang Liu, Jeff White, Amandine Gamble, and collaborators.</p>
    <p>Code and visualization concepts adapted from Braxton Sizemore (skua
    tracking app) and Xibiao Wang (ELISA prevalence app).</p>
  </section>

  <section>
    <h5>Contact</h5>
    <p><a href="mailto:xl857@cornell.edu">xl857@cornell.edu</a></p>
  </section>

  <section>
    <h5>Code</h5>
    <p>Gamble Lab Shiny App &mdash; Caracara Tracker:<br>
    <a href="https://github.coecis.cornell.edu/xl857/Gamble_Lab_Rotation"
       target="_blank" rel="noopener">https://github.coecis.cornell.edu/xl857/Gamble_Lab_Rotation</a></p>
  </section>

</div>
)---")

# 3. UI ----
# layout copied from Xibiao's app: title bar w/ logos, sidebar of filters
# (accordion sections), tabs (Map / Data / About) in white cards
# bslib's page_sidebar() + accordion() + navset_tab() instead of fluidPage()/sidebarLayout()
# nothing reactive here, that's all in the server

# logos: put the PNGs in www/ next to app.R with these exact names
# if the file isn't there it shows a dashed text box instead
header_logo <- function(file, placeholder_text) {
  if (file.exists(file.path("www", file))) {
    # file.exists() = is the file really on disk
    tags$img(src = file, class = "header-logo", alt = placeholder_text)
  } else {
    span(class = "header-logo-placeholder", placeholder_text)
  }
}

# colors sampled from a screenshot of Xibiao's app
app_theme <- bs_theme(
  version = 5,          # bootstrap 5, bslib's new layouts need it
  primary = "#21827a",  # teal: active tab, outlined buttons, slider
  base_font = font_collection("Inter", "system-ui", "-apple-system", "Segoe UI",
                              "Roboto", "Helvetica Neue", "Arial", "sans-serif")
  # font_collection() = try fonts in order, first one installed wins
)

app_css <- "
  body { background-color: #f4f8fb; color: #162a3f; }

  /* title bar */
  .bslib-page-title { background-color: #f4f8fb; border-bottom: 1px solid #dfe5e9; padding: 10px 24px; }
  .app-header { display: flex; justify-content: space-between; align-items: center; width: 100%; }
  .app-title { font-size: 1.35rem; font-weight: 700; color: #162a3f; }
  .app-logos { display: flex; align-items: center; gap: 20px; }
  .header-logo { height: 44px; }
  .header-logo-placeholder {
    font-size: 13px; font-weight: 600; color: #666;
    border: 1px dashed #bbb; border-radius: 6px; padding: 5px 10px;
  }

  /* sidebar: everything inside shares one 20px left/right inset, so labels,
     dropdowns, section titles and the reset button all line up */
  .bslib-sidebar-layout > .sidebar { border-right: 1px solid #dfe5e9; }
  .sidebar-title { font-size: 1.1rem; font-weight: 500; color: #384a54; margin-bottom: 2px; }
  .sidebar label { color: #3c4852; font-weight: 400; font-size: .9rem; margin-bottom: 4px; }
  .sidebar .shiny-input-container { margin-bottom: 12px; }
  .sidebar .bootstrap-select > .dropdown-toggle {
    background-color: #dee4e9; border: 0; border-radius: 8px;
    padding: .5rem .85rem; color: #23333e; font-size: .92rem;
  }
  .sidebar #reset_filters { border-radius: 8px; font-weight: 500; padding: .5rem 1rem; margin-bottom: 6px; }
  .sidebar .irs { margin: 0 10px; }  /* keeps the slider's end labels from being clipped */

  /* accordion sections in the sidebar (full-width dividers, 20px inner inset) */
  .accordion {
    --bs-accordion-bg: transparent; --bs-accordion-border-width: 0;
    --bs-accordion-btn-bg: transparent; --bs-accordion-active-bg: transparent;
    --bs-accordion-btn-color: #314852; --bs-accordion-active-color: #314852;
    --bs-accordion-btn-focus-box-shadow: none; --bs-accordion-border-radius: 0;
  }
  .accordion-item { border-top: 1px solid #dfe5e9; border-radius: 0; }
  /* 24px, not 20px: bslib makes the sidebar accordion bleed 4px past each edge, so 24 lands on the same 20px line as the reset button */
  .accordion-button { font-weight: 600; font-size: .95rem; padding: 12px 24px; box-shadow: none; }
  .accordion-body { padding: 2px 24px 6px 24px; }

  /* main area + tabs */
  .bslib-sidebar-layout > .main { padding: 16px 20px 20px 20px; }
  .nav-tabs { border-bottom: 1px solid #dae0e4; gap: 4px; margin-bottom: 12px; }
  .nav-tabs .nav-link {
    color: #172d3f; font-weight: 500; font-size: .95rem; border: 0; border-top: 3px solid transparent;
    border-radius: 6px 6px 0 0; padding: .55rem 1.25rem;
  }
  .nav-tabs .nav-link:hover { color: #1f786c; }
  .nav-tabs .nav-link.active {
    color: #1f786c; font-weight: 600; background-color: #fff;
    border: 1px solid #dae0e4; border-top: 3px solid #21827a; border-bottom-color: #fff;
  }

  /* cards: 20px padding, no automatic gaps, spacing set explicitly below */
  .card {
    --bs-card-bg: #fff; border: 1px solid #e2e8ed; border-radius: 14px;
    box-shadow: 0 1px 3px rgba(22, 42, 63, .05);
  }
  .card-body { padding: 20px; gap: 0; }
  .card-heading { font-weight: 700; font-size: 1.25rem; color: #122a41; margin: 0 0 4px 0; }
  .card-note { color: #606f75; font-size: .92rem; margin: 0 0 16px 0; }

  /* map card: one controls row, one summary+legend row, then the map */
  .map-controls { display: flex; flex-wrap: wrap; align-items: center; gap: 8px 20px; margin: 0 0 12px 0; }
  .map-controls .shiny-input-container { width: auto; margin: 0; }
  .map-controls .form-check { margin: 0; min-height: 0; }
  .summary-strip {
    display: flex; flex-wrap: wrap; justify-content: space-between; align-items: center;
    gap: 6px 20px; background-color: #ecf5f6; border-left: 4px solid #21827a;
    border-radius: 6px; padding: 8px 14px; margin: 0 0 12px 0; color: #384951; font-size: .92rem;
  }
  .map-legend { display: flex; gap: 18px; color: #606f75; font-size: .88rem; }
  .legend-dot {
    display: inline-block; width: 12px; height: 12px; border-radius: 50%;
    background-color: #8a96a3; vertical-align: -2px; margin-right: 6px;
  }
  .legend-first { border: 2px solid #fff; box-shadow: 0 0 0 1px #b8c1c7; }
  .legend-last { border: 2px solid rgba(0, 0, 0, .5); }
  #falklands_map { border-radius: 10px; overflow: hidden; }

  /* about tab: one readable column, clear heading hierarchy, even spacing */
  .about-content { max-width: 700px; color: #384951; font-size: .95rem; line-height: 1.65; }
  .about-content p { margin: 0 0 12px 0; }
  .about-content .about-lead { font-size: 1rem; color: #2c3e4c; margin: 10px 0 6px 0; }
  .about-content section { border-top: 1px solid #e6ecef; margin-top: 18px; padding-top: 18px; }
  .about-content h5 { font-size: 1.05rem; font-weight: 600; color: #122a41; margin: 0 0 8px 0; }
  .about-content a { color: #1f786c; text-decoration: none; overflow-wrap: anywhere; }
  .about-content a:hover { text-decoration: underline; }
  .id-list { display: grid; grid-template-columns: max-content 1fr; gap: 8px 20px; align-items: center; margin: 4px 0 14px 0; }
  .id-list dt, .id-list dd { margin: 0; font-weight: 400; }
  .id-chip {
    display: inline-block; font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
    font-size: .86rem; background-color: #ecf5f6; color: #1f5f58;
    border: 1px solid #d3e6e6; border-radius: 6px; padding: 2px 10px;
  }

  /* data table: slightly tighter cells so all columns fit without sideways scrolling */
  table.dataTable th, table.dataTable td { padding: 5px 8px; font-size: .88rem; }
"

ui <- page_sidebar(
  # page_sidebar() = page with a collapsible sidebar + a main area
  title = div(class = "app-header",
              div(class = "app-title", "Caracara Tracker"),
              div(class = "app-logos",
                  header_logo("falkland_wildlife_health.png", "Falkland Wildlife Health"),
                  header_logo("cornell_logo.png", "Cornell University"))),
  window_title = "Caracara Tracker",   # browser tab name
  theme = app_theme,
  fillable = FALSE,   # normal scrolling page, not stretched to fit the window

  # sidebar: all the filters
  sidebar = sidebar(
    title = "Filter records",
    width = 320,
    bg = "#f4f8fb",
    padding = c(16, 20),   # top/bottom 16px, left/right 20px (same 20px the accordion uses)

    actionButton("reset_filters", "Reset all filters",
                 icon = icon("rotate-right"), class = "btn-outline-primary",
                 width = "100%"),

    accordion(
      # accordion() = collapsible sections; open = which ones start expanded
      id = "filter_sections",
      multiple = TRUE,   # can have several open at once
      open = c("Species & time", "Locations", "Individuals"),

      accordion_panel(
        "Species & time",
        pickerInput("species_select", "Species:",
                    # pickerInput() = dropdown with checkboxes (shinyWidgets)
                    choices = unique_species,
                    selected = unique_species,   # all selected by default
                    multiple = TRUE,
                    options = pickerOptions(
                      actionsBox = TRUE,                 # Select All / Deselect All buttons
                      selectedTextFormat = "count > 3",  # "N selected" after 3 picks
                      container = "body"                 # list floats over the page, not clipped by the sidebar
                    )),

        sliderInput(
          "date_filter",
          "Date range:",
          min = date_min,
          max = date_max,
          value = c(date_min, date_max),   # start with the full range
          dragRange = TRUE,                 # drag the whole range, not just each end
          timeFormat = "%d/%m/%Y"
        )
      ),

      accordion_panel(
        "Locations",
        pickerInput("colony_select", "Colony:",
                    choices = unique_colony,
                    selected = unique_colony,
                    multiple = TRUE,
                    options = pickerOptions(
                      actionsBox = TRUE,
                      liveSearch = TRUE,     # search box
                      selectedTextFormat = "count > 3",
                      container = "body"
                    ))
      ),

      accordion_panel(
        "Individuals",
        pickerInput("age_select", "Age:",
                    choices = unique_age,
                    selected = unique_age,
                    multiple = TRUE,
                    options = pickerOptions(
                      actionsBox = TRUE,
                      selectedTextFormat = "count > 3",
                      container = "body"
                    )),

        # used to have a separate single/multiple toggle here (2 inputs to keep in sync)
        # deleted it, picking 1 bird in a multi-select does the same thing
        pickerInput("individual_select", "Individual(s):",
                    choices = unique_individuals,
                    selected = unique_individuals,   # all selected by default
                    multiple = TRUE,
                    options = pickerOptions(
                      actionsBox = TRUE,
                      liveSearch = TRUE,
                      selectedTextFormat = "count > 3",
                      container = "body"
                    ))
      )
    )
  ),

  tags$head(tags$style(HTML(app_css))),   # puts the css string above into the page

  # main area: 3 tabs
  navset_tab(
    # navset_tab() = row of tabs; each nav_panel() is one tab
    id = "main_tabs",

    # map tab
    nav_panel(
      "Map",
      card(
        # card() = the white rounded box
        # fill = FALSE: card grows with its content instead of clipping it
        fill = FALSE,
        h4("Map of individual sightings", class = "card-heading"),
        p(class = "card-note",
          "Click a point for its ID and date. With tracks on, click a track to show only that individual."),

        div(class = "map-controls",
            checkboxInput(
              inputId = "show_tracks",
              label = "Show tracks (connect points)",
              value = FALSE
            ),
            # hides ALL dots: normal dots + the white/black start/end ones
            checkboxInput(
              inputId = "show_dots",
              label = "Show dots",
              value = TRUE
            ),
            actionButton("save_map_view", "Save map view (PNG)",
                         icon = icon("camera"), class = "btn-outline-primary btn-sm")),

        uiOutput("map_summary"),   # summary text + legend, built in the server

        leafletOutput("falklands_map", height = "560px")
        # leafletOutput() = spot in the UI for the map
        # height has to be px: save map view reads it with parseInt(),
        # "65vh" got exported as a 65px tall strip
      )
    ),

    # data tab
    nav_panel(
      "Data",
      card(
        fill = FALSE,
        h4("Filtered sighting records", class = "card-heading"),
        p(class = "card-note", "Shows exactly what's currently on the map."),
        div(class = "mb-3",   # mb-3 = bottom margin so the table doesn't touch the button
            downloadButton("download_data", "Download Filtered Data (CSV)",
                           class = "btn-outline-primary btn-sm")),
        DT::dataTableOutput("data_table", fill = FALSE)
        # fill = FALSE: inside a card the table was stuck in a 400px box
        # and cut off everything below, this lets it be its natural height
      )
    ),

    # about tab
    nav_panel(
      "About",
      card(
        fill = FALSE,
        h4("About this application", class = "card-heading"),
        about_html
      )
    )
  )
)

# 4. server setup (runs once, shared by every user) ----
# stuff that's the same for everyone and doesn't depend on inputs

## colors & marker icons ----
# one color per individual, same circle shape for every species
# (different shapes get messy with lots of species)

# colors are built from the FULL individual list (not the filtered one)
# so a bird's color never changes when you filter
set.seed(123)  # fixed seed = same colors every restart
my.colors <- createPalette(length(unique_individuals), c("#000000", "#FFFFFF"), M = 5000)
# createPalette(n, ...) = make n colors that look different from each other
random_colors <- sample(my.colors, length(unique_individuals))
# sample(x, n) = randomly shuffle/pick n items from x
individuals_pal <- colorFactor(palette = random_colors, domain = unique_individuals)
# colorFactor(palette, domain) = gives back a FUNCTION. individuals_pal("313 - Green D19") = that bird's color
# domain has to match what you call it with, otherwise NA

# makes one marker image: a little SVG circle turned into a "data URI" (image stored as text)
# divIcon() doesn't exist in R leaflet, and makeIcon() only takes an image URL
# so the circle gets encoded as a data: URI and used as the "image"
#
# dot_size = colored circle, stays the same whatever the border_width
# border = bigger circle drawn BEHIND the dot, not an SVG stroke
# (stroke eats into the dot as it gets thicker, that was the earlier bug)
make_dot_icon_uri <- function(fill_color, dot_size = 20,
                              border_color = NA, border_width = 0,
                              border_opacity = 1) {
  total_size <- dot_size + 2 * border_width
  center <- total_size / 2

  border_svg <- if (!is.na(border_color) && border_width > 0) {
    sprintf('<circle cx="%s" cy="%s" r="%s" fill="%s" fill-opacity="%s"/>',
            center, center, total_size / 2, border_color, border_opacity)
            # sprintf(fmt, ...) = fill the %s / %d slots in a string
  } else {
    ""
  }

  dot_svg <- sprintf('<circle cx="%s" cy="%s" r="%s" fill="%s"/>',
                     center, center, dot_size / 2, fill_color)

  svg <- sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d">%s%s</svg>',
                 total_size, total_size, border_svg, dot_svg)
  paste0("data:image/svg+xml;base64,", jsonlite::base64_enc(charToRaw(svg)))
  # charToRaw(svg) = svg text as raw bytes; base64_enc() = bytes -> plain text so it fits in a URL
  # (used to percent-encode it, but save map view choked on those, base64 works)
}

# makes ONE icons() object for every row of df, for addMarkers(icon = ...)
#
# has to be icons() (one iconUrl per row), NOT a list of makeIcon()s
# tried the list first: no error, addMarkers() just used the default blue pin
# only caught it by checking the marker HTML in the browser
build_icons <- function(df, dot_size = 20, border_color = NA, border_width = 0,
                         border_opacity = 1) {
  total_size <- dot_size + 2 * border_width
  icon_uris <- vapply(seq_len(nrow(df)), function(i) {
    # vapply(x, f, character(1)) = like sapply() but forces 1 string per element,
    # errors if f returns anything else
    make_dot_icon_uri(individuals_pal(df$individual_id[i]),
                      dot_size = dot_size, border_color = border_color,
                      border_width = border_width, border_opacity = border_opacity)
  }, character(1))

  icons(
    # icons() = leaflet's icon set, one entry per row
    iconUrl = icon_uris,
    iconWidth = total_size, iconHeight = total_size,
    iconAnchorX = total_size / 2, iconAnchorY = total_size / 2
    # anchor = half the size both ways puts the icon centered on the point,
    # default is bottom-center like a map pin
  )
}

# 5. server logic (runs once per browser tab) ----
server <- function(input, output, session) {

  ## filtered data ----
  # the one place that decides which rows match the filters
  # map, table, and download all read from this so they always show the same thing
  filtered_data <- reactive({
    # reactive({...}) = cached value, only recomputes when an input it uses (input$...) changes
    selected_ids <- input$individual_select

    caracara_data %>%
      filter(individual_id %in% selected_ids,
             Species %in% input$species_select,
             Colony %in% input$colony_select,
             Age %in% input$age_select,
             Date >= input$date_filter[1],
             Date <= input$date_filter[2]) %>%
      arrange(Date)   # arrange() = sort rows. needs to be by date for tracks + first/last
  })
  # no need to check for empty selection: %in% an empty set is FALSE for every row,
  # so filter() just gives 0 rows

  ## data table & download ----
  output$data_table <- DT::renderDataTable({
    # renderDataTable({...}) = builds the table for dataTableOutput("data_table")
    filtered_data() %>%
      select(Date, Time, individual_id, Species, Colony, Age, Latitude, Longitude) %>%
      # select() = keep only these columns
      rename(`Individual ID` = individual_id) %>%
      # rename(new = old) = cleaner header
      mutate(Latitude = round(Latitude, 5), Longitude = round(Longitude, 5))
      # round(x, 5) = 5 decimals (~1 m) so the table fits
      # display only, the csv download keeps full precision
  }, rownames = FALSE,   # no 1,2,3... first column
  options = list(pageLength = 25,
                    scrollX = TRUE),   # wide table scrolls sideways in the card instead of being cut off
  class = "compact stripe hover nowrap")  # compact = tighter rows; nowrap = IDs stay on one line

  output$download_data <- downloadHandler(
    # downloadHandler() = makes the download button work
    # filename and content are functions so they run fresh each click
    filename = function() paste0("caracara_filtered_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(rename(filtered_data(), `Individual ID` = individual_id),
                file, row.names = FALSE)
      # write.csv(df, file) = save data frame as csv (same "Individual ID" header as the table)
    }
  )

  ## individual dropdown narrowing ----
  # only list birds that have at least 1 sighting under the current species/colony/age/date filters
  # colors don't change (palette uses the full list), only the dropdown choices do
  # if everything was selected before, everything stays selected after
  # (otherwise narrowing then widening a filter leaves birds unselected)
  previous_valid_ids <- reactiveVal(NULL)  # reactiveVal() = one value you can read/update, NULL until first run
  observe({
    # observe({...}) = like reactive() but runs for what it DOES (updates another input), not a value
    valid_ids <- caracara_data %>%
      filter(Species %in% input$species_select,
             Colony %in% input$colony_select,
             Age %in% input$age_select,
             Date >= input$date_filter[1],
             Date <= input$date_filter[2]) %>%
      pull(individual_id) %>%   # pull() = one column -> plain vector
      unique() %>%
      sort()

    current <- isolate(input$individual_select)
    previous <- isolate(previous_valid_ids())
    # isolate(x) = read x without watching it
    # without it every click in the dropdown re-runs this observer,
    # which closed the dropdown after each click

    all_were_selected <- is.null(previous) || setequal(current, previous)
    # setequal(a, b) = same values in a and b? (order ignored)
    new_selected <- if (all_were_selected) valid_ids else intersect(current, valid_ids)
    # intersect(a, b) = values in both
    previous_valid_ids(valid_ids)

    updatePickerInput(session, "individual_select",
                       choices = valid_ids,
                       selected = new_selected)
                       # updatePickerInput() = change a pickerInput's choices/selection from the server
  })

  ## reset all filters ----
  # reset button -> everything selected, full date range
  observeEvent(input$reset_filters, {
    # observeEvent(x, {...}) = only runs when x fires (here: button click)
    updatePickerInput(session, "species_select", selected = unique_species)
    updatePickerInput(session, "colony_select", selected = unique_colony)
    updatePickerInput(session, "age_select", selected = unique_age)
    updateSliderInput(session, "date_filter", value = c(date_min, date_max),
                      timeFormat = "%d/%m/%Y")   # pass the format again or labels go back to yyyy-mm-dd
    updatePickerInput(session, "individual_select",
                      choices = unique_individuals, selected = unique_individuals)
    # update*Input() = change an input from the server
  })

  ## click a track -> only that individual ----
  observeEvent(input$falklands_map_shape_click, {
    # fires when a drawn line is clicked. each track's layerId = that bird's individual_id
    clicked_id <- input$falklands_map_shape_click$id
    req(clicked_id)   # req() = stop here if clicked_id is empty
    updatePickerInput(session, "individual_select", selected = clicked_id)
  })

  ## save map view ----
  observeEvent(input$save_map_view, {
    leafletProxy("falklands_map") %>%
      easyprintMap(sizeModes = "CurrentSize", filename = "caracara_map_view")
      # easyprintMap() = tells the addEasyprint() set up in the base map to export now
      # filename = name of the downloaded png
  })

  ## map summary strip ----
  # "how much is on the map" bar + legend, like Xibiao's
  output$map_summary <- renderUI({
    # renderUI({...}) = builds HTML that can change with the data
    d <- filtered_data()
    div(class = "summary-strip",
        span(paste0("Individuals displayed: ", n_distinct(d$individual_id),
                    " | Sightings: ", nrow(d))),
        # n_distinct(x) = number of unique values
        div(class = "map-legend",   # white/black border key, same row
            span(span(class = "legend-dot legend-first"), "First sighting (white border)"),
            span(span(class = "legend-dot legend-last"), "Most recent (black border)")))
  })

  ## base map ----
  # only the stuff that never changes (tiles, controls, starting view)
  # points and lines get added later by leafletProxy in the draw observer
  output$falklands_map <- renderLeaflet({
    # renderLeaflet({...}) = builds the map for leafletOutput("falklands_map")
    leaflet(options = leafletOptions(maxZoom = 17)) %>%
      # leaflet() = new map. maxZoom caps ALL zooming (manual + fitBounds),
      # satellite imagery runs out past this so you get blank tiles
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      # addProviderTiles() = add a basemap from a 3rd party
      addTiles(group = "Open Street Map") %>%
      # addTiles() = default OpenStreetMap
      addProviderTiles("OpenTopoMap", group = "Topography") %>%
      addLayersControl(
        # addLayersControl() = the button that switches between the group = "..." layers above
        baseGroups = c("Satellite", "Open Street Map", "Topography"),
        # names have to exactly match the group = above or clicking does nothing
        # (happened with "Topography" vs a layer still named "Dark")
        options = layersControlOptions(collapsed = TRUE),  # collapsed = just the icon, like Xibiao's
        position = "topright"   # zoom buttons stay top-left
      ) %>%
      addEasyprint(options = easyprintOptions(
        # addEasyprint() = sets up png export of the map (leaflet.extras2)
        # hidden = hide the plugin's own icon, my "Save map view" button above the map triggers it
        # exportOnly = download png straight away, no print dialog
        # "CurrentSize" = exactly what's on screen
        hidden = TRUE,
        exportOnly = TRUE,
        sizeModes = list("CurrentSize"),
        filename = "caracara_map_view"
      )) %>%
      setView(lng = -59.5236, lat = -51.7963, zoom = 8)
      # setView() = starting center + zoom
  })

  ## draw tracks, dots, start/end markers, zoom ----
  # runs again every time the filters or checkboxes change
  observe({
    data <- filtered_data()

    map_proxy <- leafletProxy("falklands_map") %>%
      # leafletProxy() = handle to the map that's ALREADY drawn,
      # so it updates in place instead of rebuilding (no flicker)
      clearMarkers() %>%
      clearShapes()
      # wipe old markers + lines before drawing the new ones
      # NOT clearControls(), it also deletes the save map view button
    # clear BEFORE the 0 rows check below, otherwise a filter with no matches
    # leaves the old drawing on screen

    if (nrow(data) == 0) return()

    # 1. tracks, drawn first so dots end up on top
    if (input$show_tracks) {
      for (bird in unique(data$individual_id)) {
        bird_data <- data %>% filter(individual_id == bird)
        map_proxy <- map_proxy %>%
          addPolylines(data = bird_data, lng = ~Longitude_display, lat = ~Latitude_display,
                       color = individuals_pal(bird), weight = 7, opacity = 0.8,
                       layerId = bird, label = bird)
                       # addPolylines() = line through the points in row order
                       # (already date sorted from filtered_data(), so it follows time)
                       # layerId = bird is what makes the track clickable,
                       # the click observer reads it back as input$falklands_map_shape_click$id
                       # label = bird shows the ID on hover
      }
    }

    # 2 + 3. dots and start/end markers
    # "Show dots" turns off everything inside this if, both together
    if (input$show_dots) {

      # 2. every point, colored by individual
      map_proxy <- map_proxy %>%
        addMarkers(
          # addMarkers() = put an icon at each row's lng/lat
          data = data,
          lng = ~Longitude_display, lat = ~Latitude_display,
          icon = build_icons(data, dot_size = 20),
          popup = ~paste("<b>ID:</b>", individual_id, "<br>",
                         "<b>Date:</b>", Date)
                         # popup = text when you click a marker
        )

      # 3. start/end markers, drawn AFTER the dots so the borders stay on top
      for (bird in unique(data$individual_id)) {
        bird_data <- data %>% filter(individual_id == bird)
        start_point <- bird_data[1, ]              # first row = earliest sighting
        end_point <- bird_data[nrow(bird_data), ]  # last row = most recent

        map_proxy <- map_proxy %>%
          addMarkers(data = start_point, lng = ~Longitude_display, lat = ~Latitude_display,
                     icon = build_icons(start_point, dot_size = 20, border_color = "white", border_width = 5, border_opacity = 0.5),
                     popup = ~paste("<b>First sighting</b><br>",
                                    "<b>ID:</b>", individual_id, "<br>",
                                    "<b>Date:</b>", Date)) %>%
          addMarkers(data = end_point, lng = ~Longitude_display, lat = ~Latitude_display,
                     icon = build_icons(end_point, dot_size = 20, border_color = "black", border_width = 5, border_opacity = 0.5),
                     popup = ~paste("<b>Most recent sighting</b><br>",
                                    "<b>ID:</b>", individual_id, "<br>",
                                    "<b>Date:</b>", Date))
      }
    }

    # 4. zoom to fit, last
    # (legend used to be here too, removed, clicking a popup tells you which bird now)
    map_proxy %>%
      fitBounds(
        # fitBounds() = zoom/pan so everything fits in this box
        lng1 = min(data$Longitude, na.rm = TRUE),
        lat1 = min(data$Latitude, na.rm = TRUE),
        lng2 = max(data$Longitude, na.rm = TRUE),
        lat2 = max(data$Latitude, na.rm = TRUE),
        options = list(maxZoom = 17)
        # different from the map's maxZoom above, this only caps THIS zoom
        # matters for a single point (min == max), otherwise it zooms in as far as possible
      )
  })
}

# 6. run app ----
shinyApp(ui = ui, server = server)
# shinyApp(ui, server) = puts UI + server together into one app
