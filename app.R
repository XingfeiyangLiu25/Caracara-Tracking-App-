# 1. Load Packages ----
library(shiny)        # app framework
library(bslib)        # loaded, unused
library(rsconnect)    # deploy to shinyapps.io
library(leaflet)      # the map
library(sf)           # loaded, unused
library(colorspace)   # loaded, unused
library(Polychrome)   # generates the distinct-color palette
library(mapview)      # loaded, unused
library(dplyr)        # filter/mutate/group_by etc
library(shinyWidgets) # pickerInput (checkbox dropdown + search + select all)
library(DT)           # data table tab

# 2. Load & Clean Data ----
caracara_data <- read.csv("Caracara_Data_combined.csv")  # read.csv() = load a csv into a data frame
# combined = original Caracara_Data.CSV + reformatted feeding-station data (see reformat_NWI_rings.R)

caracara_data$Date <- as.Date(caracara_data$Date, format = "%d/%m/%Y")
# as.Date(x, format) turns a text date into a real Date object using the given
# layout (day/month/year here). Wrong format = silent NAs, not an error.

unique_species <- sort(unique(caracara_data$Species))
# unique() = distinct values in a vector; sort() = alphabetical order
# -> becomes the choices list for the Species filter

# Fixed jitter for overlapping points. set.seed() locks the random number
# generator so jitter() gives the SAME offsets every run, instead of new
# random ones every redraw (which would make points visibly jump around).
# Done once here, before filtering, so an offset sticks to a point no matter
# how the data gets filtered later. Only affects drawing, reallatitude/Longitude untouched.
set.seed(42)
caracara_data$Latitude_display <- jitter(caracara_data$Latitude, amount = 0.00005)
caracara_data$Longitude_display <- jitter(caracara_data$Longitude, amount = 0.00005)
# jitter(x, amount) = add small random noise to every value in x

## Build Combined Metal + Darvic ID. 1 Darvic = 1 individual. If any record of a Darvic has a
# real Metal number, backfill that Metal onto every other record of that
# same Darvic (resightings usually only read the Darvic from a distance).
metal_lookup <- caracara_data %>%
  filter(!is.na(Darvic), !Metal %in% c("", "X", "None", "?")) %>%
  # filter() = keep matching rows; is.na() = TRUE if missing; %in% = value found in this set
  # "X"/"None"/"?"/blank all mean "no real Metal read here," not an actual value
  distinct(Darvic, Metal)
  # distinct() = drop duplicate Darvic+Metal row pairs

# Conflict check to see if a Darvic linked to 2+ different Metals. 
# Hasn't happened yet.
metal_conflicts <- metal_lookup %>%
  group_by(Darvic) %>%          # group_by() = do the next step separately per Darvic
  filter(n() > 1)                # n() = row count in the current group
if (nrow(metal_conflicts) > 0) {  # nrow() = number of rows
  warning("Darvic codes linked to more than one Metal number: ",
          paste(unique(metal_conflicts$Darvic), collapse = ", "))
          # paste(..., collapse=",") = glue a vector into one string
}

metal_lookup <- metal_lookup %>%
  group_by(Darvic) %>%
  summarize(known_metal = first(Metal), .groups = "drop")
  # summarize() = collapse each group to one row; first() = first value in the group
  # (only matters if there was a conflict above)

caracara_data <- caracara_data %>%
  left_join(metal_lookup, by = "Darvic") %>%
  # left_join() = attach known_metal onto every row sharing that Darvic
  mutate(
    # mutate() = add/change columns
    Metal_filled = case_when(
      # case_when() = vectorized if/elseif/else, first matching condition wins
      !Metal %in% c("", "X", "None", "?") ~ Metal,   # already has a real Metal
      !is.na(known_metal) ~ known_metal,             # else pull it from another sighting
      TRUE ~ NA_character_                            # else genuinely unknown
    ),
    individual_id = paste0(
      # paste0() = glue strings with no separator
      ifelse(is.na(Metal_filled), "NA", Metal_filled), " - ",
      ifelse(is.na(Darvic), "NA", Darvic)
      # ifelse(test, yes, no) = base R vectorized if/else
    )
  )
# individual_id identifies a bird everywhere in this app (dropdown, color, tracks)
# never group/color by raw Darvic or Metal alone

unique_individuals <- sort(unique(caracara_data$individual_id))

## Colony & Age Cleanup ----
# blank -> "(Unknown)" so those rows stay selectable instead of vanishing
caracara_data$Colony[caracara_data$Colony == ""] <- "(Unknown)"
caracara_data$Age[caracara_data$Age == ""] <- "(Unknown)"

unique_colony <- sort(unique(caracara_data$Colony))
unique_age <- sort(unique(caracara_data$Age))

date_min <- min(caracara_data$Date, na.rm = TRUE)  # na.rm = TRUE: ignore NAs when computing min/max
date_max <- max(caracara_data$Date, na.rm = TRUE)

## About Tab Content ----
# static HTML, nothing reactive
about_html <- HTML("
  <p>This Shiny application supports the exploration of Striated caracara
  ring-resighting records from New Island, Falkland Islands, at both the
  individual and colony level. It is designed to clearly present individual
  movement between sightings, first/last-seen dates, and sighting effort
  across colonies, ages, and time.</p>

  <h4>Individual ID definition</h4>
  <p>Each individual is identified by a combined Metal + Darvic ID, since
  either tag alone can be incomplete on a given sighting:</p>
  <ul>
    <li>Format: <i>Metal number - Darvic code</i> (e.g., \"313 - Green D19\")</li>
    <li>If no Metal number has ever been recorded for that Darvic: <i>NA - Darvic code</i> (e.g., \"NA - Green P15\")</li>
    <li>If no Darvic has been read: <i>Metal number - NA</i> (e.g., \"302 - NA\")</li>
  </ul>
  <p>One Darvic code is treated as one individual: if a Metal number is ever
  recorded for a given Darvic (typically at ringing), that Metal is carried
  over to every other sighting of that same Darvic, including resightings
  where only the Darvic was read from a distance.</p>

  <h4>Map display notes</h4>
  <p>Points recorded at the exact same location (common in feeding-station
  watches, where several birds are seen together) are offset slightly on the
  map so they don't overlap and hide each other. This offset is for display
  only -- underlying coordinates are unaffected. Species are distinguished by
  marker shape; individuals are distinguished by marker color.</p>

  <h4>Authors and contributors</h4>
  <p>Feiyang Liu, Amandine Gamble, and collaborators.<br>
  Code and visualization concepts adapted from: Braxton Sizemore (skua
  tracking app) and Xibiao Wang (ELISA prevalence app). Ring identification
  guidance: Jeff White.</p>

  <h4>Contact</h4>
  <p>xl857@cornell.edu</p>

  <h4>Code</h4>
  <p>Gamble Lab Shiny App -- Caracara Tracker: https://github.coecis.cornell.edu/xl857/Gamble_Lab_Rotation
")

# 3. Define the User Interface (UI) ----
# Top title band + fixed sidebar (filters) + tabbed main content (Map/Data Table/About)
ui <- fluidPage(
  title = "Caracara Tracker",
  style = "padding: 0;",

  tags$head(
    tags$style(HTML("
      .app-header {
        background-color: #2c3e50;
        color: white;
        padding: 18px 25px;
        font-size: 26px;
        font-weight: bold;
      }
      .transparent-marker {
        background: transparent;
        border: none;
      }
    "))
  ),
  # .transparent-marker: unused now (markers are SVG images, not div icons)

  # placeholder title band, Amandine will send a logo/program name later
  div(class = "app-header", "Caracara Tracker"),

  sidebarLayout(
    # sidebarLayout() = a fixed side column + main column, side by side
    sidebarPanel(
      width = 3,   # out of 12 grid units total

      pickerInput("species_select", "Filter by Species:",
                  choices = unique_species,
                  selected = unique_species,
                  multiple = TRUE,
                  options = pickerOptions(
                    actionsBox = TRUE,                 # adds Select All / Deselect All buttons
                    selectedTextFormat = "count > 3"   # shows "N selected" past 3 picks
                  )),

      pickerInput("colony_select", "Filter by Colony:",
                  choices = unique_colony,
                  selected = unique_colony,
                  multiple = TRUE,
                  options = pickerOptions(
                    actionsBox = TRUE,
                    liveSearch = TRUE,     # adds a search box (list is long)
                    selectedTextFormat = "count > 3"
                  )),

      pickerInput("age_select", "Filter by Age:",
                  choices = unique_age,
                  selected = unique_age,
                  multiple = TRUE,
                  options = pickerOptions(
                    actionsBox = TRUE,
                    selectedTextFormat = "count > 3"
                  )),

      sliderInput(
        "date_filter",
        "Select Date Range:",
        min = date_min,
        max = date_max,
        value = c(date_min, date_max),   # starting range = full span
        dragRange = TRUE,                 # lets you drag the whole range, not just each end
        timeFormat = "%d/%m/%Y"
      ),

      # Used to have a separate Single/Multiple toggle here (2 inputs to keep in sync) 
      # deleted it once I realized picking one bird in a
      # multi-select works identically.
      pickerInput("individual_select", "Select Individual(s):",
                  choices = unique_individuals,
                  multiple = TRUE,
                  options = pickerOptions(
                    actionsBox = TRUE,
                    liveSearch = TRUE,
                    selectedTextFormat = "count > 3"
                  )),

      checkboxInput(
        inputId = "show_tracks",
        label = "Show tracks (connect points)",
        value = FALSE
      ),
      # only hides the plain per-sighting dots (layer 2 below), not the
      # white/black start/end markers. 
      checkboxInput(
        inputId = "show_dots",
        label = "Show dots",
        value = TRUE
      ),
      helpText(HTML("<b>White border:</b> first sighting<br><b>Black border:</b> most recent sighting"))
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        # tabsetPanel() = a set of clickable tabs; each tabPanel() is one tab's content
        tabPanel("Map",
                 leafletOutput("falklands_map", height = "80vh")),
                 # leafletOutput() = reserves a spot in the UI for the map; "80vh" = 80% of viewport height
        tabPanel("Data Table",
                 br(),
                 downloadButton("download_data", "Download Filtered Data (CSV)"),
                 br(), br(),
                 DT::dataTableOutput("data_table")),
        tabPanel("About", about_html)
      )
    )
  )
)

# 4. Server Setup (built ONCE, shared across every user session) ----

## Color Palette & Marker Icon Builders ----
# One distinct color per individual, built from the FULL individual list (not
# whatever's filtered) so a bird's color never changes when filters change.
set.seed(123)  # fixed seed = same colors every restart
my.colors <- createPalette(length(unique_individuals), c("#000000", "#FFFFFF"), M = 5000)
# createPalette(n, ...) = generate n visually distinct colors
random_colors <- sample(my.colors, length(unique_individuals))
# sample(x, n) = randomly pick/shuffle n items from x
individuals_pal <- colorFactor(palette = random_colors, domain = unique_individuals)
# colorFactor(palette, domain) = returns a FUNCTION; call individuals_pal("313 - Green D19")
# to get that bird's color. domain must match what you call it with, or you get NA.

# Species get a marker SHAPE; color still encodes individual. Add new
# species here as they show up (e.g. Skua), anything missing falls back
# to "circle" instead of erroring.
species_shapes <- c(
  "Striated caracara" = "circle",
  "Upland goose" = "square"
)

# Builds one marker's image as a self-contained SVG "data URI" string.
# divIcon() (HTML/CSS icons) doesn't exist in the R leaflet package;
# makeIcon() only takes an image URL -- so this builds a tiny SVG shape and
# encodes it as a data: URI to use as that "image."
#
# dot_size = the visible colored shape's size, fixed regardless of
# border_width. The border is a same-shaped layer drawn BEHIND the dot
# (bigger circle/square in border_color, dot on top), not an SVG stroke --
# a stroke eats into the shape as it thickens, which was the earlier bug.
make_species_icon_uri <- function(species, fill_color, dot_size = 20,
                                   border_color = NA, border_width = 0,
                                   border_opacity = 1) {
  shape <- species_shapes[species]
  if (is.na(shape)) shape <- "circle"

  total_size <- dot_size + 2 * border_width
  center <- total_size / 2

  border_svg <- if (!is.na(border_color) && border_width > 0) {
    if (shape == "square") {
      sprintf('<rect x="0" y="0" width="%s" height="%s" fill="%s" fill-opacity="%s"/>',
              total_size, total_size, border_color, border_opacity)
              # sprintf(fmt, ...) = fill %s/%d placeholders into a string
    } else {
      sprintf('<circle cx="%s" cy="%s" r="%s" fill="%s" fill-opacity="%s"/>',
              center, center, total_size / 2, border_color, border_opacity)
    }
  } else {
    ""
  }

  dot_svg <- if (shape == "square") {
    sprintf('<rect x="%s" y="%s" width="%s" height="%s" fill="%s"/>',
            border_width, border_width, dot_size, dot_size, fill_color)
  } else {
    sprintf('<circle cx="%s" cy="%s" r="%s" fill="%s"/>',
            center, center, dot_size / 2, fill_color)
  }

  svg <- sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d">%s%s</svg>',
                 total_size, total_size, border_svg, dot_svg)
  paste0("data:image/svg+xml;utf8,", utils::URLencode(svg, reserved = TRUE))
  # URLencode(x, reserved=TRUE) = percent-encode special characters so the
  # SVG text can live inside a URL
}

# Builds ONE icons() object covering every row of df, for addMarkers(icon=...).
#
# Must be icons() (vectors -- one iconUrl per row), NOT a plain list of
# individual makeIcon() objects. I tried the list version first: no error
# anywhere, addMarkers() just silently used Leaflet's default blue pin
# instead. Only found it by checking the rendered marker HTML.
build_icons <- function(df, dot_size = 20, border_color = NA, border_width = 0,
                         border_opacity = 1) {
  total_size <- dot_size + 2 * border_width
  icon_uris <- vapply(seq_len(nrow(df)), function(i) {
    # vapply(x, f, character(1)) = like sapply(), but forces one string
    # output per element and errors loudly if f ever returns something else
    make_species_icon_uri(df$Species[i], individuals_pal(df$individual_id[i]),
                           dot_size = dot_size, border_color = border_color,
                           border_width = border_width, border_opacity = border_opacity)
  }, character(1))

  icons(
    # icons(...) = leaflet's vectorized icon set, one entry per row
    iconUrl = icon_uris,
    iconWidth = total_size, iconHeight = total_size,
    iconAnchorX = total_size / 2, iconAnchorY = total_size / 2
    # anchor = total_size/2 both ways centers the icon on its point;
    # otherwise it defaults to bottom-center like a map pin
  )
}

# 5. Server Logic (runs once per browser session) ----
server <- function(input, output, session) {

  ## Reactive Filtered Data ----
  # everything else (map, table, download) reads from this one reactive, so
  # they're always guaranteed to show/export the same rows
  filtered_data <- reactive({
    # reactive({...}) = a cached value that recomputes only when an input
    # it reads (via input$...) actually changes
    selected_ids <- input$individual_select

    caracara_data %>%
      filter(individual_id %in% selected_ids,
             Species %in% input$species_select,
             Colony %in% input$colony_select,
             Age %in% input$age_select,
             Date >= input$date_filter[1],
             Date <= input$date_filter[2]) %>%
      arrange(Date)   # arrange() = sort rows (needed so tracks/first-last are in time order)
  })
  # no "nothing selected -> empty" check needed: %in% against an empty
  # selection is FALSE for every row, so filter() already returns 0 rows

  ## Data Table & Download ----
  output$data_table <- DT::renderDataTable({
    # renderDataTable({...}) = build the table shown by dataTableOutput("data_table")
    filtered_data() %>%
      select(Date, Time, individual_id, Species, Colony, Age, Latitude, Longitude)
      # select() = keep only these columns
  }, options = list(pageLength = 25))

  output$download_data <- downloadHandler(
    # downloadHandler() = wires up a downloadButton; filename/content are
    # both functions so they're computed fresh each time it's clicked
    filename = function() paste0("caracara_filtered_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
      # write.csv(df, file) = save a data frame as csv at that path
    }
  )

  ## Individual Dropdown Narrowing ----
  # keep only birds with at least 1 sighting matching the current
  # Species/Colony/Age/Date filters. Colors stay fixed (individuals_pal
  # uses the FULL list up top) -- only the dropdown's choices change.
  observe({
    # observe({...}) = like reactive(), but runs for its side effects
    # (here, updating another input) instead of returning a value
    valid_ids <- caracara_data %>%
      filter(Species %in% input$species_select,
             Colony %in% input$colony_select,
             Age %in% input$age_select,
             Date >= input$date_filter[1],
             Date <= input$date_filter[2]) %>%
      pull(individual_id) %>%   # pull() = extract one column as a plain vector
      unique() %>%
      sort()

    updatePickerInput(session, "individual_select",
                       choices = valid_ids,
                       selected = intersect(isolate(input$individual_select), valid_ids))
                       # updatePickerInput() = change a pickerInput's choices/selection from the server
                       # intersect(a, b) = values found in both a and b
                       # isolate(x) = read x's current value WITHOUT subscribing to its changes --
                       # without this, every click in the dropdown re-triggers this same observer
                       # (since reading input$individual_select normally creates that dependency),
                       # which was closing the dropdown after every single click
  })

  ## Base Map ----
  # built once; never touched again directly -- all data drawing happens
  # via leafletProxy() below instead of rebuilding this from scratch
  output$falklands_map <- renderLeaflet({
    # renderLeaflet({...}) = build the map shown by leafletOutput("falklands_map")
    leaflet(options = leafletOptions(maxZoom = 17)) %>%
      # leaflet() = create a new map widget; maxZoom caps ALL zooming
      # (manual or fitBounds) since satellite imagery doesn't exist past
      # this level here and shows blank tiles instead
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      # addProviderTiles() = add a 3rd-party basemap layer
      addTiles(group = "Open Street Map") %>%
      # addTiles() = add the default OpenStreetMap layer
      addProviderTiles("OpenTopoMap", group = "Topography") %>%
      addLayersControl(
        # addLayersControl() = UI control to switch between the group="..." layers above
        baseGroups = c("Satellite", "Open Street Map", "Topography"),
        # these names must exactly match the group= args above, or that
        # option does nothing when clicked (bit me once with "Topography"
        # vs a layer still named "Dark")
        options = layersControlOptions(collapsed = FALSE),
        position = "topleft"
      ) %>%
      setView(lng = -59.5236, lat = -51.7963, zoom = 8)
      # setView() = set the map's starting center + zoom level
  })

  ## Main Draw Observer (tracks, dots, start/end markers, zoom) ----
  # redraws everything whenever filtered_data() changes
  observe({
    data <- filtered_data()

    map_proxy <- leafletProxy("falklands_map") %>%
      # leafletProxy() = handle to the ALREADY-rendered map, so it can be
      # updated in place instead of rebuilt (fast, no flicker)
      clearMarkers() %>%
      clearShapes() %>%
      clearControls()
      # clear old markers/lines/legend before drawing the new set
    # clearing happens BEFORE the nrow(data)==0 check below on purpose --
    # otherwise a filter matching 0 rows would leave the OLD drawing on
    # screen instead of actually going blank

    if (nrow(data) == 0) return()

    # 1. Tracks -- drawn first so dots/markers layer on top
    if (input$show_tracks) {
      for (bird in unique(data$individual_id)) {
        bird_data <- data %>% filter(individual_id == bird)
        map_proxy <- map_proxy %>%
          addPolylines(data = bird_data, lng = ~Longitude_display, lat = ~Latitude_display,
                       color = individuals_pal(bird), weight = 7, opacity = 0.8)
                       # addPolylines() = draw connected line segments through these points, in row order
                       # (bird_data is already date-sorted from filtered_data(), so this traces time order)
      }
    }

    # 2. Main layer: every point, colored by individual, shaped by species.
    # This is what "Show dots" toggles.
    if (input$show_dots) {
      map_proxy <- map_proxy %>%
        addMarkers(
          # addMarkers() = place an icon at each row's lng/lat
          data = data,
          lng = ~Longitude_display, lat = ~Latitude_display,
          icon = build_icons(data, dot_size = 20),
          popup = ~paste("<b>ID:</b>", individual_id, "<br>",
                         "<b>Date:</b>", Date)
                         # popup = text shown when a marker is clicked
        )
    }

    # 3. Start/end markers, drawn AFTER dots so their borders stay on top
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

    # 4. fitBounds, last. Legend used to live here too, but Amandine had it
    # dropped -- a legend listing every selected bird doesn't fit once a
    # lot are picked, so clicking a popup answers "which bird" instead.
    map_proxy %>%
      fitBounds(
        # fitBounds() = zoom/pan the map to fit this box
        lng1 = min(data$Longitude, na.rm = TRUE),
        lat1 = min(data$Latitude, na.rm = TRUE),
        lng2 = max(data$Longitude, na.rm = TRUE),
        lat2 = max(data$Latitude, na.rm = TRUE),
        options = list(maxZoom = 17)
        # separate from the map's own maxZoom above -- this one only caps
        # THIS zoom action (matters for a single point, where min==max and
        # it would otherwise zoom in as far as physically possible)
      )
  })
}

# 6. Run the Application ----
shinyApp(ui = ui, server = server)
# shinyApp(ui, server) = bundles both halves into one runnable app
