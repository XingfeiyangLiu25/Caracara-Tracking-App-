# Reformat NWI Rings.xlsx into the same row-per-individual-sighting format as
# Caracara_Data.CSV, and combine the two into one file.
#
# Only the "Feeding Data" sheet is used -- "ROOK SURVEY"'s GPS Location codes
#   (S1-S20, B1-B19, etc.) are survey-transect points, not the 4 named
#   locations we have coordinates for, so those rows can't be geolocated.
# Feeding Data rows with no RINGS (75 of 129) are dropped -- no individual
#   to attach the row to, they're aggregate headcounts, not sightings.
# Each RINGS token is classified as a Darvic (letters+digits, e.g. "D19" ->
#   "Green D19") or a Metal number (digits only, e.g. "225"). Anything
#   matching neither is dropped with a warning, not guessed at.
# "Green" is assumed as the Darvic color, since every Darvic in
#   Caracara_Data.CSV is Green.
# Species assumed "Striated caracara", Island assumed "New Island" -- both
#   true everywhere in Caracara_Data.CSV, not recorded in this sheet.
# Colony comes from LOCATION ("Ferguson Point" corrected to "Fergusson Point")

# 1. Setup ----
# make sure the packages this script needs are actually installed
pkgs <- c("readxl", "dplyr", "tidyr", "stringr")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  # requireNamespace(p) = TRUE if p is installed, without loading it
}
library(readxl)  # read_excel() -- load .xlsx sheets
library(dplyr)   # filter/mutate/etc
library(tidyr)   # separate_rows()
library(stringr) # str_* string helpers

# record where the source spreadsheet lives.
xlsx_path <- "NWI Rings.xlsx"

# 2. Known GPS Coordinates ----
# spreadsheet only records location names, coordinates are from Amandine
location_coords <- tibble::tibble(
  # tibble() = build a small data frame column by column
  Colony    = c("Burnt Island", "North Hut", "North Saddle", "Fergusson Point"),
  Latitude  = c(-51.727408, -51.692591, -51.695856, -51.720408),
  Longitude = c(-61.293728, -61.255761, -61.269821, -61.290134)
)

# 3. Load & Reshape Feeding Data ----
# raw sheet has one row per feeding observation, with a whole
# group of birds seen at once crammed into a single cell. convert it into 1 row
# per signting. 

feeding_raw <- read_excel(xlsx_path, sheet = "Feeding Data")
# read_excel(path, sheet) = load one sheet of an xlsx file as a data frame

feeding_long <- feeding_raw %>%
  filter(!is.na(RINGS)) %>%          # drop rows with no ring codes at all
  separate_rows(RINGS, sep = ",") %>%
  # separate_rows(col, sep) = split a "A, B, C" cell into 3 separate rows,
  # copying the rest of that row's data onto each 
  mutate(RINGS = str_trim(RINGS)) %>%  # str_trim() = strip leading/trailing spaces
  filter(RINGS != "")                   # drop empty tokens left over from trailing commas

# 4. Classify Ring Codes ----
# Figures out whether the ring number is darvic or metal 
# letters+digits = Darvic, digits-only = Metal

is_metal  <- str_detect(feeding_long$RINGS, "^[0-9]+$")
# str_detect(x, pattern) = TRUE/FALSE, does x match this regex
# "^[0-9]+$" = the whole string is digits only, e.g. "225"
is_darvic <- str_detect(feeding_long$RINGS, "^[A-Za-z]+[0-9]+$")
# "^[A-Za-z]+[0-9]+$" = letters then digits, e.g. "D19"

unclassified <- unique(feeding_long$RINGS[!is_metal & !is_darvic])
if (length(unclassified) > 0) {
  warning("Ring token(s) matching neither a Darvic nor a Metal pattern -- ",
          "dropped, needs manual review: ", paste(unclassified, collapse = ", "))
          # paste(..., collapse = ", ") = glue a vector into one string
}

# 5. Build Final Columns ----
# turn the classified ring into real Darvic/Metal columns
# attach real GPS coordinates from section 2's lookup table
# fill in the assumed fields (Species, Island) but that this sheet never recorded
# The column list at the end is in the exact same order as Caracara_Data.CSV,

feeding_long <- feeding_long %>%
  mutate(
    Darvic = ifelse(is_darvic, paste("Green", RINGS), NA_character_),
    Metal  = ifelse(is_metal, RINGS, NA_character_),
    keep   = is_darvic | is_metal
  ) %>%
  filter(keep) %>%   # drop the unclassified tokens flagged above
  mutate(
    Colony = str_to_title(LOCATION),  # str_to_title() = "FERGUSON POINT" -> "Ferguson Point"
    Colony = recode(Colony, "Ferguson Point" = "Fergusson Point")
    # recode(x, old = new) = replace specific values
  ) %>%
  left_join(location_coords, by = "Colony") %>%
  # left_join() = attach the matching Lat/Long row for each Colony
  transmute(
    # transmute() = like mutate(), but drops every column not listed here --
    # this is what actually builds the final output columns, matching
    # Caracara_Data.CSV's layout
    Date = format(as.Date(DATE), "%d/%m/%Y"),
    # format(date, fmt) = turn a Date back into text in the given layout
    # (matches the original csv's day/month/year style)
    Time = format(TIME, "%H:%M:%S"),
    Latitude,
    Longitude,
    Island = "New Island",
    Colony,
    Species = "Striated caracara",
    `Other species` = "",
    Capture = "",
    Metal = ifelse(is.na(Metal), "", Metal),
    Darvic,
    Contact = "",
    `Event comment` = "",
    Age = ""
  )

# 6. Write Output ----
# save reformatted feeding data 
write.csv(feeding_long, "NWI_Rings_reformatted.csv", row.names = FALSE)
# write.csv(df, path, row.names = FALSE) = save without an extra index column

# 7. Combine With Original Dataset ----
# combine the 2 files together to create a new file 

caracara_original <- read.csv("Caracara_Data.CSV", stringsAsFactors = FALSE)
combined <- bind_rows(caracara_original, feeding_long)
# bind_rows() = stack two data frames on top of each other, matching by column name
write.csv(combined, "Caracara_Data_combined.csv", row.names = FALSE)

cat("Reformatted", nrow(feeding_long), "individual sightings from NWI Rings.xlsx\n")
cat("Combined dataset:", nrow(combined), "rows (", nrow(caracara_original),
    "original +", nrow(feeding_long), "new)\n")
