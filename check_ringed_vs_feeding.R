# For Jeff's proposal question: of the birds newly ringed this May/June
# (Caracara_Data.CSV, Capture == "New tag"), which ones were NEVER seen at
# any of the 4 feeding-watch stations (NWI Rings.xlsx "Feeding Data" sheet,
# already reformatted into NWI_Rings_reformatted.csv)?
#
# IMPORTANT: this keeps the two source files separate (rather than reading
# Caracara_Data_combined.csv) and tags each row with where it came from.
# That distinction matters because some Colony names appear in BOTH files
# (e.g. "Burnt Island" shows up in the original ringing/resighting records
# too, not just the feeding-watch data) -- so "seen at a feeding station"
# has to mean "appears in the feeding-watch file specifically," not "has a
# matching colony name."
#
# Individual identity uses the same Metal + Darvic backfill logic as app.R:
# 1 Darvic = 1 individual, so if either file ever records a real Metal
# number for a given Darvic, that Metal is attached to every record of that
# Darvic in EITHER file -- this is what lets a bird ringed with both a Metal
# and Darvic get correctly matched to a feeding-watch sighting that only
# recorded its Darvic (or vice versa).

library(dplyr)

original <- read.csv("Caracara_Data.CSV", stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%Y"), source = "original",
         Metal = as.character(Metal))

feeding <- read.csv("NWI_Rings_reformatted.csv", stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%Y"), source = "feeding",
         Metal = ifelse(is.na(Metal), "", as.character(Metal)))

all_data <- bind_rows(original, feeding)

# Same Metal + Darvic backfill as app.R, run across BOTH files together so a
# link discovered in either source is used to complete the ID in the other
metal_lookup <- all_data %>%
  filter(!is.na(Darvic), !Metal %in% c("", "X", "None", "?")) %>%
  distinct(Darvic, Metal)

metal_conflicts <- metal_lookup %>% group_by(Darvic) %>% filter(n() > 1)
if (nrow(metal_conflicts) > 0) {
  warning("Darvic codes linked to more than one Metal number: ",
          paste(unique(metal_conflicts$Darvic), collapse = ", "))
}

metal_lookup <- metal_lookup %>%
  group_by(Darvic) %>%
  summarize(known_metal = first(Metal), .groups = "drop")

all_data <- all_data %>%
  left_join(metal_lookup, by = "Darvic") %>%
  mutate(
    Metal_filled = case_when(
      !Metal %in% c("", "X", "None", "?") ~ Metal,
      !is.na(known_metal) ~ known_metal,
      TRUE ~ NA_character_
    ),
    individual_id = paste0(
      ifelse(is.na(Metal_filled), "NA", Metal_filled), " - ",
      ifelse(is.na(Darvic), "NA", Darvic)
    )
  )

# Birds newly ringed this May/June
ringed <- all_data %>%
  filter(source == "original", Capture == "New tag") %>%
  select(individual_id, ringed_date = Date, ringed_colony = Colony, Species)

cat("Newly ringed individuals (New tag rows):", nrow(ringed), "\n")
cat("Distinct individual IDs among them:", n_distinct(ringed$individual_id), "\n\n")

# Which individual IDs ever appear in the feeding-watch file, specifically
seen_at_feeding <- all_data %>%
  filter(source == "feeding") %>%
  distinct(individual_id) %>%
  pull(individual_id)

result <- ringed %>%
  mutate(
    seen_at_feeding_station = individual_id %in% seen_at_feeding,
    # Days between ringing and the end of the observation window -- birds
    # ringed near the end have had little chance to be resighted yet, so
    # this is important context alongside "never seen" (see caveat below)
    days_of_opportunity = as.integer(max(all_data$Date, na.rm = TRUE) - ringed_date)
  ) %>%
  arrange(seen_at_feeding_station, ringed_date)

write.csv(result, "ringed_vs_feeding_check.csv", row.names = FALSE)

never_seen <- result %>% filter(!seen_at_feeding_station)

cat("=== SUMMARY ===\n")
cat("Ringed this May/June:", n_distinct(result$individual_id), "individuals\n")
cat("Seen at a feeding station at some point:",
    n_distinct(result$individual_id[result$seen_at_feeding_station]), "\n")
cat("NEVER seen at a feeding station:", n_distinct(never_seen$individual_id), "\n\n")

cat("=== Never seen at a feeding station ===\n")
print(never_seen, row.names = FALSE)

cat("\nFull per-bird result written to ringed_vs_feeding_check.csv\n")
