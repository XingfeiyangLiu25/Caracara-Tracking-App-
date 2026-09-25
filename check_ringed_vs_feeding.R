# For Jeff's proposal question: of the birds newly ringed this May/June
# (Caracara_Data.CSV, Capture == "New tag"), which ones were NEVER seen at
# any of the 4 feeding-watch stations (NWI Rings.xlsx "Feeding Data" sheet,
# already reformatted into NWI_Rings_reformatted.csv)?
#
# not reading Caracara_Data_combined.csv because some Colony names appear in both 
# ring/resighting record and feeding-watch data, so "seen at a feeding station"
# has to mean "appears in the feeding-watch file specifically," not "has a
# matching colony name."

# 1. Setup ----
# load the two source files independently and stamp each row with a 
# "source" column saying which file it came from for section 4 use. 
library(dplyr)

original <- read.csv("Caracara_Data.CSV", stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%Y"), source = "original",
         Metal = as.character(Metal))
         # as.character() forces Metal to text in both files -- read.csv()
         # guessed it as numbers here vs text in the other file, which
         # blocks bind_rows() below from stacking them

feeding <- read.csv("NWI_Rings_reformatted.csv", stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%Y"), source = "feeding",
         Metal = ifelse(is.na(Metal), "", as.character(Metal)))

all_data <- bind_rows(original, feeding)
# bind_rows() = stack both data frames, matching columns by name; source
# column (set above) is what lets us tell which file each row came from later

# 2. Build Combined Metal + Darvic ID ----
# same identity system as app.R. Built from all_data (both
# files together) so a Metal-Darvic pairing discovered in either file
# completes the ID for that bird everywhere.

metal_lookup <- all_data %>%
  filter(!is.na(Darvic), !Metal %in% c("", "X", "None", "?")) %>%
  distinct(Darvic, Metal)   # distinct() = one row per unique Darvic+Metal pair

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
      !Metal %in% c("", "X", "None", "?") ~ Metal,    # this row already has a real Metal
      !is.na(known_metal) ~ known_metal,               # else pull from another record of the same Darvic
      TRUE ~ NA_character_                              # else genuinely unknown
    ),
    individual_id = paste0(
      ifelse(is.na(Metal_filled), "NA", Metal_filled), " - ",
      ifelse(is.na(Darvic), "NA", Darvic)
    )
  )

# 3. Identify Ringed Birds ----
# pull out exactly newly ringed individuals from the original file this May/June

ringed <- all_data %>%
  filter(source == "original", Capture == "New tag") %>%
  select(individual_id, ringed_date = Date, ringed_colony = Colony, Species)
  # select(new_name = old_col) = keep + rename a column in one step

cat("Newly ringed individuals (New tag rows):", nrow(ringed), "\n")
cat("Distinct individual IDs among them:", n_distinct(ringed$individual_id), "\n\n")
# n_distinct(x) = count of unique values, shorthand for length(unique(x))

# 4. Check Feeding-Station Presence ----
# for each ringed bird from section 3, check whether it appeared in feeding watch row
# and if so, how many times. days_of_opportunity matters because a bird ringed at the end
# of the dataset hasn't had a fair chance to be resighted yet

seen_at_feeding <- all_data %>%
  filter(source == "feeding") %>%
  distinct(individual_id) %>%
  pull(individual_id)   # pull() = extract one column as a plain vector

result <- ringed %>%
  mutate(
    seen_at_feeding_station = individual_id %in% seen_at_feeding,
    days_of_opportunity = as.integer(max(all_data$Date, na.rm = TRUE) - ringed_date)
    # as.integer(date1 - date2) = whole number of days between two dates
  ) %>%
  arrange(seen_at_feeding_station, ringed_date)  # arrange() = sort rows

# 5. Save & Report ----
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
