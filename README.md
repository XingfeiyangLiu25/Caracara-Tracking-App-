# Caracara Tracker

A Shiny app for exploring Striated caracara ring-resighting records from New
Island, Falkland Islands.

**Live app:** https://xingfeiyangliugamblelabrotation.shinyapps.io/Gamble_Lab_Rotation/

## What it does

- Maps every sighting of a bird as a point, colored by individual and shaped
  by species.
- Optionally connects each bird's sightings into a track, in time order.
- Marks each bird's first sighting (white border) and most recent sighting
  (black border) distinctly.
- Filters by species, colony, age, date range, and individual.
- Data Table tab shows the currently filtered rows and lets you download
  them as CSV.
- About tab explains the individual ID convention below.

## Individual ID convention

Birds are identified by a combined **Metal + Darvic** ID, since either tag
alone can be missing on a given sighting (resightings from a distance often
only capture the Darvic; the Metal number is only readable when a bird is
actually in hand):

- `313 - Green D19` — both known
- `NA - Green P15` — no Metal ever recorded for this Darvic
- `302 - NA` — no Darvic read yet

## Files

| File | Purpose |
|---|---|
| `app.R` | The Shiny app itself |
| `Caracara_Data.CSV` | Original ring-resighting data |
| `NWI Rings.xlsx` | Feeding-station watch data (raw) |
| `reformat_NWI_rings.R` | Reformats the "Feeding Data" sheet of `NWI Rings.xlsx` into the same row-per-sighting layout as `Caracara_Data.CSV`, and combines the two |
| `NWI_Rings_reformatted.csv` | Output of the script above |
| `Caracara_Data_combined.csv` | Original + reformatted feeding data combined. This is what `app.R` actually reads |
| `check_ringed_vs_feeding.R` | One-off analysis: of the birds ringed this May/June, which were never seen at a feeding station |
| `ringed_vs_feeding_check.csv` | Output of the script above |

## Running locally

Open `app.R` in RStudio and click **Run App**, or from the console:

```r
shiny::runApp()
```

Required packages: `shiny`, `leaflet`, `dplyr`, `shinyWidgets`, `DT`

## Credits

Feiyang Liu, Jeff White, Amandine Gamble, and collaborators. Code and visualization
concepts adapted from Braxton Sizemore's skua tracking app and Xibiao Wang's
ELISA prevalence app. 

## Contact

xl857@cornell.edu
