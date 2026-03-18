# Soil BON Project – Map of Sampling Locations


## Overview

This repository contains R code and resources to generate maps of sampling locations for the **Soil BON** (Soil Biodiversity Observation Network) project. The maps highlight **protected** and **unprotected** soil sampling sites globally, including magnified insets for specific points of interest.

The project outputs include:

- Globe-style maps (orthographic projection)  
- Zoomed panels for selected sites  
- Exported in **PNG** and **SVG** formats for publications and presentations  

> **Disclaimer:** The boundaries and names shown on these maps do not imply official endorsement or acceptance by the project.


## Author & Maintainers

- **Author:** Romy Zeiss  
- **Created:** 2026-03-16  
- **Last Updated:** 2026-03-18  


## Features

- Display **all sampling sites**   
- Highlight **protected vs. unprotected sites**  
- Zoom panels for **specific sampling locations** as defined in column "Sample_point" and object "sample_points" 
- Country borders displayed with disclaimer  
- Color scheme consistent with project logo colors  
- Export-ready in **SVG** for high-quality vector graphics  


## Requirements

R packages required:

```r
tidyverse
maps
readxl
leaflet
htmlwidgets
htmltools
ggmagnify
magick
svglite
patchwork
ggimage
```

Note: ggmagnify may need installation from Hugh Jones’ R-universe:

```r
install.packages(
  "ggmagnify", 
  repos = c(
    "https://hughjonesd.r-universe.dev", 
    "https://cloud.r-project.org"
  )
)
```


## Data

- World map: Obtained from ggplot2::map_data("world")
- Sampling data: Excel sheets:
- field_records_digitized_campaign1.xlsx
- field_records_digitzed_campaign2.xlsx
- Logo: Soil BON logo (for embedding in maps)

Ensure the paths to data and logo are correct before running scripts.


## Usage

- Load packages and data
- Clean and merge datasets with f_clean_data() and f_subset()
- Define zoom-in sample points
- Generate globe map with geom_magnify()
- Optional: Generate splayed map with inset zoom using patchwork::inset_element()
- Save outputs using ggsave(), for example:

```r
ggsave(
  "Map_sites_globe_<date>.png", 
  p, 
  height = 5.65, 
  width = 5.65, 
  dpi = 600
)

ggsave(
  "Map_sites_splayed_<date>.svg", 
  last_plot(), 
  dpi = 600
)
```


## Notes & Tips

- Zoom panel points can be enlarged only within the inset using filtering.
- When exporting SVG, some grobs (e.g., geom_magnify_tile()) may not render correctly — consider using PNG or constructing insets via patchwork for full SVG compatibility.
- Longitude and latitude must match the map CRS for rectangles or zoom highlighting.


## License

This project is licensed under the CC-BY 4.0 license - see the LICENSE.txt file for details.
