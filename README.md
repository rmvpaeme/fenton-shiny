# Fenton growth curves - shiny app

A Shiny app that plots the Fenton 2013 preterm growth curves (weight, length, head
circumference) for an infant and overlays measured values, plus a percentile table.

The interface is **bilingual (Dutch by default, English)** — switch with the small language
toggle in the top-right corner — and uses a calm **Nord Light** theme.

## Usage
The purpose of this app is not to be a fully customizable growth chart plotter, this already exists at https://peditools.org/peditools_universal/. 

Values can be entered in two ways:
- **Excel upload** (default): upload a workbook following `example_excel.xlsx` in this repository.
- **Manual / HTTP GET request** (the "manual entry (advanced)" input method): see below.

For the HTTP GET request are the following parameters required:
- Gestational age (in weeks): `&PML_GET=23.14,23.28`. 23.142 equals 23 weeks + 1 day (=23+1/7).
- weight (in grams): `&weight_GET=400,500,600`
- length (in cm): `&length_GET=34,33,NA`
- head circumference (in cm): `&HC_GET=23,NA,25`
- Sex (M or F): `&sex_GET=M`
  
The number of comma separated values in each field needs to be equal, and needs to contain either a value > 0 or "NA".

In addition, an Excel file can be used to input and upload measurements (see example_excel.xlsx) in this Github repository. 

## Example URL
- Basic example: http://rubenvp.shinyapps.io/fenton/?advanced=yes&sex_GET=M&PML_GET=23%2B1/7,24%2B1/7,25%2B1/7&weight_GET=400,500,600&HC_GET=23,NA,25&length_GET=34,33,NA
- Real world example: https://rubenvp.shinyapps.io/fenton/?advanced=no&sex_GET=M&PML_GET=29.14,29.29,29.43,29.57,29.71,29.86,30.00,30.14,30.29,30.43,30.57,30.71,30.86,31.00,31.14,31.29,31.43,31.57,31.71,31.86,32.00,32.14,32.29,32.43,32.57,32.71,32.86,33.00,33.14,33.29,33.43,33.57,33.71,33.86,34.00,34.14,34.29,34.43,34.57&weight_GET=NA,1090,1040,1000,1010,1010,1110,1120,1180,NA,1140,1130,1190,NA,NA,1210,NA,1140,NA,NA,1275,1270,NA,1430,1410,1500,NA,NA,1655,1610,1610,1620,NA,1630,1750,1750,NA,1820,1860&HC_GET=26.3,NA,NA,NA,NA,NA,NA,26.1,NA,NA,NA,NA,NA,NA,27.1,NA,NA,NA,NA,NA,NA,28,NA,NA,NA,NA,NA,NA,28.5,NA,NA,28.5,NA,NA,NA,29.5,NA,NA,NA&length_GET=38.6,38.6,NA,NA,39,NA,NA,38.2,NA,NA,NA,NA,NA,NA,38.2,NA,NA,NA,NA,NA,NA,40,NA,NA,NA,NA,NA,NA,42.5,NA,NA,NA,NA,NA,NA,43,NA,NA,NA

## Background and setup
The raw data (L,M,S values) was kindly shared by dr. Tanis Fenton (tfenton@ucalgary.ca). If you want to selfhost this app you'll also have to request access to this data. 

After obtaining the data, the Excel sheet need to be reworked so that it follows the format of the csv files in the `data` folder in this github repository (I have included the first 2 lines of my reworked csv files as example). After obtaining these csv files, reworked datafiles (containing P03 > P97 threshold) can be obtained with `growthcurves_*.R`. 

## Installation

The app can be used at rubenvp.shinyapps.io/fenton or self-hosted through Docker.

The reference data (L/M/S values) is **not bundled in the image** — you must volume-mount your own `data/` directory (see Background and setup above).

```sh
docker run -dp 0.0.0.0:3838:3838 \
  -v /path/to/data:/srv/shiny-server/data \
  --platform linux/amd64 \
  rmvpaeme/fenton:v1.0.0-beta
```

The mounted `data/` folder must contain:

```
boys_HC.csv
boys_length.csv
boys_weight.csv
boys_all.csv          # output from growthcurves_boys.R
boys_all_spread.csv   # output from growthcurves_boys.R
girls_HC.csv
girls_length.csv
girls_weight.csv
girls_all.csv         # output from growthcurves_girls.R
girls_all_spread.csv  # output from growthcurves_girls.R
```

You'll have to generate these CSV files yourself after requesting the raw data from dr. Fenton (see above).

The app is then available at `http://localhost:3838` or `http://server-ip:3838`.

### Docker image details

| | Value |
| --- | --- |
| Image | `rmvpaeme/fenton` |
| Tags | `v1.0.0-beta`, `latest` |
| Base | `rocker/shiny:4.4.2` (R 4.4.2, Ubuntu 22.04 jammy) |
| R packages | shiny, tidyverse, readxl, bslib, shiny.i18n, shinycssloaders, scales, DT |
| Architecture | linux/amd64 |

## Changelog

### v1.0.0-beta (2026-05-29)

#### Code refactor

- Replaced three near-identical `renderPlot` blocks with a single `growth_plot()` helper
- Reference CSVs (Fenton L/M/S percentile curves) now loaded once at startup instead of on every reactive call
- Excel workbook now read once per upload (was 4 separate `read_excel` calls)
- URL query-param handling collapsed from 7 repeated `if` blocks to a single loop
- Replaced `eval(parse(text = x))` PML parser (arbitrary code execution risk) with a safe fraction parser; verified bit-for-bit identical output for all documented formats (`23+1/7`, decimals, integers)
- Removed all dead/commented-out code

#### Bilingual interface (NL/EN)

- Dutch by default; English available via a small toggle button fixed in the top-right corner
- All UI labels, sidebar inputs, tab titles, disclaimer, usage text, and plot axis labels/subtitles are translated
- Language switch is client-side (preserves uploaded file and active tab)
- Strings managed in `www/translations.json`

#### UI/UX improvements

- Confusing "Show advanced settings" yes/no dropdown replaced with clearly labelled radio buttons (input id and values preserved for full URL GET backwards-compatibility)
- Loading spinner added to each plot panel
- Flat header with app title and subtitle

#### Styling — Nord Light theme

- Ported Nord Light palette from companion app: frost blue `#5E81AC` for curves and chrome, aurora red `#BF616A` for measured data points, soft `#ECEFF4`/`#E5E9F0` backgrounds
- `bslib` Bootstrap 5 theme replaces the old `shinythemes` flatly theme
- Custom `www/nord.css`: flat header, soft borders, muted nav tabs, no loud accent colours
- Font stack: UGent Panno Text (self-hosted, opt-in via `www/fonts/`) with Arial fallback
- Plot grid colour harmonised with theme (`#E5E9F0`)

#### Head circumference formatting

- HC displayed as XX.X (one decimal) everywhere: plot y-axis labels and percentile table

#### Infrastructure

- `Dockerfile` updated: added `bslib`, `shiny.i18n`, `shinycssloaders`, `scales`; removed unused `shinythemes`, `shinyTime`
- `.DS_Store` added to `.gitignore`

## Disclaimer
This application is not associated with dr. Fenton or University of Calgary. 

## References
Fenton TR, Kim JH. A systematic review and meta-analysis to revise the Fenton growth chart for preterm infants. BMC Pediatr. 2013;13:59.


