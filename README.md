# Fenton growth curves - shiny app

## Usage
The purpose of this app is not to be a fully customizable growth chart plotter, this already exists at https://peditools.org/peditools_universal/. Values can only be entered through a HTTP GET request:
- Gestational age (in weeks): `&PML_GET=23.14,23.28`. 23.145 equals 23 weeks + 1 day (=23+1/7).
- weight (in grams): &weight_GET=`400,500,600`
- length (in cm): `&length_GET=34,33,NA`
- head circumference (in cm): `&HC_GET=23,NA,25`
- Sex (M or F): `&sex_GET=M`
  
The number of values in each field needs to be equal, and needs to contain either a value > 0 or "NA".

## Example URL
- Basic example: http://rubenvp.shinyapps.io/fenton/?advanced=yes&sex_GET=M&PML_GET=23%2B1/7,24%2B1/7,25%2B1/7&weight_GET=400,500,600&HC_GET=23,NA,25&length_GET=34,33,NA
- Real world example: https://rubenvp.shinyapps.io/fenton/?advanced=no&sex_GET=M&PML_GET=29.14,29.29,29.43,29.57,29.71,29.86,30.00,30.14,30.29,30.43,30.57,30.71,30.86,31.00,31.14,31.29,31.43,31.57,31.71,31.86,32.00,32.14,32.29,32.43,32.57,32.71,32.86,33.00,33.14,33.29,33.43,33.57,33.71,33.86,34.00,34.14,34.29,34.43,34.57&weight_GET=NA,1090,1040,1000,1010,1010,1110,1120,1180,NA,1140,1130,1190,NA,NA,1210,NA,1140,NA,NA,1275,1270,NA,1430,1410,1500,NA,NA,1655,1610,1610,1620,NA,1630,1750,1750,NA,1820,1860&HC_GET=26.3,NA,NA,NA,NA,NA,NA,26.1,NA,NA,NA,NA,NA,NA,27.1,NA,NA,NA,NA,NA,NA,28,NA,NA,NA,NA,NA,NA,28.5,NA,NA,28.5,NA,NA,NA,29.5,NA,NA,NA&length_GET=38.6,38.6,NA,NA,39,NA,NA,38.2,NA,NA,NA,NA,NA,NA,38.2,NA,NA,NA,NA,NA,NA,40,NA,NA,NA,NA,NA,NA,42.5,NA,NA,NA,NA,NA,NA,43,NA,NA,NA

## Background and setup
I can't share the raw data (L,M,S values), but it can be obtained in Excel format from dr. Tanis Fenton (tfenton@ucalgary.ca). Afterwards, the data need to be reworked so that it follows the format of the csv files in the `data` folder in this github repository (I have included the first 2 lines of my reworked csv files as example). After obtaining these csv files, reworked datafiles (containing P03 > P97 threshold) can be obtained with `growthcurves_*.R`. 

## Installation

The app can be used at rubenvp.shinyapps.io/fenton or self hosted through Docker. 

```
docker run -dp 0.0.0.0:3838:3838   -v /data:/srv/shiny-server/data  --platform linux/amd64 rmvpaeme/fenton:0.1
```

The `data` folder should contain:

```
- boys_HC.csv
- boys_length.csv
- boys_weight.csv
- boys_all.csv # output from growthcurves_boys.R
- boys_all_spread.csv # output from growthcurves_boys.R
- girls_HC.csv
- girls_length.csv
- girls_weight.csv
- girls_all.csv # output from growthcurves_girls.R
- girls_all_spread.csv # output from growthcurves_girls.R
```

Examples are available in this github repository

Afterwards, the app can be found at:

```
localhost:3838
```

or

```
server-ip:3838
```

## References
Fenton TR, Kim JH. A systematic review and meta-analysis to revise the Fenton growth chart for preterm infants. BMC Pediatr. 2013;13:59.


