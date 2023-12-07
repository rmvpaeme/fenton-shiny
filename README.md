# Fenton growth curves - shiny app

## Usage
Values can only be entered through a HTTP GET request:
- Gestational age (in weeks): `&PML_GET=23.14,23.28`. 23.145 equals 23 weeks + 1 day (=23+1/7).
- weight (in grams): &weight_GET=`400,500,600`
- length (in cm): `&length_GET=34,33,NA`
- head circumference (in cm): `&HC_GET=23,NA,25`
- Sex (M or F): `&sex_GET=M`
Furthermore, `?advanced=yes` needs to be specified.

## Example URL
http://rubenvp.shinyapps.io/fenton/?advanced=yes&sex_GET=M&PML_GET=23%2B1/7,24%2B1/7,25%2B1/7&weight_GET=400,500,600&HC_GET=23,NA,25&length_GET=34,33,NA

## Background and setup
I can't share the raw data (L,M,S values), but it can be obtained in Excel format from dr. Tanis Fenton (tfenton@ucalgary.ca). Afterwards, the data need to be reworked so that it follows the format of the csv files in the `data` folder in this github repository (I have included the first 2 lines of my reworked csv files as example). After obtaining these csv files, reworked datafiles (containing P03 > P97 threshold) can be obtained with `growthcurves_*.R`. 

## Installation

The app can be used at rubenvp.shinyapps.io/fenton or selfhosted. 

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


