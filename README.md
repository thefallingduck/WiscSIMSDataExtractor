# WiscSIMSDataExtractor

Package for extracting data from [WiscSIMS laboratory](http://www.geology.wisc.edu/~wiscsims/index.html) archival excel files and reformatting it for import into the [Sparrow data system](https://sparrow-data.org/).

### Funding support

# Sparrow
[![NSF-1740694](https://img.shields.io/badge/NSF-1740694-blue.svg)](https://nsf.gov/awardsearch/showAward?AWD_ID=1740694)
# WiscSIMS

## Current functions

1.  `ColumnRename()`

    Standardizes column names using a lookup table based on observation of the SIMS archive.

2.  `GeneralSIMSImporter()`

    Uses `ColumnRename()` and associated code to standardize the excel tables and prepare them for import into Sparrow. This is a useful stand alone function for quickly opening and browsing SIMS data and is the core of the `SessionSummary.Rmd` document.

3.  `SparrowReformater()`

    Creates a JSON file and uses a `PUT()` to hand the file over to Sparrow. Additional code for implementation in docker with Sparrow is in [WiscSIMS-Sparrow github repository](https://github.com/EarthCubeGeochron/Sparrow-WiscSIMS/blob/master/importer/importer-script.R).

## Improvements to make...

-   [x] Make `BracketRecalculate()` a stand alone function
-   [x] Make `MountID()` a stand alone function
-   [ ] Make `SessionSummary()` markdown document a stand alone function
-   [ ] Make lookup tables as dataframes within the package.
-   [ ] Add more documentation and finish packaging the functions.
-   [ ] Refine error handling and reporting.
