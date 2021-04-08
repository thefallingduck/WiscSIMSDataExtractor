# WiscSIMSDataExtractor

Package for extracting data from [WiscSIMS laboratory](http://www.geology.wisc.edu/~wiscsims/index.html) archival excel files and reformatting it for import into the [Sparrow data system](https://sparrow-data.org/).

## Current functions

1.  `ColumnRename()`

    Standardizes column names using a lookup table based on observation of the SIMS archive.

2.  `GeneralSIMSImporter()`

    Uses `ColumnRename()` and associated code to standardize the excel tables and prepare them for import into Sparrow. This is a useful stand alone function for quickly opening and browsing SIMS data and is the core of the `SessionSummary.Rmd` document.

3.  `SparrowReformater()`

    Creates a JSON file and uses a `PUT()` to hand the file over to Sparrow. Additional code for implementation in docker with Sparrow is in [WiscSIMS-Sparrow github repository](https://github.com/EarthCubeGeochron/Sparrow-WiscSIMS/blob/master/importer/importer-script.R).

4.  `AddBracketStructure()`

    Adds the bracket structure to the data frame using REGEX for the material. Depends on the `BlockID()` structure to look for the STD-Sample-STD blocks. Also includes the step of adding a mount name with the `MaxStringRepeated()` because it also depends on the `BlockID()` output.

5.  `BlockID()`

    Identified runs of Standard or sample analyses and determines likely mount changes by looking for STD-Sample-STD-STD blocks. The pattern of STD-STD is key.

6.  `BracketRecalc()`

    Recalculates running-standard corrected isotope ratio, yield, and the hydride ratio by using bracketing standards.

7.  `CatchExcelBrackets()`

    Search for excel bracket summary (average and 2SD) and ensure that has a mount name.

8.  `MaxStringRepeated()`

    Function to determine the maximum string repeated between analyses based on the Comment column in the original data sheet. This should be used only with Sample data because running standards often have uniform names that do not include the mount name.

9.  `StandardID()`

    Identifies if an analysis is of a standard based on if a Comment matches the REGEX for standards in a look-table and adds a logical is_standard for if the material is a standard based on the look up table.

## Improvements to make...

-   [ ] Fix `BracketRecalculate()` to require STD-Sample-STD pattern. Currently can allow for failures (e.g. Sample - STD - STD).
-   [ ] Package data for look up tables once with the package?
-   [ ] Make `SessionSummary()` markdown document a stand alone function
