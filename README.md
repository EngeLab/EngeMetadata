EngeMetadata package
================
Jason T. Serviss
2018-10-09

Release build:
<a href="https://travis-ci.org/EngeLab/EngeMetadata"><img src="https://travis-ci.org/EngeLab/EngeMetadata.svg?branch=master"></a>
Test coverage:
[![codecov](https://codecov.io/gh/EngeLab/EngeMetadata/branch/master/graph/badge.svg)](https://codecov.io/gh/EngeLab/EngeMetadata)

## Installation

``` r
if(!"devtools" %in% rownames(installed.packages())) {
  install.packages("devtools")
}
devtools::install_github("EngeLab/EngeMetadata")
```

## Template setup

All instructions herein are case sensitive and tense specific, i.e.
“Well” is not the same as “well” and “Well” is not the same as
“Wells”.

The template design is based on sheet precedence with Wells \> Columns
\> Plate. This means e.g. if a key is present in the Plate sheet and
also present in Columns sheet, the values in the Columns sheet will
overwrite the values in the Plate sheet where they differ.

### Workbook

  - Must include 3 sheets named “Plate”, “Columns”, and “Wells”. Note
    that the names are plural.

### Plate sheet

  - Should only contain 2 columns named “Key” and “Value”.
  - The unique\_key key is mandatory and should match the file name.
  - The wells\_in\_plate key is mandatory. Possible values are 384 or
    96. More possibilities in *Adding non-standard metadata* section.
  - Note that if the whole plate does not contain cells, this may be
    annotated with the “Missing” key (in either the Columns or Wells
    sheet). If “Missing” is given the value TRUE, it these samples will
    be removed from the metadata downstream. Otherwise the whole plate,
    according to the wells\_in\_plate key, is assumed to contain cells.

### Columns sheet

  - The Column key is mandatory. Note that this is singular.
  - The Column key values must include all columns present in the plate
    type given by the wells\_in\_plate key in the Plate sheet. Single
    digit columns should be prefixed with a 0, i.e. “01” not “1”.

### Wells sheet

  - The Well key is mandatory. Note that this is singular.
  - The Well key values must include all wells in the plate type given
    by the wells\_in\_plate key in the Plate sheet. Single digit columns
    should be prefixed with a 0, i.e. “A01” not “A1”.

### Dates

  - Dates are expected to have the word “date” somewhere in the key.
  - Dates including a month or day that is a single digit should be
    prefixed with a “0”, i.e. “01” for January and not “1” likewise “01”
    for the first of the month not “1”.
  - Dates should be formatted year, month, day without spaces.
    Acceptable syntax is: 20181201 or 181201.

## Importing metadata into R

There is currently only one exported function in the package which is
called “metadata”. The metadata function takes a plate/file name and a
googledrive path as arguments and downloads, processes, and returns the
metadata as a tibble. Precedence is resolved automatically and the
metadata is expanded so that there is one entry per sample.

## Adding non-standard metadata

There is some limited functionality for adding metadata for non-standard
datasets, e.g. datasets produced by other labs that don’t necessiarily
conform well to the typical Enge lab setup. This can be achieved by
substituting the value in the wells\_in\_plate key for the number of
samples and, in addition, removing all columns from the Columns sheet.
The current setup only allows for metadata associated with an entire
experiment or individual samples (which can be annotated in the wells
sheet).
