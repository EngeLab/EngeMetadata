EngeMetadata package template setup
================
Jason T. Serviss
2018-08-17

Release build:
<a href="https://travis-ci.org/EngeLab/EngeMetadata"><img src="https://travis-ci.org/EngeLab/EngeMetadata.svg?branch=master"></a>

Test coverage:
[![codecov](https://codecov.io/gh/EngeLab/EngeMetadata/branch/master/graph/badge.svg)](https://codecov.io/gh/EngeLab/EngeMetadata)

All instructions herein are case sensitive and tense specific, i.e.
“Well” is not the same as “well” and “Well” is not the same as
“Wells”.

### Workbook

  - Must include 3 sheets named “Plate”, “Columns”, and “Wells”. Note
    that the names are plural.

### Plate sheet

  - Should only contain 2 columns named “Key” and “Value”.
  - The unique\_key key is mandatory and should match the file name.
  - The wells\_in\_plate key is mandatory. Possible values are 384 or
    96.

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
