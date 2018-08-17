#run from package root: source('inst/rawdata/testData.R')
testData <- EngeMetadata:::getPlateMeta("test1", 'data/package_testing/', FALSE)
save(testData, file = 'data/testData.rda')

p <- file.path("inst/rawdata", paste0("test1", ".xlsx"))
googledrive::drive_download("test1", path = p, overwrite = TRUE, verbose = FALSE)
