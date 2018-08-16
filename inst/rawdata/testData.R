#run from package root: source('inst/rawdata/testData.R')
testData <- EngeMetadata:::getPlateMeta("test1", 'data/package_testing/')
save(testData, file = 'data/testData.rda')
