#run from package root: source('tests/testthat/test-files/test1.R')
meta <- EngeMetadata:::getPlateMeta("test1", 'data/package_testing/')
save(meta, file = 'tests/testthat/test-files/test1.rda')
