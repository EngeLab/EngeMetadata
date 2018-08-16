#run from package root: source('tests/testthat/test-files/test1.R')
meta <- getPlateMeta("test1", 'Enge_lab/GFP_mouse/Annotation/package_testing')
save(meta, file = 'tests/testthat/test-files/test1.rda')
