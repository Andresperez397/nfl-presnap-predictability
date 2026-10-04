# Run the test suite from the project root: Rscript tests/run_tests.R
library(testthat)
test_dir("tests/testthat", reporter = "summary", stop_on_failure = TRUE)
