# Test runner for testthat framework
# This file is required by testthat to run all tests in tests/testthat/

library(testthat)
library(exams)

# Set testing environment
Sys.setenv(TESTING = "TRUE")

# Run all tests
test_check("RepositorioMatematicasICFES")
