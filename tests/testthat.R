library(testthat)
library(soar)
test_dir("testthat", reporter = c("check", "progress"))

test_check("soar")
