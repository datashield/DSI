library(DSLite)
library(testthat)
library(cli)
library(dplyr)

options(datashield.env = environment())
data("mtcars")
dslite.server <- DSLite::newDSLiteServer(tables = list(mtcars = mtcars))
data("logindata.dslite.cnsim")
logindata.dslite.cnsim <- logindata.dslite.cnsim %>%
  mutate(table = "mtcars")
dslite.server$config(defaultDSConfiguration(include = c("dsBase", "dsTidyverse", "dsDanger")))
dslite.server$assignMethod("selectDS", "selectDS")
dslite.server$assignMethod("absDS", "abs")
dslite.server$aggregateMethod("exists", "base::exists")
dslite.server$aggregateMethod("classDS", "dsBase::classDS")
dslite.server$aggregateMethod("lsDS", "dsBase::lsDS")
conns <- datashield.login(logins = logindata.dslite.cnsim, assign = TRUE)

# ---- Aggregate expression --------------------------------------------------------------------------
test_that("datashield.assign.expr returns 'datashield error' message when datashield.return_errors = FALSE", {
  options("datashield.return_errors" = FALSE)
  expect_error(
    datashield.aggregate(conns, call("classDS", "doesntexist")),  
    "There are some DataSHIELD errors, list them with datashield.errors()"
  )
})

test_that("datashield.assign.expr returns error message when datashield.return_errors = TRUE", {
  options("datashield.return_errors" = TRUE)
  expect_error(
    datashield.aggregate(conns, call("classDS", "doesntexist")),  
    "object 'doesntexist' not found"
  )
})

test_that("datashield.aggregate throws error if external script called", {
  options("datashield.return_errors" = TRUE)
  expect_error(
    source("testdata/r-fail-aggregate-loop.R"),
  )
})

test_that("datashield.aggregate throws error if external script called", {
  options("datashield.return_errors" = FALSE)
  expect_error(
    source("testdata/r-fail-aggregate-loop.R"),
    "There are some DataSHIELD errors, list them with datashield.errors()"
  )
})

test_that("datashield.aggregate doesn't return errors if there aren't any", {
  cally <- call("classDS", "mtcars")
  options("datashield.return_errors" = FALSE)
  expect_silent(
    datashield.aggregate(conns, cally)
  )
  options("datashield.return_errors" = TRUE)
  expect_silent(
    datashield.aggregate(conns, cally)
  )
})
