library(DSLite)
library(testthat)
library(dplyr)
library(stringr)

options(datashield.env = environment())
data("mtcars")
dslite.server <- DSLite::newDSLiteServer(tables = list(mtcars = mtcars))
data("logindata.dslite.cnsim")
logindata.dslite.cnsim <- logindata.dslite.cnsim %>%
  mutate(table = "mtcars")
dslite.server$config(defaultDSConfiguration(include = c("dsBase", "dsTidyverse", "dsDanger")))
dslite.server$assignMethod("selectDS", "selectDS")
dslite.server$aggregateMethod("exists", "base::exists")
dslite.server$aggregateMethod("classDS", "dsBase::classDS")
dslite.server$aggregateMethod("lsDS", "dsBase::lsDS")
conns <- datashield.login(logins = logindata.dslite.cnsim, assign = TRUE)

## ---- Assign expression --------------------------------------------------------------------------
test_that("datashield.assign.expr returns 'datashield error' message when datashield.return_errors = FALSE", {
  options("datashield.return_errors" = FALSE)
  expect_error(
    datashield.assign.expr(conns, symbol = "G", expr = "doesntwork"), 
    "There are some DataSHIELD errors, list them with datashield.errors()"
    )
})

test_that("datashield.assign.expr returns message when datashield.return_errors = TRUE", {
  options("datashield.return_errors" = TRUE)
  expect_error(
    datashield.assign.expr(conns, symbol = "G", expr = "doesntwork"),
  )
})

test_that("datashield.assign.expr throws error if external script called", {
  options("datashield.return_errors" = TRUE)
  expect_error(
    source("testdata/r-fail-loop.R")
  )
})
  
test_that("datashield.assign.expr throws error if external script called", {
  options("datashield.return_errors" = FALSE)
  expect_error(
    source("testdata/r-fail-loop.R")
  )
})

test_that("datashield.assign.expr doesn't return errors if there aren't any", {
  cally <- call("absDS", 10)
  options("datashield.return_errors" = FALSE)
  expect_silent(
    datashield.assign.expr(conns, symbol = "new_obj", expr = cally) 
  )
  options("datashield.return_errors" = TRUE)
  expect_silent(
    datashield.assign.expr(conns, symbol = "new_obj", expr = cally) 
  )
})
