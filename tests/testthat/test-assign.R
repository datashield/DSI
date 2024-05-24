library(DSLite)
library(testthat)

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
  error_message <- tryCatch(
    datashield.assign.expr(conns, symbol = "G", expr = "doesntwork"), 
    error = function(e){
      return(conditionMessage(e))
    }
  )
  expect_equal(
    error_message,
    "There are some DataSHIELD errors, list them with datashield.errors()")
})

test_that("datashield.assign.expr returns message when datashield.return_errors = TRUE", {
  options("datashield.return_errors" = TRUE)
  expect_message(
    object = datashield.assign.expr(conns, symbol = "G", expr = "doesntwork"),
    regexp = "sim1: object 'doesntwork' not found", 
    fixed = TRUE
  )
})
