library(DSLite)
library(DSI)
library(testthat)
library(dplyr)

options(datashield.env = environment())
logindata.dslite.cnsim <- setupCNSIMTest()
conns <- datashield.login(logins = logindata.dslite.cnsim, assign = F)

for (i in 1:10){
  datashield.assign.expr(conns, symbol = "G", expr = "doesntwork")
}
