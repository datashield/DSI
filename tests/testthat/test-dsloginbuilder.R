make.standard.logins <- function() {
  builder <- newDSLoginBuilder()
  builder$append(server="server1", url="https://opal-demo.obiba.org",
                 table="datashield.CNSIM1", resource="datashield.CNSIM1r",
                 user="administrator", password="password",
                 options="list(ssl_verifyhost=0,ssl_verifypeer=0)")
  builder$append(server="server2", url="dslite.server", table="CNSIM2", resource="CNSIM2r", driver="DSLiteDriver")
  builder$append(server="server3", url="https://molgenis.example.org", table="CNSIM3", resource="CNSIM3r", token="123456789", driver="MolgenisDriver")
  builder$append(server="server4", url="dslite.server", table="CNSIM4", resource="CNSIM4r", driver="DSLiteDriver")
  builder$build()
}

expect.standard.logins <- function(logins) {
  expect_equal(nrow(logins), 4)
  expect_equal(ncol(logins), 9)
  expect_equal(logins$server, c("server1", "server2", "server3", "server4"))
  expect_equal(logins$url, c("https://opal-demo.obiba.org", "dslite.server", "https://molgenis.example.org", "dslite.server"))
  expect_equal(logins$table, c("datashield.CNSIM1", "CNSIM2", "CNSIM3", "CNSIM4"))
  expect_equal(logins$resource, c("datashield.CNSIM1r", "CNSIM2r", "CNSIM3r", "CNSIM4r"))
  expect_equal(logins$driver, c("OpalDriver", "DSLiteDriver", "MolgenisDriver", "DSLiteDriver"))
  expect_equal(logins$user, c("administrator", "", "", ""))
  expect_equal(logins$password, c("password", "", "", ""))
  expect_equal(logins$token, c("", "", "123456789", ""))
  expect_equal(logins$options, c("list(ssl_verifyhost=0,ssl_verifypeer=0)", "", "", ""))
}

test_that("builder works", {
  logins <- make.standard.logins()
  expect.standard.logins(logins)
})

test_that("builder copy constructor works", {
  builder <- newDSLoginBuilder(make.standard.logins())
  logins <- builder$build()
  expect.standard.logins(logins)
})

test_that("builder checks server is not empty", {
  builder <- newDSLoginBuilder()
  expect_error(builder$append(server="", url="https://molgenis.example.org", table="CNSIM3", token="123456789", driver="MolgenisDriver"))
})

test_that("builder checks server is not duplicated", {
  builder <- newDSLoginBuilder()
  builder$append(server="server3", url="https://molgenis.example.org", table="CNSIM3", token="123456789", driver="MolgenisDriver")
  expect_error(builder$append(server="server3", url="https://molgenis.example.org", table="CNSIM3", token="123456789", driver="MolgenisDriver"))
})

test_that("builder checks url is not empty", {
  builder <- newDSLoginBuilder()
  expect_error(builder$append(server="server3", url="", table="CNSIM3", token="123456789", driver="MolgenisDriver"))
})

test_that("builder checks table and resource are not both empty", {
  builder <- newDSLoginBuilder()
  expect_error(builder$append(server="server3", url="https://molgenis.example.org", table=NULL, token="123456789", driver="MolgenisDriver"))
})

test_that("builder recommends https", {
  builder <- newDSLoginBuilder()
  expect_warning(builder$append(server="server3", url="http://molgenis.example.org", table="CNSIM3", token="123456789", driver="MolgenisDriver"))
})
