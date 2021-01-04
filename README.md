# DataSHIELD Interface

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/DSI)](https://cran.r-project.org/package=DSI)

The DataSHIELD Interface (DSI) defines a set of [S4 classes and generic methods](http://adv-r.had.co.nz/S4.html) 
that can be implemented for accessing a data repository supporting the DataSHIELD infrastructure: controlled 
R commands to be executed on the server side are garanteeing that non disclosive information is returned 
to client side.

Learn more about [DataSHIELD](http://www.datashield.ac.uk/).

## Class Structure

The DSI classes are:

* `DSObject` a common base class for all DSI,
* `DSDriver` drives the creation of a connection object,
* `DSConnection` allows the interaction with the remote server; DataSHIELD operations such as 
aggregation and assignment return a result object; DataSHIELD setup status check can be performed (dataset access, 
configuration comparision),
* `DSResult` wraps access to the result, which can be fetched either synchronously or asynchronously 
depending on the capabilities of the data repository server.

All classes are *virtual*: they cannot be instantiated directly and instead must be subclassed. See [DSOpal](https://github.com/datashield/DSOpal) for a reference implementation of DSI based on the 
[Opal](https://www.obiba.org/pages/products/opal/) data repository.

These S4 classes and generic methods are meant **to be used for implementing connection to a DataSHIELD-aware data repository.**

## Higher Level Functions

In addition to these S4 classes, DSI provides functions to handle a list of remote data repository servers:

* `datashield.login` and `datashield.logout` will make use of the `DSDriver` paradigm to create `DSConnection`s
to the data repositories,
* `datashield.aggregate` and `datashield.assign` will perform typical DataSHIELD operations on `DSConnection`s, 
which result will be fetched through `DSResult` objects,
* `datashield.connections`, `datashield.connections_default` and `datashield.connections_find` are functions
for managing the list of `DSConnection` objects that will be discovered and used by the client-side analytic functions.
* `datashield.errors` will report the last R errors that may have occurred after a `datashield.assign` or `datashield.aggregate` call.
* Other data management functions are provided by the `DSConnection` objects:
  * `datashield.workspaces`, `datashield.workspace_save` and `datashield.workspace_rm` allow to manage R images 
  of the remote DataSHIELD sessions (to speed up data analysis sessions),
  * `datashield.symbols` and `datashield.symbol_rm` offer a minimalistic management of the R symbols living in 
  the remote DataSHIELD sessions,
  * `datashield.tables`, `datashield.table_status` list the tables and their accessibility across a set of data repositories,
  * `datashield.resources`, `datashield.resource_status` list the resources and their accessibility across a set of data repositories, 
  * `datashield.pkg_status`, `datashield.method_status` and `datashield.methods` are 
  utility functions to explore the DataSHIELD setup across a set of data repositories.

These `datashield.*` functions are meant **to be used by DataSHIELD packages developers and users.**
  
## Options

Some options can be set to modify the behavior of the DSI:

* `datashield.env` is the R environment in which the `DSConnection` object list is to be looking for. Default value is the Global Environment: `globalenv()`.
* `datashield.progress` is a logical to enable the visibility of the progress bars. Default value is `TRUE`.
* `datashield.progress.clear` is a logical to make the progress bar disappear after it has been completed. Default value is `FALSE`.
* `datashield.errors.stop` is a logical to alter error handling behavior: if `TRUE` an error is raised when at least one server has failed, otherwise a warning message is issued. Default value is `TRUE`.
* `datashield.polling.sleep.1` base time in seconds to wait before checking async calls completion. Default value is 1 second.
* `datashield.polling.sleep.10` time in seconds to wait before checking async calls completion, after ~10 seconds. Default value is x2 the base time (2 seconds).
* `datashield.polling.sleep.60` time in seconds to wait before checking async calls completion, after ~1 minute. Default value is x10 the base time (10 seconds).
* `datashield.polling.sleep.600` time in seconds to wait before checking async calls completion, after ~10 minutes. Default value is x60 the base time (1 minute).
* `datashield.polling.sleep.3600` time in seconds to wait before checking async calls completion, after ~1 hour. Default value is x600 the base time (10 minutes).