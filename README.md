# DataSHIELD Interface

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

## Higher Level Functions

In addition to these S4 classes, DSI provides functions to handle a list of remote data repository servers:

* `datashield.login` and `datashield.logout` will make use of the `DSDriver` paradigm to create `DSConnection`s
to the data repositories,
* `datashield.aggregate` and `datashield.assign` will perform typical DataSHIELD operations on `DSConnection`s, 
which result will be fetched through `DSResult` objects,
* Other data management functions are provided by the `DSConnection` objects:
  * `datashield.workspaces`, `datashield.workspace_save` and `datashield.workspace_rm` allow to manage R images 
  of the remote DataSHIELD sessions (to speed up data analysis sessions),
  * `datashield.symbols` and `datashield.symbol_rm` offer a minimalistic management of the R symbols living in 
  the remote DataSHIELD sessions,
  * `datashield.table_status`, `datashield.pkg_status`, `datashield.method_status` and `datashield.methods` are 
  utility functions to explore the DataSHIELD setup across a set of data repositories.
