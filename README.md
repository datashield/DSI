# DataSHIELD Interface

The DataSHIELD Interface (DSI) defines a set of S4 classes and generic methods that can be implemented 
for accessing a data repository supporting the DataSHIELD infrastructure: controlled R commands to be 
executed on the server side and garanteeing that non disclosive information is returned to client side.

Learn more about [DataSHIELD](http://www.datashield.ac.uk/).

The DSI classes are:

* **DSDriver** drives the creation of a connection object,
* **DSConnection** allows the interaction with the remote server; DataSHIELD operations such as 
aggregation and assignment return a result object,
* **DSResult** wraps access to the result, which can be accessed either synchronously or asynchronously 
depending on the capabilities of the data repository server.

See [DSOpal](https://github.com/datashield/DSOpal) for a reference implementation of DSI based on the 
[Opal](https://www.obiba.org/pages/products/opal/) data repository.
