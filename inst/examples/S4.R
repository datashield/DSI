# http://adv-r.had.co.nz/S4.html

library(methods)
setClass("Person", representation(name = "character", age = "numeric"))
setClass("Employee", representation(boss = "Person"), contains = "Person")
hadley <- new("Person", name = "Hadley", age = 31)
hadley@age
names(hadley)

sides <- function(object) 0
setGeneric("sides")
setGeneric("sides", function(object) {
  standardGeneric("sides")
})

setClass("Shape")
setClass("Polygon", representation(sides = "integer"), contains = "Shape")
setClass("Triangle", contains = "Polygon")
setClass("Square", contains = "Polygon")
setClass("Circle", contains = "Shape")

setMethod("sides", signature(object = "Polygon"), function(object) {
  object@sides
})
setMethod("sides", signature("Triangle"), function(object) 3)
setMethod("sides", signature("Square"),   function(object) 4)
setMethod("sides", signature("Circle"),   function(object) Inf)

sides(new("Triangle"))
showMethods("sides")

setClass("A")
setClass("A1", contains = "A")
setClass("A2", contains = "A1")
setClass("A3", contains = "A2")

setGeneric("foo", function(a, b) standardGeneric("foo"))
setMethod("foo", signature("A1", "A2"), function(a, b) "1-2")
setMethod("foo", signature("A2", "A1"), function(a, b) "2-1")

foo(new("A2"), new("A2"))

setGeneric("type", function(x) standardGeneric("type"))
setMethod("type", signature("matrix"), function(x) "matrix")
setMethod("type", signature("character"), function(x) paste0("character: ", x))
setMethod("type", signature("list"), function(x) lapply(x, function(o) type(o)))

type("letters")
type(matrix(letters, ncol = 2))
type(list(a="1", b="2"))

setGeneric("default", function(x, y=NULL) standardGeneric("default"))
setMethod("default", signature("character"), function(x, y) paste0("character: ", x, " + ", y))

default("toto", "tutu")
default("toto")
