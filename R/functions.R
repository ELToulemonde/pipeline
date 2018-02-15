setGeneric("fit", function(object, X) standardGeneric("fit"))
setGeneric("transform", function(object, X) standardGeneric("transform"))
setGeneric("set_params", function(object, params) standardGeneric("set_params"))
setGeneric("set_params<-", function(object, params) standardGeneric("set_params<-"))
setGeneric("add", function(object, step, step_name) standardGeneric("add"))

.get_used_params <- function(fct, params){
  fcnFormalNames <- names(formals(fct))
  okFcnFormalNames <- intersect(names(params), fcnFormalNames)
  params[okFcnFormalNames]
}

## .control_params
# ----------------
# Control that params is a named list
# @params a named list of params
# @return Nothing everything pass. Stop if params is not in the expected shape
.control_params <- function(params){
  if (!is.list(params)){
    stop("params should be a list")
  }
  if (length(params) > 0){
    if (is.null(names(params)) || any(names(params) == "")){
      stop("params should be a named list")
    }
  }
}