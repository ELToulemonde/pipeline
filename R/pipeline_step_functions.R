## fit_transform_step functions
# -----------------------------
#' Compute fit
#' 
#' Compute fit for \code{\link{fit_transform_step}} and store results
#' @param object a fit_transform_step
#' @param X object to use to fit step
#' @return object fitted.
#' @export
setMethod("fit",signature("fit_transform_step"),function(object, X){
  params = .get_used_params(object@fit, object@params)
  object@.values = do.call(object@fit, args = c(list(X), params))
  object
})


#' Compute transform
#' 
#' Compute transform for \code{\link{fit_transform_step}}
#' @param object a \code{\link{fit_transform_step}} that should be fitted
#' @param X object to transform
#' @return transformed X
#' @export
setMethod("transform","fit_transform_step",function(object, X){
  params = .get_used_params(object@transform, object@params)
  do.call(object@transform, args = c(list(object@.values, X), params))
})

#' Add params
#' 
#' Set params for \code{\link{fit_transform_step}}
#' @param object a \code{\link{fit_transform_step}}
#' @param params a named list
#' @return object with added params
#' @export
setMethod("set_params", "fit_transform_step", function(object, params){
  ## Sanity check
  .control_params(params)
  
  ## Initialization
  already_set = names(object@params)[names(params) %in% names(object@params)]
  if (length(already_set) > 0){
    print(paste("The following params were already set: ", paste(already_set, collapse = ", "), ". Changing them."))
  }
  ## Computation
  object@params = c(object@params[! names(params) %in% names(object@params)], params)
  
  ## Return
  object
})

#' as.character
#' 
#' Transform a \code{\link{fit_transform_step}} to a character
#' @param object a \code{\link{fit_transform_step}}
#' @return A printable character
#' @export
as.character.fit_transform_step <- function(object){
  paste0("This transform step was built with following code: ", deparse(object@.call), "\n",
         ifelse(is.null(object@.values), "It as not been fitted yet.", "It is fitted."), "\n",
                ifelse(length(object@params) == 0, "No params are set.", paste0("It has the following params:\n", deparse(object@params)))
  )
}


#' print
#' 
#' Print a \code{\link{fit_transform_step}}
#' @param object a \code{\link{fit_transform_step}}
#' @export
print.fit_transform_step <- function(object){
  cat(as.character(object))
}

## transform_step functions
# -------------------------
#' Compute fit
#' 
#' Compute fit for \code{\link{transform_step}} and store results
#' @param object a transform_step
#' @param X object to use to fit step
#' @return same object without fit
#' @export
setMethod("fit",signature("transform_step"),function(object, X){
  object
})


setMethod("transform","transform_step",function(object, X){
  params = .get_used_params(object@transform, object@params)
  do.call(object@transform, args = c(list(X), params))
})


setMethod("set_params", "transform_step", function(object, params){
  ## Sanity check
  .control_params(params)
  
  ## Initialization
  already_set = names(object@params)[names(params) %in% names(object@params)]
  if (length(already_set) > 0){
    print(paste("The following params were already set: ", paste(already_set, collapse = ", "), ". Changing them."))
  }
  ## Computation
  object@params = c(object@params[! names(params) %in% names(object@params)], params)
  
  ## Return
  object
})

as.character.transform_step <- function(object){
  paste0("This transform step was built with following code: ", deparse(object@.call), "\n",
         ifelse(length(object@params) == 0, "No params are set", paste0("It has the following params:\n", deparse(object@params))))
}


print.transform_step <- function(object){
  cat(as.character(object))
}