.check_pipeline <- function(object){
  errors <- NULL
  for (elt in object@steps){
    if (! .is.pipeline_step(elt)){
      errors <- c(errors, paste0(elt, ": isn't a fit_transform_step nor a transform_step."))
    }  
  }
  if (length(object@params) > 0){
    if (is.null(names(object@params)) || any(names(object@params) == "")){
      errors <- c(errors, "Params should have names")
    }
  }
  if (length(errors) == 0) TRUE else errors
}

.is.pipeline_step <- function(object){
  is(object, "fit_transform_step") || is(object, "transform_step")
}
setClass("pipeline",
         representation(
           steps = "list",
           params = "list"
         ),
         validity = .check_pipeline
)
pipeline <- function(steps = list())new("pipeline", steps = steps)
