as.character.pipeline <- function(object){
  text = ""
  for (step_name in names(object@steps)){
    
    text = paste0(text, "\n",
                 "STEP: ", step_name, ":\n",
                 paste(rep(x = "=", times = nchar(paste0("STEP: ", step_name, ":"))), collapse = ""), "\n",
                 as.character(object@steps[[step_name]]),  "\n")
  }
  text
}

print.pipeline <- function(object){
  cat(as.character(object))
}
setMethod("print","pipeline",function(object){
  cat(as.character(object))
})


setMethod("add","pipeline",function(object, step, step_name){
  if (! .is.pipeline_step(step)){
    stop("step should be a pipeline step")
  }
  tmp = list(step)
  if (missing(step_name)){
    step_name <- as.character(as.list(match.call())$step)
  }
  names(tmp) <- step_name
  object@steps = c(object@steps, tmp)
  object
})



setMethod("set_params","pipeline",function(object, params){
  .control_params(params)
  for (i in 1:length(params)){
    object <- .dispach_param(object, params[i])
  }
  object@params = c(object@params, params)
  object
})

setMethod("fit",signature("pipeline"),function(object, X){
  for (i in 1:length(object@steps)){
    object@steps[[i]] = fit(object@steps[[i]], X)
    X = transform(object@steps[[i]], X)
  }
  object
})

setMethod("transform", signature("pipeline"), function(object, X){
  n_steps = length(object@steps)
  for (step_index in 1:n_steps){
    X = transform(object@steps[[step_index]], X)
  }
  X
})

.dispach_param <- function(object, param){
  splitted_name = unlist(strsplit(names(param), "__"))
  if (length(splitted_name) == 1){ # Apply params to all
    for (step_name in names(object@steps)){
      #object@steps <- replace(object@steps, step_name == names(object@steps), set_params(object@steps[[step_name]], param))
      object@steps[[step_name]] <- set_params(object@steps[[step_name]], param)
    }
  }
  if (length(splitted_name) == 2){ # Apply param to corresponding step
    corresponding_step = which(splitted_name[1] == names(object@steps))
    if (length(corresponding_step) > 0){
      names(param) <- splitted_name[2] 
      #object@steps <- replace(object@steps, corresponding_step, set_params(object@steps[[corresponding_step]], param))
      object@steps[[corresponding_step]] <- set_params(object@steps[[corresponding_step]], param)
    }
    else{print(paste0(names(param), ": doesn't correspond to any step. Ignored param."))}
  }
  object
}