setClass("fit_transform_step",
         representation(
           fit = "function",
           transform = "function",
           .values = "ANY",
           params = "list",
           .call = "call"
         )
)
fit_transform_step <- function(fit, transform, ...)new("fit_transform_step", fit = fit, transform = transform, .call = match.call(), ...)


setClass("transform_step",
         representation(
           transform = "function",
           params = "list",
           .call = "call"
         )
)
transform_step <- function(transform, ...)new("transform_step", transform = transform, .call = match.call(), ...)