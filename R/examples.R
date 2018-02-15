require(dataPreparation)
require(data.table)

# pipeline_step transform should take as first argument params that have been learned at fit and as second argument the element to be transformed ( add then ... for optional argument if needed)
# pipeline_step fit should take as first agrument the object from which to learn and which to be transformed at transform
scale <- fit_transform_step(fit = build_scales, transform = function(x, params,...)fastScale(params, x,...))
binarize <- fit_transform_step(fit = build_bins, transform = function(x, params,...)fastDiscretization(params, x,...))
encode <- fit_transform_step(fit = build_encoding, transform = function(x, params,...)one_hot_encoder(params, x,...))
drop_age <- transform_step( transform = function(x){x$age = NULL; x}, params = list(verbose = F))

binarize = set_params(binarize, list(verbose = FALSE))
binarize <- fit(binarize, adult)
tmp <- transform(binarize, adult)

scale = set_params(scale, list(verbose = FALSE))
scale <- fit(scale, adult)
tmp <- transform(scale, adult)


my_pipeline <- pipeline()
my_pipeline <- add(my_pipeline, drop_age)
my_pipeline <- add(my_pipeline, scale)
my_pipeline <- add(my_pipeline, binarize)
my_pipeline <- add(my_pipeline, encode)
my_pipeline <- set_params(my_pipeline, list(verbose = TRUE, binarize__n_bins = 2))
data("adult")

I_train = sample(1:nrow(adult), 0.75 * nrow(adult))
I_test = (1:nrow(adult))[! 1:nrow(adult) %in% I_train]

adult_train = adult[I_train, ]
adult_test = adult[I_test, ]

my_pipeline <- fit(my_pipeline, copy(adult_train))

adult_train <- transform(my_pipeline, adult_train)
adult_test <- transform(my_pipeline, adult_test)

identical(names(adult_train), names(adult_test))
identical(sapply(adult_train, class), sapply(adult_test, class))
