#-------------------------------------------------------------------------------
# Convert the Robust Linear Model into a parsnip model
#-------------------------------------------------------------------------------

library(tidymodels)
str(MASS::rlm)

set_model_engine("linear_reg", "regression", eng = "rlm")
set_dependency("linear_reg", eng = "rlm", pkg = "MASS")

set_fit(
  model = "linear_reg",
  eng = "rlm",
  mode = "regression",
  value = list(
    interface = "formula",
    protect = c("formula", "data", "weights"),
    func = c(pkg = "MASS", fun = "rlm"),
    defaults = list()
  )
)

set_encoding(
  model = "linear_reg",
  eng = "rlm",
  mode = "regression",
  options = list(
    predictor_indicators = "traditional",
    compute_intercept = TRUE,
    remove_intercept = TRUE,
    allow_sparse_x = FALSE
  )
)

set_pred(
  model = "linear_reg",
  eng = "rlm",
  mode = "regression",
  type = "numeric",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args =
      list(
        object = expr(object$fit),
        newdata = expr(new_data),
        type = "response"
      )
  )
)