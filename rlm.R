#-------------------------------------------------------------------------------
# Convert the Robust Linear Model into a parsnip model
#-------------------------------------------------------------------------------

library(tidymodels)
str(MASS::rlm)

# STEP 1. REGISTER THE MODEL, MODES, AND ARGUMENTS
set_new_model("r_linear_reg")
set_model_mode(model = "r_linear_reg", mode = "regression")


set_model_engine("r_linear_reg", mode = "regression", eng = "rlm")
set_dependency("r_linear_reg", eng = "rlm", pkg = "MASS")

show_model_info("r_linear_reg")

set_model_arg(
  model = "r_linear_reg",
  eng = "rlm",
  parsnip = "psi",
  original = "psi",
  func = list(pkg = "foo", fun = "bar"),
  has_submodel = FALSE
)

# STEP 2. CREATE THE MODEL FUNCTION
r_linear_reg <-
  function(mode = "regression",  psi = MASS::psi.huber) {
    # Check for correct mode
    if (mode  != "regression") {
      rlang::abort("`mode` should be 'regression'")
    }
    
    # Capture the arguments in quosures
    args <- list(psi = rlang::enquo(psi))
    
    # Save some empty slots for future parts of the specification
    new_model_spec(
      "r_linear_reg",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = NULL
    )
  }

# STEP 3. ADD A FIT MODULE
set_fit(
  model = "r_linear_reg",
  eng = "rlm",
  mode = "regression",
  value = list(
    interface = "formula",
    protect = c("formula", "data", "psi"),
    func = c(pkg = "MASS", fun = "rlm"),
    defaults = list()
  )
)

set_encoding(
  model = "r_linear_reg",
  eng = "rlm",
  mode = "regression",
  options = list(
    predictor_indicators = "traditional",
    compute_intercept = TRUE,
    remove_intercept = TRUE,
    allow_sparse_x = FALSE
  )
)

# STEP 4. ADD MODULES FOR PREDICTION
set_pred(
  model = "r_linear_reg",
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

show_model_info("r_linear_reg")


# EXAMPLE:
data("iris")
str(iris)

iris_folds <- vfold_cv(iris)

rlm_spec <- r_linear_reg() %>%
  set_engine("rlm")

rlm_rec <-
  recipe(Sepal.Length ~ ., data = iris)

rlm_wf <- 
  workflow() %>%
  add_model(rlm_spec) %>%
  add_recipe(rlm_rec)

rlm_fit <- rlm_wf %>% 
  fit_resamples(
    iris_folds,
    control = control_resamples(save_pred = T)
    )

rlm_fit %>% collect_predictions() %>%
  arrange(.row) %>%
  dplyr::select(.pred, Sepal.Length) %>%
  cor()
