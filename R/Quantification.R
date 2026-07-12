#' Prepare quantification labels
#' @noRd 
prepare_quantification_labels <- function(group,
                                          factor_levels = NULL,
                                          expect_numeric = NULL) {
  group_character <- trimws(as.character(group))

  if (anyNA(group_character) || any(group_character == "")) {
    stop("`group` contains missing or empty values.")
  }

  numeric_values <- suppressWarnings(as.numeric(group_character))
  is_numeric_group <- all(!is.na(numeric_values))

  if (!is_numeric_group) {
    numeric_pattern <- paste0(
      "^[^0-9]*(",
      "(?:\\d*\\.\\d+|\\d+\\.?\\d*)(?:[eE][-+]?\\d+)?",
      ")[^0-9]*$"
    )

    extracted_values <- stringr::str_match(
      group_character,
      numeric_pattern
    )[, 2]

    extracted_numeric <- suppressWarnings(as.numeric(extracted_values))

    if (all(!is.na(extracted_numeric))) {
      numeric_values <- extracted_numeric
      is_numeric_group <- TRUE
    }
  }

  if (!is.null(expect_numeric) && is_numeric_group != expect_numeric) {
    stop("Training and test `group` must use the same label type.")
  }

  if (is_numeric_group) {
    return(list(
      values = numeric_values,
      is_numeric = TRUE,
      levels = NULL
    ))
  }

  if (is.null(factor_levels)) {
    factor_levels <- if (is.factor(group)) levels(group) else unique(group_character)
  }

  factor_values <- factor(group_character, levels = factor_levels)

  if (anyNA(factor_values)) {
    stop("Test `group` contains levels absent from the training data.")
  }

  list(
    values = as.numeric(factor_values),
    is_numeric = FALSE,
    levels = factor_levels
  )
}

#' Partial Least Squares (PLS)
#'
#' A regression method that projects predictors and responses to latent variables to maximize covariance.
#'
#' @param train The training Ramanome object
#' @param test The test data object (optional). If not provided, the function will perform a stratified cross-validation (Training : Test = 7 : 3).
#' @param n_comp The number of latent components that the PLS model extract from the training data to model the response variable
#' @param show Whether to show the plot of the regression results (Prediction vs True)
#' @param save Whether to save the plot of the results (default path: getwd())
#' @param seed The seed for the random number generator
#' 
#' @return A list containing:
#' \describe{ 
#'   \item{model}{The PLS model}
#'   \item{pred_test}{The prediction for test data if test is provided}
#' }  
#' @export Quantification.Pls
#' @importFrom pls plsr
#' @importFrom stringr str_extract
#' @examples
#' data(RamEx_data)
#' data_processed <- Preprocessing.OneStep(RamEx_data)
#' quan_pls <- Quantification.Pls(data_processed)
Quantification.Pls <- function(train, test = NULL,n_comp = 8, show = TRUE, save = FALSE, seed = 42) {
  set.seed(seed)
  train_label_info <- prepare_quantification_labels(train@meta.data$group)
  if (is.null(test)) {
    data_set <- get.nearest.dataset(train)
    labels <- train_label_info$values
    index <- stratified_partition(labels, p = 0.7)
    data_train <- data_set[index,]
    label_train <- as.numeric(labels[index])
    data_val <- data_set[-index,]
    label_val <- labels[-index]
  } else {
    data_train <- get.nearest.dataset(train)
    label_train <- train_label_info$values
    data_val <- get.nearest.dataset(test)
    test_label_info <- prepare_quantification_labels( test@meta.data$group, factor_levels = train_label_info$levels, expect_numeric = train_label_info$is_numeric)
    label_val <- test_label_info$values
  }

  pls_model <- plsr(label_train ~ data_train, ncomp = n_comp, scale = TRUE, validation = 'none')
  pre_result <- predict(pls_model, data_val, ncomp = n_comp)
  if(show | save){
    p1 <- Plot.scatter(label_train, predict(pls_model, data_train, ncomp = n_comp), cols = label_train)
    p2 <- Plot.scatter(label_val, pre_result, cols = label_val)
    if(show){print(p1)
      print(p2)}  
    if(save){
      cat('Saving plot to the current working directory: ', getwd(), '\n')
      ggsave('Quantification_Pls_Train.png', p1, width = 10, height = 10)
      ggsave('Quantification_Pls_Test.png', p2, width = 10, height = 10)
    }
  }
  if (is.null(test)) return(list(model=pls_model)) else return(list(model=pls_model, pred_test = pre_result))
}

#' Multiple linear regression (MLR)
#' A linear approach modeling the relationship between multiple predictors and a response variable via linear coefficients.
#' 
#' @param train The training data object
#' @param test The test data object (optional). If not provided, the function will perform a stratified cross-validation (Training : Test = 7 : 3).
#' @param n_pc The number of principal components that the PCA model extract for further MLR model building. Feature extraction is recommended to reduce the multicollinearity among predictors.
#' @param show Whether to show the plot of the regression results (Prediction vs True)
#' @param save Whether to save the plot of the results (default path: getwd())
#' @param seed The seed for the random number generator
#' 
#' @return A list containing:
#' \describe{ 
#'   \item{model}{The MLR model}
#'   \item{pred_test}{The prediction for test data if test is provided}
#'   \item{pca_params}{The pre-feature extraction PCA parameters used for the MLR model building}
#' }  
#' @export Quantification.Mlr
#' @importFrom stats lm
#' @importFrom stringr str_extract
#' @examples
#' data(RamEx_data)
#' data_processed <- Preprocessing.OneStep(RamEx_data)
#' quan_mlr <- Quantification.Mlr(data_processed)
Quantification.Mlr <- function(train, test = NULL, n_pc = 20,show = TRUE, save = FALSE, seed = 42) {
  set.seed(seed)
  train_label_info <- prepare_quantification_labels(train@meta.data$group)
  if (is.null(test)) {
    data_set <- get.nearest.dataset(train)
    labels <- train_label_info$values
    index <- stratified_partition(labels, p = 0.7)
    data_train <- data_set[index,]
    label_train <- labels[index]
    data_val <- data_set[-index,]
    label_val <- labels[-index]
  } else {
    data_train <- get.nearest.dataset(train)
    label_train <- train_label_info$values
    data_val <- get.nearest.dataset(test)
    test_label_info <- prepare_quantification_labels( test@meta.data$group, factor_levels = train_label_info$levels, expect_numeric = train_label_info$is_numeric)
    label_val <- test_label_info$values
  }
  
  data.pca <- prcomp(data_train, scale = TRUE, retx = TRUE)
  data_20 <- scale(data_train, center = data.pca$center, scale = data.pca$scale) %*% data.pca$rotation[, 1:n_pc] %>% as.data.frame
  test_20 <- scale(data_val, center = data.pca$center, scale = data.pca$scale) %*% data.pca$rotation[, 1:n_pc] %>% as.data.frame
  
  mlr_model <- lm(label_train~ ., data = data_20)
  pre_result <- predict(mlr_model, test_20)
  if(show | save){
    p1 <- Plot.scatter(label_train, predict(mlr_model, data_20), cols = label_train)
    p2 <- Plot.scatter(label_val, pre_result, cols = label_val)
    if(show){print(p1)
      print(p2)}  
    if(save){
      cat('Saving plot to the current working directory: ', getwd(), '\n')
      ggsave('Quantification_Mlr_Train.png', p1, width = 10, height = 10)
      ggsave('Quantification_Mlr_Test.png', p2, width = 10, height = 10)
    }
  }
  pca_params <- list(
    center = data.pca$center,
    scale = data.pca$scale,
    rotation = data.pca$rotation[, 1:n_pc]
  )
  if (is.null(test)) return(list(model=mlr_model, pca_params = pca_params)) else return(list(model=mlr_model, pred_test = pre_result, pca_params = pca_params))
}

#' Generalized linear model (GLM)
#' Extends linear regression by allowing response variables to follow non-normal distributions via link functions.
#'
#' @param train The training data object
#' @param test The test data object (optional). If not provided, the function will perform a stratified cross-validation (Training : Test = 7 : 3).
#' @param n_pc The number of principal components that the PCA model extract for further GLM model building. Feature extraction is recommended to reduce the multicollinearity among predictors.
#' @param show Whether to show the plot of the regression results (Prediction vs True)
#' @param save Whether to save the plot of the results (default path: getwd())
#' @param seed The seed for the random number generator
#' 
#' @return A list containing:
#' \describe{ 
#'   \item{model}{The GLM model}
#'   \item{pred_test}{The prediction for test data if test is provided}
#'   \item{pca_params}{The pre-feature extraction PCA parameters used for the GLM model building}
#' }  
#' @export Quantification.Glm
#' @importFrom stats glm
#' @importFrom stringr str_extract
#' @examples
#' data(RamEx_data)
#' data_processed <- Preprocessing.OneStep(RamEx_data)
#' quan_glm <- Quantification.Glm(data_processed)
Quantification.Glm <- function(train, test = NULL, n_pc = 20, show = TRUE, save = FALSE, seed = 42) {
  set.seed(seed)
  train_label_info <- prepare_quantification_labels(train@meta.data$group)
  if (is.null(test)) {
    data_set <- get.nearest.dataset(train)
    labels <- train_label_info$values
    index <- stratified_partition(labels, p = 0.7)
    data_train <- data_set[index,]
    label_train <- labels[index]
    data_val <- data_set[-index,]
    label_val <- labels[-index]
  } else {
    data_train <- get.nearest.dataset(train)
    label_train <- train_label_info$values
    data_val <- get.nearest.dataset(test)
    test_label_info <- prepare_quantification_labels( test@meta.data$group, factor_levels = train_label_info$levels, expect_numeric = train_label_info$is_numeric)
    label_val <- test_label_info$values
  }
  
  data.pca <- prcomp(data_train, scale = TRUE, retx = TRUE)
  data_20 <- scale(data_train, center = data.pca$center, scale = data.pca$scale) %*% data.pca$rotation[, 1:n_pc] %>% as.data.frame
  test_20 <- scale(data_val, center = data.pca$center, scale = data.pca$scale) %*% data.pca$rotation[, 1:n_pc] %>% as.data.frame
  
  glm_model <- glm(label_train~ ., data_20, family = gaussian())
  pre_result <- predict(glm_model, test_20,  type = "response")
  pca_params <- list(
    center = data.pca$center,
    scale = data.pca$scale,
    rotation = data.pca$rotation[, 1:n_pc]
  )
  if(show | save){
    p1 <- Plot.scatter(label_train, predict(glm_model, data_20), cols = label_train)
    p2 <- Plot.scatter(label_val, pre_result, cols = label_val)
    if(show){print(p1)
      print(p2)}  
    if(save){
      cat('Saving plot to the current working directory: ', getwd(), '\n')
      ggsave('Quantification_Glm_Train.png', p1, width = 10, height = 10)
      ggsave('Quantification_Glm_Test.png', p2, width = 10, height = 10)
    }
  }
  if (is.null(test)) return(list(model=glm_model, pca_params = pca_params)) else return(list(model=glm_model, pred_test = pre_result, pca_params = pca_params))
}


#' Predict using a saved quantification model
#'
#' This function uses a saved quantification model to predict values for new data.
#'
#' @param model The saved quantification model (from Quantification.Pls, Quantification.Mlr, or Quantification.Glm)
#' @param new_data The new data object to predict
#' @param show Whether to show the plot of the results
#' @param save Whether to save the plot of the results (default path: getwd())
#'
#' @return A list containing:
#' \describe{
#'   \item{predictions}{The predicted values for the new data}
#' }
#'
#' @export
#'
#' @examples
#' data(RamEx_data)
#' # Train a model
#' model <- Quantification.Pls(RamEx_data)
#' # Use the model to predict new data
#' predictions <- predict_quantification(model, RamEx_data)
predict_quantification <- function(model, new_data) {
  # Get the new data
  new_data_matrix <- get.nearest.dataset(new_data)
  
  # Determine model type and make predictions
  if (inherits(model$model, "mvr")) {
    # For PLS model
    pred <- predict(model$model, new_data_matrix, ncomp = model$model$ncomp)
    predictions <- pred
    
  } else if (inherits(model$model, "lm")) {
    # For MLR model
    new_data_20 <- scale(new_data_matrix, center = model$pca_params$center, scale = model$pca_params$scale) %*% model$pca_params$rotation %>% as.data.frame
    predictions <- predict(model$model, new_data_20)
    
  } else if (inherits(model$model, "glm")) {
    # For GLM model
    new_data_20 <- scale(new_data_matrix, center = model$pca_params$center, scale = model$pca_params$scale) %*% model$pca_params$rotation %>% as.data.frame
    predictions <- predict(model$model, new_data_20, type = "response")
    
  } else {
    stop("Unsupported model type")
  }
  
  return(list(predictions = predictions))
}

