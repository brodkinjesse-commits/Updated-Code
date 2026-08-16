# ============================================================================
# yield_curve_simulation.R
#
# Fits a PCA model to historical SA nominal yield curves and simulates
# future yield curves via AR(1)-simulated PC scores + block-bootstrapped
# residuals. Source() this file once from the main retirement project, then
# call simulate_yield_curves() to get back a model you can query for the
# curve at any future (month, simulation) pair - used for discounting /
# bond valuation elsewhere in the model.
#
# Usage from the main project:
#
#   source("yield_curve_simulation.R")
#   yc <- simulate_yield_curves("data/sa_nominal_yield_curves.csv",
#                                horizon_months = 360, n_sims = 200)
#   yc$get_curve(month = 84, sim = 12)   # full curve, 7 years ahead, sim 12
#   yc$get_curve(month = 84, sim = 12)["120M"]   # just the 10Y point
#
# ============================================================================

## ---- block bootstrap (as supplied, unmodified) ----------------------------
moving_block_bootstrap <- function(returns_matrix,
                                    block_size = 6,
                                    horizon = 360,
                                    n_sims = 1000) {

  n_obs = nrow(returns_matrix)
  n_assets = ncol(returns_matrix)
  n_blocks_needed = ceiling(horizon / block_size)

  sims = array(NA_real_, dim = c(horizon, n_assets, n_sims))

  for (s in 1:n_sims) {
    path = matrix(NA_real_, nrow = n_blocks_needed * block_size, ncol = n_assets)

    for (b in 1:n_blocks_needed) {
      start_idx = sample(1:(n_obs - block_size + 1), 1)
      block = returns_matrix[start_idx:(start_idx + block_size - 1), , drop = FALSE]
      rows = ((b - 1) * block_size + 1):(b * block_size)
      path[rows, ] = block
    }

    sims[, , s] = path[1:horizon, ]
  }

  dimnames(sims) = list(NULL, colnames(returns_matrix), NULL)
  sims
}

## ---- construct one curve from a set of PC scores ---------------------------
#' @param pc_scores  numeric vector, length k (scores for the retained PCs)
#' @param mean_curve numeric vector, length N (historical average curve)
#' @param loadings_k N x k matrix of PC loadings for the retained components
#' @param residual   optional length-N vector to add on top (e.g. a
#'                    bootstrapped residual draw)
#' @return named numeric vector, length N: the constructed yield curve (%)
construct_yield_curve <- function(pc_scores, mean_curve, loadings_k, residual = NULL) {
  curve <- as.numeric(mean_curve + loadings_k %*% as.numeric(pc_scores))
  if (!is.null(residual)) curve <- curve + as.numeric(residual)
  names(curve) <- names(mean_curve)
  curve
}

## ---- fit the PCA + AR(1) model and simulate forward ------------------------
#' Fit a yield curve PCA/AR(1) model from history and simulate it forward.
#'
#' @param data_path         CSV of historical yield curves (Date column +
#'                           one column per tenor, headers like "ON","1M",...)
#' @param start_date        only fit the PCA on data from this date onward
#' @param n_pca_components  k, number of PCs to retain (Level/Slope/Curvature = 3)
#' @param block_size        months per residual bootstrap block
#' @param horizon_months    simulation horizon in months (30y = 360, CHANGEABLE)
#' @param n_sims            number of Monte Carlo paths
#' @param seed              RNG seed, for reproducibility
#'
#' @return a list:
#'   get_curve(month, sim, include_residual = TRUE) - function returning the
#'       simulated yield curve (named vector, tenor labels e.g. "120M") at
#'       `month` months ahead (1..horizon_months) under simulation `sim`
#'       (1..n_sims)
#'   mean_curve, loadings_k, ar1_fits, pc_sim, resid_sim, tenors_months,
#'       k, var_explained  - underlying model pieces, for reference/debugging
simulate_yield_curves <- function(data_path,
                                   start_date        = "1994-01-01",
                                   n_pca_components   = 3,
                                   block_size          = 6,
                                   horizon_months      = 360,
                                   n_sims              = 200,
                                   seed                = 2026) {

  start_date <- as.Date(start_date)

  ## ---- 1. load & clean ----
  raw <- read.csv(data_path, stringsAsFactors = FALSE, check.names = FALSE)
  raw$Date <- as.Date(raw$Date)
  raw <- raw[!is.na(raw$Date), ]
  raw <- raw[order(raw$Date), ]
  raw <- raw[raw$Date >= start_date, ]

  tenor_cols <- setdiff(colnames(raw), "Date")
  tenor_months <- numeric(length(tenor_cols))
  is_on <- tenor_cols == "ON"
  tenor_months[is_on]  <- 0
  tenor_months[!is_on] <- as.numeric(sub("M$", "", tenor_cols[!is_on]))
  ord <- order(tenor_months)
  tenor_cols   <- tenor_cols[ord]
  tenor_months <- tenor_months[ord]

  Y <- as.matrix(raw[, tenor_cols])
  storage.mode(Y) <- "double"
  colnames(Y) <- tenor_cols
  rownames(Y) <- as.character(raw$Date)

  if (any(is.na(Y))) {
    stop("Missing values in yield curve matrix from ", start_date, " onward.")
  }

  ## ---- 2. PCA, reconstruction, residuals ----
  pca_fit    <- prcomp(Y, center = TRUE, scale. = FALSE)
  mean_curve <- pca_fit$center
  loadings_k <- pca_fit$rotation[, 1:n_pca_components, drop = FALSE]
  scores_k   <- pca_fit$x[, 1:n_pca_components, drop = FALSE]
  var_explained <- pca_fit$sdev^2 / sum(pca_fit$sdev^2)

  Y_hat     <- sweep(scores_k %*% t(loadings_k), 2, mean_curve, "+")
  resid_mat <- Y - Y_hat

  ## ---- 3. block-bootstrap the residuals forward ----
  set.seed(seed)
  resid_sim <- moving_block_bootstrap(resid_mat, block_size, horizon_months, n_sims)

  ## ---- 4. AR(1) per retained PC, simulated forward with correlated shocks ----
  fit_ar1 <- function(x) {
    n <- length(x)
    fit <- lm(x[2:n] ~ x[1:(n - 1)])
    list(phi0 = unname(coef(fit)[1]), phi1 = unname(coef(fit)[2]),
         resid = unname(residuals(fit)), last_value = unname(x[n]))
  }
  ar1_fits <- setNames(lapply(seq_len(n_pca_components), function(j) fit_ar1(scores_k[, j])),
                        colnames(scores_k))

  phi0  <- vapply(ar1_fits, `[[`, numeric(1), "phi0")
  phi1  <- vapply(ar1_fits, `[[`, numeric(1), "phi1")
  start <- vapply(ar1_fits, `[[`, numeric(1), "last_value")
  E     <- do.call(cbind, lapply(ar1_fits, `[[`, "resid"))
  Sigma <- cov(E)
  L <- tryCatch(t(chol(Sigma)), error = function(e) diag(sqrt(diag(Sigma))))

  set.seed(seed + 1L)
  pc_sim <- array(NA_real_, dim = c(horizon_months, n_pca_components, n_sims),
                   dimnames = list(NULL, colnames(scores_k), NULL))
  for (s in seq_len(n_sims)) {
    prev <- start
    for (t in seq_len(horizon_months)) {
      cur <- phi0 + phi1 * prev + as.numeric(L %*% rnorm(n_pca_components))
      pc_sim[t, , s] <- cur
      prev <- cur
    }
  }

  ## ---- 5. curve-construction accessor ----
  get_curve <- function(month, sim, include_residual = TRUE) {
    curve <- construct_yield_curve(
      pc_scores  = pc_sim[month, , sim],
      mean_curve = mean_curve,
      loadings_k = loadings_k,
      residual   = if (include_residual) resid_sim[month, , sim] else NULL
    )
    curve
  }

  list(
    get_curve      = get_curve,
    mean_curve     = mean_curve,
    loadings_k     = loadings_k,
    ar1_fits       = ar1_fits,
    pc_sim         = pc_sim,
    resid_sim      = resid_sim,
    tenors_months  = tenor_months,
    k              = n_pca_components,
    var_explained  = var_explained,
    horizon_months = horizon_months,
    n_sims         = n_sims
  )
}
plot(yc$mean_curve)

