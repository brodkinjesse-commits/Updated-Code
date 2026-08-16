moving_block_bootstrap = function(returns_matrix,
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
