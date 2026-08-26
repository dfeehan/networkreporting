##' Fast bootstrap estimation using matrix multiplication
##'
##' Replaces the wide-dataframe summarize_at + gather/spread approach with direct
##' matrix multiplication. For each cell, computes weighted sums across all M bootstrap
##' replicates simultaneously using BLAS routines, avoiding the creation of 10k-column
##' intermediate dataframes.
##'
##' @param ec_dat ego X cell data from get_ec_reports()
##' @param boot_weights_df dataframe with .ego.id column and boot_weight_1..M columns
##' @param ego_id_col name of the ego id column in ec_dat and boot_weights_df
##' @param cell_vars vector of column names defining cells (age, sex, time period, etc)
##' @param estimator_type either 'ind' (individual visibility) or 'agg' (aggregate visibility)
##' @return long-form data frame with one row per cell per bootstrap replicate
##' @export
get_boot_ests_matrix <- function(ec_dat, boot_weights_df, ego_id_col, cell_vars, estimator_type) {

  # Build boot weight matrix: rows = respondents, cols = bootstrap replicates
  boot_col_names <- stringr::str_subset(colnames(boot_weights_df), 'ego.id', negate = TRUE)
  boot_mat <- as.matrix(boot_weights_df[, boot_col_names, drop = FALSE])
  boot_ego_ids <- boot_weights_df[[ego_id_col]]
  M <- ncol(boot_mat)

  # Split ec_dat by cell for vectorized operations within each cell
  cell_groups <- ec_dat %>% dplyr::group_by(dplyr::across(dplyr::all_of(cell_vars))) %>% dplyr::group_split()
  cell_keys   <- ec_dat %>% dplyr::group_by(dplyr::across(dplyr::all_of(cell_vars))) %>% dplyr::group_keys()

  purrr::map2_dfr(cell_groups, seq_len(nrow(cell_keys)), function(grp, i) {
    # Match respondents in this cell to rows in the boot weight matrix
    row_idx <- match(grp[[ego_id_col]], boot_ego_ids)

    # Rows from boot_mat corresponding to respondents in this cell
    W <- boot_mat[row_idx, , drop = FALSE]  # N_cell x M

    # Select numerator and denominator vectors based on estimator type
    if (estimator_type == 'ind') {
      num_vec   <- grp$y.Dcell.ind
      denom_vec <- grp$y.Ncell.ind
    } else {
      num_vec   <- grp$y.Dcell
      denom_vec <- grp$y.Ncell
    }

    # Matrix multiply: length-N_cell vector %*% N_cell x M matrix = length-M vector
    # This uses BLAS and runs in milliseconds even for large M
    num.hat   <- as.vector(num_vec   %*% W)
    denom.hat <- as.vector(denom_vec %*% W)

    estimator_label <- if (estimator_type == 'ind') 'sib_ind' else 'sib_agg'

    data.frame(
      cell_keys[rep(i, M), , drop = FALSE],
      boot_idx  = seq_len(M),
      num.hat   = num.hat,
      denom.hat = denom.hat,
      asdr.hat  = num.hat / denom.hat,
      estimator = estimator_label,
      stringsAsFactors = FALSE
    )
  })
}

##' helper function for calculating individual visibility estimate from ego X cell data
##'
##' @param ec_dat the ego X cell data
##' @param wgt_var either a string with the name of the column that has sampling weights or a vector with the names of columns with bootstrap weights
##' @param cell_vars vector of strings with the names of variables to group by (the cells)
##' @return a tibble with the individual visibility ASDR estimates (not including the respondents' exposures)
##' @export
get_ind_est_from_ec <- function(ec_dat, wgt_var, cell_vars) {

  res <- ec_dat %>%
    dplyr::mutate(ind.num.ego   = y.Dcell.ind,
                  ind.denom.ego = y.Ncell.ind)

  weighted_sum <- function(x, w) { return(sum(x*w)) }

  res2 <- res %>%
    group_by(across(all_of(cell_vars))) %>%
    summarize(num.hat   = weighted_sum(x = ind.num.ego,   w = .data[[wgt_var]]),
              denom.hat = weighted_sum(x = ind.denom.ego,  w = .data[[wgt_var]]),
              ind.y.F   = weighted_sum(x = y.F,            w = .data[[wgt_var]]),
              n         = n(),
              wgt.sum   = weighted_sum(x = 1,              w = .data[[wgt_var]]),
              .groups   = "drop")

  ## if we have bootstrap weights, reshape and clean things up
  if(length(wgt_var) > 1) {
    res3 <- res2 %>%
      tidyr::pivot_longer(cols = tidyselect::starts_with('boot_weight'),
                          names_to = 'rawqty',
                          values_to = 'value') %>%
      mutate(qty = stringr::str_remove(rawqty, 'boot_weight_\\d+_'),
             boot_idx = as.integer(stringr::str_remove_all(rawqty, '[^\\d]'))) %>%
      select(-rawqty) %>%
      tidyr::pivot_wider(names_from = qty, values_from = value)
  } else {
    res3 <- res2
  }

  res4 <- res3 %>%
    dplyr::mutate(asdr.hat = num.hat / denom.hat,
                  estimator='sib_ind')

  return(res4)
}

##' helper function for calculating aggregate visibility estimate from ego X cell data
##'
##' @param ec_dat the ego X cell data
##' @param wgt_var either a string with the name of the column that has sampling weights or a vector with the names of columns with bootstrap weights
##' @param cell_vars vector of strings with the names of variables to group by (the cells)
##' @return a tibble with the individual visibility ASDR estimates (not including the respondents' exposures)
##' @export
get_agg_est_from_ec <- function(ec_dat, wgt_var, cell_vars) {

  weighted_sum <- function(x, w) { return(sum(x*w)) }

  res <- ec_dat %>%
    #dplyr::mutate(.cur.weight = !!sym(wgt_var)) %>%
    group_by(across(all_of(cell_vars))) %>%
    summarize(num.hat   = weighted_sum(x = y.Dcell, w = .data[[wgt_var]]),
              denom.hat = weighted_sum(x = y.Ncell,  w = .data[[wgt_var]]),
              n         = n(),
              wgt.sum   = weighted_sum(x = 1,        w = .data[[wgt_var]]),
              .groups   = "drop")

  ## if we have bootstrap weights, reshape and clean things up
  if(length(wgt_var) > 1) {
    res2 <- res %>%
      tidyr::pivot_longer(cols = tidyselect::starts_with('boot_weight'),
                          names_to = 'rawqty',
                          values_to = 'value') %>%
      mutate(qty = stringr::str_remove(rawqty, 'boot_weight_\\d+_'),
             boot_idx = as.integer(stringr::str_remove_all(rawqty, '[^\\d]'))) %>%
      select(-rawqty) %>%
      tidyr::pivot_wider(names_from = qty, values_from = value)
  } else {
    res2 <- res
  }

  res3 <- res2 %>%
    mutate(asdr.hat = num.hat / denom.hat,
           estimator='sib_agg')

  return(res3)
}
