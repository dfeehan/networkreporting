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
##' @param visibility optional [visibility_rule]. If it is `is_estimated`, the
##'        rule is refit inside every replicate rather than frozen; see Details
##' @param refit optional `function(replicate_index)` returning the group size
##'        `S.hat` for each row of `ec_dat` under that replicate. Supplied by
##'        [apply_visibility_rule()]-aware callers for an estimated rule whose
##'        visibility is constant within a cell
##' @param refit_sums optional `function(replicate_index)` returning a data frame
##'        of `num` and `denom`, one row per row of `ec_dat`, for an estimated
##'        rule whose visibility is *not* constant within a cell. Takes
##'        precedence over `refit`; see [make_vis_refit_esc()]
##' @return long-form data frame with one row per cell per bootstrap replicate
##'
##' @section Details:
##' Visibility is normally baked into `y.Dcell.ind` and `y.Ncell.ind` at
##' [get_ec_reports()] time, which freezes it across replicates. For
##' [vis_from_clique()] that is *correct*: visibility is a function of ego's own
##' reports, not of which egos happened to be sampled.
##'
##' For any rule with `is_estimated = TRUE` it is wrong. The estimated group
##' size is a sample quantity, and holding it fixed understates the variance. So
##' when such a rule is passed, each replicate recomputes the estimate from the
##' frame-split sufficient statistics that [get_ec_reports()] already produces:
##'
##' \deqn{num = y.DandFcell / (S - 1) + y.DandnotFcell / S}
##' \deqn{denom = y.NandFcell / (S - 1) + y.NandnotFcell / S}
##'
##' which needs one length-M vector per cell and no per-alter recomputation.
##' This holds whenever the estimated visibility is constant within a cell,
##' which is the common case, since matching on alter sex and age group means
##' matching on the cells themselves.
##' @export
get_boot_ests_matrix <- function(ec_dat, boot_weights_df, ego_id_col, cell_vars,
                                 estimator_type,
                                 visibility = NULL,
                                 refit      = NULL,
                                 refit_sums = NULL) {

  ## Is visibility being re-estimated inside the replicate loop?
  reestimate <- !is.null(visibility) && isTRUE(visibility$is_estimated) &&
                estimator_type == 'ind'

  ## refit_sums is the expensive path: visibility recomputed per report rather
  ## than per cell, for a rule whose prediction is not constant within a cell.
  use_sums <- reestimate && !is.null(refit_sums)

  if (reestimate && is.null(refit) && is.null(refit_sums)) {
    warning("visibility rule '", visibility$label, "' is estimated from the ",
            "sample, but no refit function was supplied, so visibility is ",
            "frozen across bootstrap replicates. The resulting intervals are ",
            "too narrow. This is the bug that is.estimated exists to prevent; ",
            "pass refit = to fix it.")
    reestimate <- FALSE
  }

  # Build boot weight matrix: rows = respondents, cols = bootstrap replicates
  boot_col_names <- stringr::str_subset(colnames(boot_weights_df), 'ego.id', negate = TRUE)
  boot_mat <- as.matrix(boot_weights_df[, boot_col_names, drop = FALSE])
  boot_ego_ids <- boot_weights_df[[ego_id_col]]
  M <- ncol(boot_mat)

  ## Row identity has to survive the split. The refit function returns one value
  ## per row of ec_dat, and the cell groups are subsets of those rows, so the
  ## group needs to know WHICH rows it holds. Without this the refit vector was
  ## indexed by bootstrap-weight row position instead, which silently handed
  ## every cell the same few rows' values -- invisible when the estimated
  ## visibility is constant, wrong as soon as it varies by cell.
  ec_dat <- ec_dat %>% dplyr::mutate(.ec.row = dplyr::row_number())

  ## Refit once per replicate, not once per replicate per cell.
  S_by_rep <- NULL
  sums_by_rep <- NULL
  if (use_sums) {
    sums_by_rep <- lapply(seq_len(ncol(boot_mat)), function(r) refit_sums(r))
  } else if (reestimate) {
    S_by_rep <- lapply(seq_len(ncol(boot_mat)), function(r) refit(r))
  }

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

    if (!reestimate) {
      # Matrix multiply: length-N_cell vector %*% N_cell x M matrix = length-M vector
      # This uses BLAS and runs in milliseconds even for large M
      num.hat   <- as.vector(num_vec   %*% W)
      denom.hat <- as.vector(denom_vec %*% W)
    } else {
      ## Visibility is a sample quantity here, so it moves with the replicate.
      ## Recompute the visibility-adjusted sums per replicate from the
      ## frame-split statistics, keeping the on-frame / off-frame asymmetry
      ## that is the only way visibility survives into a ratio.
      num.hat   <- numeric(M)
      denom.hat <- numeric(M)
      for (r in seq_len(M)) {

        if (use_sums) {
          ## visibility was recomputed per report and re-aggregated, so the
          ## adjusted sums are already here; just weight them
          sr <- sums_by_rep[[r]]
          num.hat[r]   <- sum(sr$num[grp$.ec.row]   * W[, r])
          denom.hat[r] <- sum(sr$denom[grp$.ec.row] * W[, r])
          next
        }

        ## index by ec_dat row, which is what refit() is aligned to
        S <- S_by_rep[[r]][grp$.ec.row]
        ## S - 1 is the visibility of an on-frame alter; S that of an off-frame
        ## one. A group of size 1 has no on-frame alters to divide, so guard it.
        S_on <- ifelse(S > 1, S - 1, NA_real_)
        num_r   <- grp$y.DandFcell / S_on + grp$y.DandnotFcell / S
        denom_r <- grp$y.NandFcell / S_on + grp$y.NandnotFcell / S
        num_r[is.na(num_r)]     <- 0
        denom_r[is.na(denom_r)] <- 0
        num.hat[r]   <- sum(num_r   * W[, r])
        denom.hat[r] <- sum(denom_r * W[, r])
      }
    }

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
