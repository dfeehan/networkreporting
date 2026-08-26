##' get a dataset with reports used for internal-consistency checks
##'
##' @param esc.dat The ego X sibling X cell dataset (see [get_esc_reports])
##' @param ego.dat The ego dataset, containing one row for each survey respondent
##' @param ego.id  String with the name of the column in \code{esc.dat} containing the survey respondent ID
##' @param sib.id  String with the name of the column in \code{esc.dat} containing the unique sibling ID
##' @param sib.frame.indicator String with the name of the column in \code{sib.dat} containing a 0/1 coded variable indicating whether or not each sib is in the frame population
##' @param sib.cell.vars see Details
##' @param ego.cell.vars see Details
##' @return A tibble with a row for each reported sibling in the frame population, the sibling's cell info,
##' the cell info of the survey respondent who reported each sibling, and unified variables with the ego and sibs'
##' cell values; these unified variables are called \code{.ego.cell} and \code{.sib.cell}, and are used in
##' [sib_ic_checks].
##' @examples
##'   # TODO write example code
##' @section Details:
##'   The \code{sib.cell.vars} and \code{ego.cell.vars} arguments should have a vector with Strings containing the names of
##'   columns in \code{esc.dat} identifying the cells to group reports by (typically age and sex, and possibly other variables).
##'   Note that, unlike the \code{cell.vars} argument in [get_ec_reports], for this function \code{sib.cell.vars} and
##'   \code{ego.cell.vars} MUST have the age variable listed FIRST.
##'   The goal of \code{get_ic_reports} is to find the cell that each sibling was in at the time of the interview, meaning the oldest age group to which the
##'   sibling contributed any exposure.
get_ic_reports <- function(esc.dat,
                           ego.dat,
                           ego.id,
                           sib.id,
                           sib.frame.indicator,
                           sib.cell.vars,
                           ego.cell.vars) {

  esc.dat <- esc.dat %>% dplyr::rename(.ego.id = !!sym(ego.id),
                                       .sib.id = !!sym(sib.id),
                                       .sib.in.F = !!sym(sib.frame.indicator))

  ec.yf.dat <- esc.dat %>%
    ## we want sibs on the frame,
    ## and we want age groups to which they contributed some
    ## exposure
    filter(.sib.in.F == 1, sib.exp > 0) %>%
    ## within each sib, we want to pick the latest age group
    ## they contributed exposure to
    group_by(across(all_of(c('.sib.id', sib.cell.vars[-1])))) %>%
    ## NB: assumes agegroup, when arranged, is in increasing
    ##     order of age, ie, age 15 precedes age 20, etc...
    arrange(desc(agegroup)) %>%
    slice(1)

  ## join in the ego data
  ec.yf.dat <- ec.yf.dat %>%
    left_join(ego.dat %>%
                dplyr::rename(.ego.id = !!sym(ego.id)) %>%
                select(.ego.id, !!!ego.cell.vars), by='.ego.id')

  ec.yf.dat <- ec.yf.dat %>% rename(!!ego.id := .ego.id,
                                    !!sib.id := .sib.id,
                                    !!sib.frame.indicator := .sib.in.F)
  return(ec.yf.dat)
}

##' get calculate internal consistency checks for sibling reports
##'
##' @param esc.dat The ego X sibling X cell dataset (see \code{get_esc_reports})
##' @param ego.dat The ego dataset, containing one row for each survey respondent
##' @param ego.id  String with the name of the column in \code{esc.dat} containing the survey respondent ID
##' @param sib.id  String with the name of the column in \code{esc.dat} containing the unique sibling ID
##' @param sib.frame.indicator String with the name of the column in \code{sib.dat} containing a 0/1 coded variable indicating whether or not each sib is in the frame population
##' @param sib.cell.vars see Details
##' @param ego.cell.vars see Details
##' @param boot.weights Dataframe with bootstrap resampled weights. See Details
##' @return A list with two entries: \code{ic.summ}, with summarized results; and \code{ic.boot.ests} with the full results for each bootstrap rep
##' @examples
##'   # TODO write example code
##' @section Details:
##'   The \code{sib.cell.vars} and \code{ego.cell.vars} arguments should have a vector with Strings containing the names of
##'   columns in \code{esc.dat} identifying the cells to group reports by (typically age and sex, and possibly other variables).
##'   Note that, unlike the \code{cell.vars} argument in \code{get_ec_reports}, for this function \code{sib.cell.vars} and
##'   \code{ego.cell.vars} MUST have the age variable listed FIRST.
##'   The goal of \code{get_ic_reports} is to find the cell that each sibling was in at the time of the interview, meaning the oldest age group to which the
##'   sibling contributed any exposure.
##'   \code{boot.weights} is assumed to have a column that is named whatever the \code{ego.id} is,
##'   and then a series of columns named \code{boot_weight_1}, ..., \code{boot_weight_M}.
##'
##' @export
sib_ic_checks <- function(esc.dat,
                          ego.dat,
                          ego.id,
                          sib.id,
                          sib.frame.indicator,
                          sib.cell.vars,
                          ego.cell.vars,
                          boot.weights) {

  ec.yf.dat <- get_ic_reports(esc.dat,
                              ego.dat,
                              ego.id,
                              sib.id,
                              sib.frame.indicator,
                              sib.cell.vars,
                              ego.cell.vars)

  # encode the cells into one variable, making it easier to work with
  ec.yf.dat <- ec.yf.dat %>%
    ungroup() %>%
    encode_cells('.ego.cell', ego.cell.vars) %>%
    encode_cells('.sib.cell', sib.cell.vars)

  # check that the levels are the same
  if(! setequal(unique(ec.yf.dat$.ego.cell), unique(ec.yf.dat$.sib.cell))) {
    warning("The cell values do not appear to be the same for ego and for sibs.")
  }

  cell.vals <- unique(ec.yf.dat$.ego.cell)

  M <- ncol(boot.weights) - 1

  ec.yf.dat <- ec.yf.dat %>%
    #left_join(boot.weights %>% dplyr::mutate(.ego.id = !!sym(ego.id)), by='.ego.id')
    left_join(boot.weights, by=ego.id)

  boot.cols <- stringr::str_subset(colnames(boot.weights), ego.id, negate=TRUE)

  # we'll map over bootstrap resamples and cells, calculating an
  # IC check for each bootstrap rep X cell
  boot.ests <- purrr::map_df(cell.vals,
                             function(cur_cell) {

                               # egos in group alpha
                               y.Falpha.dat <- ec.yf.dat %>%
                                 grab_cell('.ego.cell', cur_cell)
                               # reports from egos in group alpha about sibs in -alpha
                               y.Falpha.Fminusalpha.dat <- y.Falpha.dat %>%
                                 grab_cell_complement('.sib.cell', cur_cell)

                               # egos not in group alpha
                               y.Fminusalpha.dat <- ec.yf.dat %>%
                                 grab_cell_complement('.ego.cell', cur_cell)
                               # reports from egos not in group alpha about sibs in alpha
                               y.Fminusalpha.Falpha.dat <- y.Fminusalpha.dat %>%
                                 grab_cell('.sib.cell', cur_cell)

                               #####
                               ## size of frame (for normalization)

                               N.Falpha <- y.Falpha.dat %>%
                                 summarize(across(all_of(boot.cols), sum)) %>%
                                 tidyr::gather(starts_with('boot_weight'),
                                               key='qty',
                                               value='N.Falpha') %>%
                                 mutate(boot_idx = as.integer(stringr::str_remove_all(qty, '[^\\d]'))) %>%
                                 select(-qty)


                               N.Fminusalpha <- y.Fminusalpha.dat %>%
                                 summarize(across(all_of(boot.cols), sum)) %>%
                                 tidyr::gather(starts_with('boot_weight'),
                                               key='qty',
                                               value='N.Fminusalpha') %>%
                                 mutate(boot_idx = as.integer(stringr::str_remove_all(qty, '[^\\d]'))) %>%
                                 select(-qty)

                               #####
                               ## aggregate visibility estimators

                               ## use the current bootstrap weights to get estimated reports
                               ## from respondents in cell alpha to sibs in not alpha,
                               ## and from respondents in not alpha to sibs in alpha
                               ## NB: these are aggregate-visibility estimates
                               #y.Falpha.Fminusalpha <- sum(y.Falpha.Fminusalpha.dat %>% pull(!!sym(cur.wgt)))
                               #y.Fminusalpha.Falpha <- sum(y.Fminusalpha.Falpha.dat %>% pull(!!sym(cur.wgt)))

                               y.Falpha.Fminusalpha <- y.Falpha.Fminusalpha.dat %>%
                                 summarize(across(all_of(boot.cols), sum)) %>%
                                 tidyr::gather(starts_with('boot_weight'),
                                        key='qty',
                                        value='y.Falpha.Fminusalpha') %>%
                                 mutate(boot_idx = as.integer(stringr::str_remove_all(qty, '[^\\d]'))) %>%
                                 select(-qty)

                               y.Fminusalpha.Falpha <- y.Fminusalpha.Falpha.dat %>%
                                 summarize(across(all_of(boot.cols), sum)) %>%
                                 tidyr::gather(starts_with('boot_weight'),
                                        key='qty',
                                        value='y.Fminusalpha.Falpha') %>%
                                 mutate(boot_idx = as.integer(stringr::str_remove_all(qty, '[^\\d]'))) %>%
                                 select(-qty)

                               res <- y.Falpha.Fminusalpha %>%
                                 left_join(y.Fminusalpha.Falpha, by='boot_idx') %>%
                                 left_join(N.Falpha, by='boot_idx') %>%
                                 left_join(N.Fminusalpha, by='boot_idx') %>%
                                 mutate(cell = cur_cell,
                                        n_Falpha = nrow(y.Falpha.Fminusalpha.dat),
                                        n_Fminusalpha = nrow(y.Fminusalpha.Falpha.dat))

                               return(res)
                               #return(tibble(y.Falpha.Fminusalpha = y.Falpha.Fminusalpha,
                               #              y.Fminusalpha.Falpha = y.Fminusalpha.Falpha,
                               #              cell=cur_cell,
                               #              n_Falpha = nrow(y.Falpha.Fminusalpha.dat),
                               #              n_Fminusalpha = nrow(y.Fminusalpha.Falpha.dat)))

                             })

  ic.agg <- boot.ests %>%
    group_by(cell) %>%
    mutate(diff = y.Falpha.Fminusalpha - y.Fminusalpha.Falpha,
           abs_diff = abs(diff),
           diff2 = diff^2,
           normalized_diff = diff/(N.Falpha*N.Fminusalpha),
           abs_normalized_diff = abs(normalized_diff),
           normalized_diff2 = normalized_diff^2) %>%
    summarise(across(all_of(c('diff', 'abs_diff', 'diff2',
                              'normalized_diff', 'abs_normalized_diff', 'normalized_diff2')),
                     list(mean=~mean(.x),
                          se=~sd(.x),
                          ci_low=~quantile(.x, probs=.025),
                          ci_high=~quantile(.x, probs=.975)))) %>%
    decode_cells('cell', ego.cell.vars, remove=FALSE)

  boot.ests <- boot.ests %>%
    decode_cells('cell', ego.cell.vars, remove=FALSE)

  ## TODO - should rename ic.agg to ic.summ (to avoid confusion w/ aggregate vis estimator)
  res <- list(ic.summ = ic.agg,
              ic.boot.ests = boot.ests)

  return(res)
}



# this is a useful helper function, used in sib_ic_checks
grab_cell <- function(dat, cell.var, cell.value) {
  res <- dat %>% filter(.data[[cell.var]] == cell.value)
  return(res)
}

# this is a useful helper function, used in sib_ic_checks
grab_cell_complement <- function(dat, cell.var, cell.value) {
  res <- dat %>% filter(.data[[cell.var]] != cell.value)
  return(res)
}

# helper function to encode cells as a single variable
encode_cells <- function(df, new.name, cell.vars, sep="_X_") {
  res <- df %>%
    mutate(!!new.name := paste(!!!c(syms(cell.vars), sep=sep))) %>%
    select(-one_of(cell.vars))
  return(res)
}

# helper function do decode cells into multiple variables
decode_cells <- function(df, cell.name, cell.vars, remove=TRUE, sep="_X_") {
  res <- df %>%
    tidyr::separate(!!sym(cell.name),
                    into=cell.vars,
                    remove=remove,
                    sep=sep)
  return(res)
}
