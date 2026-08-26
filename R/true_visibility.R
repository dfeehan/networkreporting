## True visibility, for validating a rule against a known network.
##
## A simulation knows the whole reporting network, so it knows every alter's
## real visibility. That makes it possible to ask of a visibility rule not
## "which tier fired" but "was it right" -- which is the only way to find out
## that a rule returning finite, plausible numbers is producing wrong ones.
##
## The definition is short enough that it keeps getting written inline, and it
## keeps getting written differently. The two things that go wrong are worth
## naming, because neither announces itself:
##
##   1. WHOSE frame membership counts. Visibility is the number of
##      FRAME-POPULATION MEMBERS who could report an alter, so it is the
##      REPORTER's frame status that matters, never the alter's. Filtering on
##      the alter's gives a different quantity that looks just as reasonable.
##   2. DUPLICATE ties. A census that links full siblings once through each
##      parent carries every pair twice, and counting rows then doubles the
##      answer. In one socsim census this made an exact rule look 27% wrong.

##' True visibility, read off a known reporting network
##'
##' Counts, for each alter, how many frame-population members are connected to
##' them --- which is exactly what a visibility rule is trying to estimate. Only
##' usable where the whole network is known, so in practice: simulation.
##'
##' @section Whose frame membership:
##'
##' `reporter.in.frame` names a column describing the **reporter**, not the
##' alter. This is the distinction that gets lost when the definition is written
##' inline, and it is not a technicality: an alter's visibility is the number of
##' people who *could report them*, so it depends on who is in the frame
##' population and connected to them. Filtering on the alter's own frame status
##' instead yields a different quantity --- one that is also a plausible number,
##' and also wrong.
##'
##' If the census has already been restricted to frame-member reporters, leave
##' `reporter.in.frame` as `NULL` and every row counts.
##'
##' @section Duplicate ties:
##'
##' A census may carry the same (reporter, alter) pair more than once --- full
##' siblings get linked once through each parent, so every sibling pair appears
##' twice. Counting rows then doubles the visibility. `dedup = TRUE` removes
##' repeated pairs and reports how many it dropped; the count is worth looking
##' at, since a large one usually means the census is built differently from how
##' you thought.
##'
##' @param census a data frame with one row per (reporter, alter) tie in the
##'        true network
##' @param reporter name of the column identifying the person who could report
##' @param alter name of the column identifying the person reported about
##' @param reporter.in.frame name of a 0/1 or logical column saying whether the
##'        **reporter** is in the frame population, or `NULL` if every row is
##'        already a frame-member reporter
##' @param dedup drop repeated (reporter, alter) pairs before counting
##' @return a tibble with one row per alter: the alter id and `vis_true`
##' @examples
##' census <- data.frame(
##'   from = c("a", "b", "c", "a"),
##'   to   = c("x", "x", "x", "y"),
##'   from_in_frame = c(1, 1, 0, 1))
##' # x is connected to three people, but only two of them are in the frame
##' true_visibility_from_network(census, "from", "to", "from_in_frame")
##' @seealso [visibility_accuracy()], which scores a rule against this
##' @export
##' @md
true_visibility_from_network <- function(census,
                                         reporter,
                                         alter,
                                         reporter.in.frame = NULL,
                                         dedup             = TRUE) {

  for (nm in c(reporter, alter, reporter.in.frame)) {
    if (!nm %in% names(census)) {
      stop("column '", nm, "' is not in the census.\n",
           "The census has: ", paste(names(census), collapse = ", "))
    }
  }

  d <- dplyr::tibble(.reporter = census[[reporter]],
                     .alter    = census[[alter]])

  d$.in.frame <- if (is.null(reporter.in.frame)) {
    TRUE
  } else {
    as.logical(census[[reporter.in.frame]] == 1 |
               census[[reporter.in.frame]] %in% TRUE)
  }

  n.before <- nrow(d)
  if (dedup) {
    d <- dplyr::distinct(d, .data$.reporter, .data$.alter, .keep_all = TRUE)
  }
  n.dropped <- n.before - nrow(d)

  if (n.dropped > 0) {
    message(n.dropped, " duplicate (reporter, alter) pair(s) dropped of ",
            n.before, ". A census that links a pair through more than one route ",
            "carries it more than once, and counting rows would double the ",
            "visibility.")
  }

  out <- d %>%
    dplyr::filter(.data$.in.frame) %>%
    dplyr::count(.data$.alter, name = "vis_true") %>%
    dplyr::rename(!!alter := ".alter")

  ## An alter connected only to non-frame members has a true visibility of
  ## zero: real, and worth knowing about, since no survey of that frame could
  ## ever have reported them.
  unreachable <- setdiff(unique(d$.alter), out[[alter]])
  if (length(unreachable)) {
    message(length(unreachable), " alter(s) are connected to no frame-population ",
            "member, so their true visibility is 0. They are omitted here: a ",
            "survey of this frame could not have reported them at all.")
  }

  out
}

##' Score a visibility rule against a known truth
##'
##' Compares predicted visibilities to true ones, **separately for alters in and
##' out of the frame population**, and reports the ratio between those two
##' errors.
##'
##' @section Why the split, and why the ratio:
##'
##' Visibility reaches a death rate only through the asymmetry between on-frame
##' and off-frame alters: every death is off-frame, while exposure is a mixture.
##' An error of the same size on both sides therefore largely cancels out of the
##' rate. A *differential* one does not --- it biases it.
##'
##' So an overall accuracy figure is close to useless here. A rule can be
##' badly wrong on both sides and still give an almost unbiased rate, or mildly
##' wrong in a lopsided way and bias it substantially. `differential` is the
##' number to read.
##'
##' @param predicted numeric vector of predicted visibilities, one per report
##' @param truth numeric vector of true visibilities, the same length
##' @param in.frame logical or 0/1 vector saying whether each **alter** is in
##'        the frame population. Note this is the alter's status --- unlike
##'        [true_visibility_from_network()], where it is the reporter's
##' @return a `visibility_accuracy` object: per-side counts, the share predicted
##'         exactly, the mean ratio of predicted to true, and the differential
##' @examples
##' visibility_accuracy(predicted = c(3, 4, 3, 4),
##'                     truth     = c(3, 3, 3, 4),
##'                     in.frame  = c(TRUE, FALSE, TRUE, FALSE))
##' @seealso [true_visibility_from_network()]
##' @export
##' @md
visibility_accuracy <- function(predicted, truth, in.frame) {

  n <- length(predicted)
  if (length(truth) != n || length(in.frame) != n) {
    stop("predicted, truth and in.frame must be the same length; got ",
         n, ", ", length(truth), " and ", length(in.frame), ".")
  }

  d <- dplyr::tibble(predicted = as.numeric(predicted),
                     truth     = as.numeric(truth),
                     in.frame  = as.logical(in.frame == 1 | in.frame %in% TRUE))

  n.dropped <- sum(is.na(d$predicted) | is.na(d$truth) | d$truth <= 0)
  d <- d %>% dplyr::filter(!is.na(.data$predicted), !is.na(.data$truth),
                           .data$truth > 0)

  if (!nrow(d)) {
    stop("nothing left to score: every row had a missing prediction, a missing ",
         "truth, or a true visibility of zero.")
  }

  by.side <- d %>%
    dplyr::mutate(side = ifelse(.data$in.frame, "on-frame", "off-frame")) %>%
    dplyr::group_by(.data$side) %>%
    dplyr::summarize(n          = dplyr::n(),
                     exact      = mean(.data$predicted == .data$truth),
                     mean_ratio = mean(.data$predicted / .data$truth),
                     .groups    = "drop")

  off <- by.side$mean_ratio[by.side$side == "off-frame"]
  on  <- by.side$mean_ratio[by.side$side == "on-frame"]

  structure(list(by_side      = by.side,
                 differential = if (length(off) && length(on)) off / on
                                else NA_real_,
                 n_scored     = nrow(d),
                 n_dropped    = n.dropped),
            class = "visibility_accuracy")
}

##' @export
print.visibility_accuracy <- function(x, ...) {
  cat("<visibility_accuracy>\n")
  cat("  scored: ", x$n_scored, " report(s)",
      if (x$n_dropped) paste0("; ", x$n_dropped,
                              " dropped (missing, or true visibility 0)") else "",
      "\n", sep = "")
  cat("\n")
  cat(sprintf("  %-10s %8s %8s %12s\n", "side", "n", "exact", "pred/true"))
  for (i in seq_len(nrow(x$by_side))) {
    r <- x$by_side[i, ]
    cat(sprintf("  %-10s %8d %7.1f%% %12.3f\n",
                r$side, r$n, 100 * r$exact, r$mean_ratio))
  }
  cat("\n")
  if (is.na(x$differential)) {
    cat("  differential: n/a (only one side present)\n")
  } else {
    cat(sprintf("  differential (off/on) = %.3f  %s\n", x$differential,
                if (abs(x$differential - 1) < 0.01)
                  "-- cancels out of a rate"
                else
                  "-- BIASES a rate"))
    cat("  Every death is off-frame while exposure is a mixture, so it is the\n")
    cat("  ratio between the two sides, not either one, that reaches the rate.\n")
  }
  invisible(x)
}
