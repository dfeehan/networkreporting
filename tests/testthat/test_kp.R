##########################################################
## test_kp.R
##
## unit tests for functions that compute known population
## degree estimates:
##   kp.individual.estimator_
##   total.degree.estimator
##########################################################

####################################
## kp.individual.estimator_
context("estimators - known population individual")

test_that("kp.individual.estimator_ returns correct hand-computed values", {
  # 2 respondents, 2 known populations
  # nurses total = 50, teachers total = 50, so total.kp.size = 100
  # alter.popn.size = 1000
  #
  # Respondent 1: nurses=2, teachers=3  -> kptot=5 -> dbar = 5/100 * 1000 = 50
  # Respondent 2: nurses=0, teachers=5  -> kptot=5 -> dbar = 5/100 * 1000 = 50
  df <- data.frame(nurses   = c(2, 0),
                   teachers = c(3, 5))

  result <- kp.individual.estimator_(df,
                                     known.populations = c("nurses", "teachers"),
                                     total.kp.size     = 100,
                                     alter.popn.size   = 1000)

  expect_equal(result$dbar.Fcell.F, c(50, 50))
})

test_that("kp.individual.estimator_ correctly scales by alter.popn.size", {
  # Same setup but alter.popn.size = 2000 -> dbar should double
  df <- data.frame(nurses   = c(2, 0),
                   teachers = c(3, 5))

  result <- kp.individual.estimator_(df,
                                     known.populations = c("nurses", "teachers"),
                                     total.kp.size     = 100,
                                     alter.popn.size   = 2000)

  expect_equal(result$dbar.Fcell.F, c(100, 100))
})

test_that("kp.individual.estimator_ with dropmiss=FALSE treats NAs as zeros in rowsum", {
  # Respondent 1: nurses=2, teachers=NA -> rowSums with na.rm=TRUE -> kptot=2
  #   (dropmiss=FALSE means: use na.rm=TRUE in rowSums, i.e. ignore missing)
  #   dbar = 2/100 * 1000 = 20
  # Respondent 2: nurses=0, teachers=5 -> kptot=5 -> dbar = 50
  df <- data.frame(nurses   = c(2, 0),
                   teachers = c(NA, 5))

  result <- kp.individual.estimator_(df,
                                     known.populations = c("nurses", "teachers"),
                                     total.kp.size     = 100,
                                     alter.popn.size   = 1000,
                                     dropmiss          = FALSE)

  expect_equal(result$dbar.Fcell.F, c(20, 50))
})

test_that("kp.individual.estimator_ with dropmiss=TRUE gives 0 for fully-missing respondent", {
  # dropmiss=TRUE -> rowSums without na.rm -> row 1 gets NA kptot
  # NA kptot is skipped (treated as 0) in the weighted sum, so dbar.Fcell.F = 0
  # (compare to dropmiss=FALSE where teachers=NA is treated as 0 in rowSums,
  #  so row 1 contributes nurses=2 and dbar.Fcell.F = 20)
  df <- data.frame(nurses   = c(2, 0),
                   teachers = c(NA, 5))

  result <- kp.individual.estimator_(df,
                                     known.populations = c("nurses", "teachers"),
                                     total.kp.size     = 100,
                                     alter.popn.size   = 1000,
                                     dropmiss          = TRUE)

  expect_equal(result$dbar.Fcell.F[1], 0)
  expect_equal(result$dbar.Fcell.F[2], 50)
})


####################################
## total.degree.estimator
context("estimators - known population total degree")

test_that("total.degree.estimator returns unweighted sum of degrees", {
  df <- data.frame(d = c(10, 20, 30))
  result <- total.degree.estimator(df, d.hat.vals = "d")
  expect_equal(result, 60)
})

test_that("total.degree.estimator returns weighted sum of degrees", {
  # weights = c(1, 2, 1); weighted sum = 10*1 + 20*2 + 30*1 = 80
  df <- data.frame(d = c(10, 20, 30), w = c(1, 2, 1))
  result <- total.degree.estimator(df, d.hat.vals = "d", weights = "w")
  expect_equal(result, 80)
})

test_that("total.degree.estimator with complete.obs excludes NA rows", {
  # Row 2 has NA degree; complete.obs should skip it
  # sum of non-NA weighted degrees: 10*1 + 30*1 = 40
  df <- data.frame(d = c(10, NA, 30), w = c(1, 1, 1))
  result <- total.degree.estimator(df, d.hat.vals = "d", weights = "w",
                                   missing = "complete.obs")
  expect_equal(result, 40)
})

test_that("total.degree.estimator stops on invalid missing argument", {
  df <- data.frame(d = c(10, 20))
  expect_error(total.degree.estimator(df, d.hat.vals = "d", missing = "badoption"))
})
