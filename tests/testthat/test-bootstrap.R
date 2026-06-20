# ---- co_bootstrap() tests ----

if (!exists("cooccurrence", mode = "function")) {
  root <- if (identical(basename(getwd()), "testthat")) {
    normalizePath(file.path(getwd(), "..", ".."))
  } else {
    getwd()
  }
  source(file.path(root, "R", "cooccurrence.R"))
  source(file.path(root, "R", "bootstrap.R"))
}

.boot_tx <- c(
  rep(list(c("A", "B")), 30),
  rep(list(c("A", "C")), 20),
  rep(list(c("B", "C")), 25),
  rep(list(c("A", "B", "C")), 15),
  rep(list(c("A")), 10)
)

test_that("co_bootstrap returns expected data frame class and columns", {
  fit <- cooccurrence(.boot_tx, similarity = "jaccard")
  res <- co_bootstrap(fit, R = 20, seed = 1)

  expect_s3_class(res, "co_bootstrap")
  expect_s3_class(res, "cooccurrence")
  expect_s3_class(res, "data.frame")
  expect_true(all(c("from", "to", "weight", "boot_mean", "boot_se",
                    "ci_low", "ci_high", "stable") %in% names(res)))
})

test_that("bootstrap central estimates track original jaccard weights", {
  fit <- cooccurrence(.boot_tx, similarity = "jaccard")
  res <- co_bootstrap(fit, R = 300, seed = 2)

  expect_true(all(abs(res$boot_mean - res$weight) < 0.05))
})

test_that("bootstrap central estimates track original cosine weights", {
  fit <- cooccurrence(.boot_tx, similarity = "cosine")
  res <- co_bootstrap(fit, R = 300, seed = 3)

  expect_true(all(abs(res$boot_mean - res$weight) < 0.05))
})

test_that("bayes and classic engines give similar central estimates", {
  fit <- cooccurrence(.boot_tx, similarity = "jaccard")
  classic <- co_bootstrap(fit, R = 250, engine = "classic", seed = 4)
  bayes <- co_bootstrap(fit, R = 250, engine = "bayes", seed = 4)

  expect_true(all(abs(classic$boot_mean - bayes$boot_mean) < 0.1))
})

test_that("block bootstrap gives wider intervals than transaction bootstrap", {
  blocks <- rep(paste0("g", seq_len(12)), each = 4)
  tx <- c(
    rep(list(c("A", "B")), 24),
    rep(list(c("A", "C")), 24)
  )
  fit <- cooccurrence(tx, similarity = "jaccard", block = blocks)

  row_boot <- suppressWarnings(co_bootstrap(fit, R = 400, seed = 5))
  block_boot <- co_bootstrap(fit, R = 400, by = "block_id", seed = 5)

  row_width <- mean(row_boot$ci_high - row_boot$ci_low)
  block_width <- mean(block_boot$ci_high - block_boot$ci_low)
  expect_gt(block_width, row_width)
})

test_that("co_bootstrap supports fractional but refuses attention counting", {
  fit_fractional <- cooccurrence(.boot_tx, counting = "fractional",
                                 keep_transactions = TRUE)
  res <- co_bootstrap(fit_fractional, R = 50, seed = 7)
  expect_s3_class(res, "co_bootstrap")
  # bootstrap is centred on the fractional point estimate
  expect_equal(res$boot_mean, res$weight, tolerance = 0.15)

  fit_attention <- cooccurrence(.boot_tx, counting = "attention",
                                keep_transactions = TRUE)
  expect_error(co_bootstrap(fit_attention, R = 20), "attention")
})

test_that("co_bootstrap hard guards fire", {
  fit_scaled <- cooccurrence(.boot_tx, scale = "log")
  expect_error(co_bootstrap(fit_scaled, R = 20),
               "unscaled output")

  fit_relative <- cooccurrence(.boot_tx, similarity = "relative")
  expect_error(co_bootstrap(fit_relative, R = 20),
               "relative")

  fit_missing <- cooccurrence(.boot_tx, keep_transactions = FALSE)
  expect_error(co_bootstrap(fit_missing, R = 20),
               "keep_transactions = TRUE")

  fit <- cooccurrence(.boot_tx)
  expect_error(co_bootstrap(fit, R = 1),
               "`R` must be at least 2")
})

test_that("co_bootstrap stable uses the two-sided consistency_range rule", {
  fit <- cooccurrence(.boot_tx, similarity = "jaccard", keep_transactions = TRUE)
  res <- co_bootstrap(fit, R = 500, consistency_range = c(0.75, 1.25),
                      consistency = 0.95, seed = 11)
  expect_true(all(c("cr_lower", "cr_upper", "prop_within", "stable") %in%
                    names(res)))
  expect_true(all(res$prop_within >= 0 & res$prop_within <= 1))
  expect_identical(res$stable, res$prop_within >= 0.95)
  # band is two-sided around the observed weight
  expect_equal(res$cr_lower, 0.75 * res$weight)
  expect_equal(res$cr_upper, 1.25 * res$weight)
})

test_that("co_bootstrap wide consistency_range flags every edge stable", {
  fit <- cooccurrence(.boot_tx, similarity = "jaccard", keep_transactions = TRUE)
  res <- co_bootstrap(fit, R = 300, consistency_range = c(1e-6, 1e6), seed = 12)
  expect_true(all(res$stable))
})

test_that("co_bootstrap stricter consistency yields no more stable edges", {
  fit <- cooccurrence(.boot_tx, similarity = "jaccard", keep_transactions = TRUE)
  lenient <- co_bootstrap(fit, R = 500, consistency = 0.80, seed = 13)
  strict  <- co_bootstrap(fit, R = 500, consistency = 0.99, seed = 13)
  expect_lte(sum(strict$stable), sum(lenient$stable))
})
