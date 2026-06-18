# Tests for gamlss/polr/reldist helper functions in R/misc_functions.R:
#   validate_gamlss, guess_gamlss, guess_polr, crossval_gamlss,
#   reldist_diagnostics

if (!requireNamespace("gamlss", quietly = TRUE)) {
  exit_file("gamlss not available - skipping gamlss helper tests")
}

suppressMessages({
  library(data.table)
  library(gamlss)
})


# Open a pdf device so plotting functions do not need an interactive device.
grDevices::pdf(tempfile(fileext = ".pdf"))
on.exit(grDevices::dev.off(), add = TRUE)

set.seed(42L)

# ---------------------------------------------------------------------------
# Build a small dataset and fit a tiny gamlss NO model
# ---------------------------------------------------------------------------
n <- 60L
dat <- data.table(
  year = rep(c(1L, 2L), each = n / 2L),
  agegrp = runif(n, 20, 60)
)
dat[, bmi := 25 + 0.05 * agegrp + rnorm(n, 0, 2)]

mod <- suppressWarnings(gamlss::gamlss(
  bmi ~ agegrp,
  sigma.fo = ~1,
  family = gamlss.dist::NO(),
  data = dat,
  control = gamlss::gamlss.control(n.cyc = 5L, trace = FALSE)
))

expect_true(inherits(mod, "gamlss"), info = "gamlss model fitted")

# ---------------------------------------------------------------------------
# validate_gamlss
# ---------------------------------------------------------------------------
vg <- validate_gamlss(copy(dat), mod, mc = 3L, orig_data = copy(dat))
expect_true(is.data.table(vg), info = "validate_gamlss returns data.table")
expect_true("type" %in% names(vg), info = "validate_gamlss has type column")
expect_true("bmi" %in% names(vg), info = "validate_gamlss has outcome column")
expect_true(all(c("Observed", "Modelled") %in% unique(vg$type)),
  info = "validate_gamlss has both Observed and Modelled rows")
# n observed rows + mc * n modelled rows
expect_equal(nrow(vg), n + 3L * n, info = "validate_gamlss row count")
expect_true(all(is.finite(vg[type == "Modelled", bmi])),
  info = "validate_gamlss modelled outcome finite")
# parameter columns dropped on exit
expect_false("mu" %in% names(vg), info = "validate_gamlss drops mu param col")

# default orig_data branch (orig_data = dtb)
vg2 <- validate_gamlss(copy(dat), mod, mc = 2L)
expect_equal(nrow(vg2), n + 2L * n, info = "validate_gamlss default orig_data")

# ---------------------------------------------------------------------------
# crossval_gamlss
# ---------------------------------------------------------------------------
dat_cv <- copy(dat)
dat_cv[, rank := runif(.N)]   # column named "rank" (default colnam)
cv <- crossval_gamlss(dat_cv, mod, orig_data = dat_cv, colnam = "rank")
expect_true(is.list(cv), info = "crossval_gamlss returns list")
expect_true(all(c("observed", "predicted") %in% names(cv)),
  info = "crossval_gamlss has observed and predicted")
expect_equal(length(cv$observed), nrow(dat_cv),
  info = "crossval_gamlss observed length")
expect_equal(length(cv$predicted), nrow(dat_cv),
  info = "crossval_gamlss predicted length")
expect_true(all(is.finite(cv$predicted)),
  info = "crossval_gamlss predicted finite")

# Exercise the p==0 / p==1 clamping branches
dat_cv2 <- copy(dat)
dat_cv2[, rank := c(0, 1, runif(.N - 2L))]
cv2 <- crossval_gamlss(dat_cv2, mod, orig_data = dat_cv2, colnam = "rank")
expect_true(all(is.finite(cv2$predicted)),
  info = "crossval_gamlss clamps p==0/p==1")

# default orig_data branch
cv3 <- crossval_gamlss(copy(dat_cv), mod, colnam = "rank")
expect_true(is.list(cv3), info = "crossval_gamlss default orig_data")

# ---------------------------------------------------------------------------
# guess_gamlss
#  - guess_gamlss splits the unique predictor rows by `dtu$year`, so the model
#    formula MUST contain `year` as a predictor (otherwise dtu has no `year`).
#  - needs a `rank_<y>` column (here rank_bmi) holding percentiles in (0, 1).
#  - modifies `dtb` by reference: the outcome column (bmi) is replaced by the
#    predicted quantile, and the working columns (p, mu, sigma) are dropped.
# ---------------------------------------------------------------------------
mod_y <- suppressWarnings(gamlss::gamlss(
  bmi ~ agegrp + year,
  sigma.fo = ~1,
  family = gamlss.dist::NO(),
  data = dat,
  control = gamlss::gamlss.control(n.cyc = 5L, trace = FALSE)
))

dat_g <- copy(dat)
dat_g[, rank_bmi := runif(.N, 0.01, 0.99)]

gg <- copy(dat_g)
res_g <- guess_gamlss(gg, mod_y, orig_data = copy(dat_g), nc = 1L)
# returns the modified data.table (invisibly) and edits by reference
expect_true(is.data.table(gg), info = "guess_gamlss keeps dtb a data.table")
expect_true(all(is.finite(gg$bmi)),
  info = "guess_gamlss fills outcome with finite predicted quantiles")
expect_false("p" %in% names(gg), info = "guess_gamlss drops the working p column")
expect_false("mu" %in% names(gg), info = "guess_gamlss drops gamlss param columns")
expect_true("rank_bmi" %in% names(gg), info = "guess_gamlss leaves rank_ column intact")

# default orig_data branch (orig_data = gamlss_obj$data): gamlss stores $data as
# a plain data.frame, so this hits the is.data.table(orig_data) guard.
err2 <- tryCatch(
  guess_gamlss(copy(dat_g), mod_y, nc = 1L),
  error = function(e) conditionMessage(e)
)
expect_true(is.character(err2) && grepl("orig_data", err2),
  info = "guess_gamlss default orig_data hits is.data.table guard")

# ---------------------------------------------------------------------------
# guess_polr (MASS::polr)
# ---------------------------------------------------------------------------
if (requireNamespace("MASS", quietly = TRUE) &&
    requireNamespace("matrixStats", quietly = TRUE)) {

  set.seed(7L)
  np <- 120L
  xp <- runif(np, -2, 2)
  # ordered outcome with 3 levels driven by xp
  lp <- 1.5 * xp + rnorm(np)
  ycat <- cut(lp, breaks = quantile(lp, c(0, 1 / 3, 2 / 3, 1)),
    include.lowest = TRUE, labels = c("low", "mid", "high"))
  pdat <- data.frame(y = factor(ycat, ordered = TRUE), x = xp)

  pmod <- MASS::polr(y ~ x, data = pdat, Hess = TRUE)
  expect_true(inherits(pmod, "polr"), info = "polr model fitted")

  pdt <- as.data.table(pdat)
  pdt[, rank_y := runif(.N, 0.01, 0.99)]
  res_polr <- guess_polr(pdt, pmod)
  # function modifies dtb in place; y column replaced with integer codes
  expect_true("y" %in% names(pdt), info = "guess_polr keeps outcome col")
  expect_true(is.numeric(pdt$y), info = "guess_polr outcome numeric")
  expect_true(all(is.finite(pdt$y)), info = "guess_polr outcome finite")
  q <- length(pmod$zeta)
  expect_true(all(pdt$y >= 0L & pdt$y <= q),
    info = "guess_polr categories within range")
  expect_false("p" %in% names(pdt), info = "guess_polr drops p column")
} else {
  cat("MASS/matrixStats not available - skipping guess_polr test\n")
}

# ---------------------------------------------------------------------------
# reldist_diagnostics (uses reldist package; produces plots)
# ---------------------------------------------------------------------------
if (requireNamespace("reldist", quietly = TRUE)) {
  set.seed(99L)
  m <- 400L
  reference <- rnorm(m, 0, 1)
  comparison <- rnorm(m, 0.4, 1.1)
  reference_wt <- rep(1, m)
  comparison_wt <- rep(1, m)

  rd <- suppressWarnings(reldist_diagnostics(
    comparison = comparison,
    reference = reference,
    comparison_wt = comparison_wt,
    reference_wt = reference_wt,
    main = "test",
    smooth = 0.35,
    discrete = FALSE
  ))
  expect_true(is.data.table(rd), info = "reldist_diagnostics returns data.table")
  expect_equal(nrow(rd), 6L, info = "reldist_diagnostics 6 summary rows")
  expect_true(all(c("Summary statistics", "Measure", "p-value") %in% names(rd)),
    info = "reldist_diagnostics expected columns")
  expect_true(is.numeric(rd[["Measure"]]),
    info = "reldist_diagnostics Measure numeric")
} else {
  cat("reldist not available - skipping reldist_diagnostics test\n")
}
