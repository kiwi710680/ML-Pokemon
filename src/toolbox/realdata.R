# ---- real data: loaders return list(name, X, A, Y, binary) ----
load_rhc <- function(path = "data/rhc.csv") {
  d <- read.csv(path, stringsAsFactors = TRUE)
  d[[1]] <- NULL                                              # first column is a row number
  A <- as.integer(d$swang1 == "RHC"); Y <- as.integer(d$dth30 == "Yes")
  drop <- c("swang1", "death", "dth30", "sadmdte", "dschdte", "dthdte", "lstctdte",   # same drop list as Karim (2026)
            "t3d30", "ptid", "cat2", "adld3p", "urin1")
  covs <- droplevels(d[, setdiff(names(d), drop), drop = FALSE])
  X <- as.data.frame(model.matrix(~ . - 1, data = covs))
  colnames(X) <- make.names(colnames(X), unique = TRUE)
  list(name = "RHC", X = X, A = A, Y = Y, binary = TRUE)
}
load_ihdp <- function(r = 1, dir = "data/ihdp") {
  d <- read.csv(file.path(dir, sprintf("ihdp_npci_%d.csv", r)), header = FALSE)
  names(d) <- c("treatment", "y_factual", "y_cfactual", "mu0", "mu1", paste0("x", 1:25))
  list(name = sprintf("IHDP #%d", r), X = d[, paste0("x", 1:25)], A = d$treatment, Y = d$y_factual,
       binary = FALSE, truth = mean(d$mu1 - d$mu0), mu0 = d$mu0, mu1 = d$mu1)
}
read_nsw <- function(file) {
  d <- read.table(file, header = FALSE)
  names(d) <- c("treat", "age", "educ", "black", "hisp", "married", "nodegree", "re74", "re75", "re78")
  d
}
load_lalonde <- function(setting = c("psid", "experimental"), dir = "data/lalonde") {
  setting <- match.arg(setting)
  tr <- read_nsw(file.path(dir, "nswre74_treated.txt")); ct <- read_nsw(file.path(dir, "nswre74_control.txt"))
  benchmark <- mean(tr$re78) - mean(ct$re78)                   # randomized: the simple difference is the answer
  d <- if (setting == "experimental") rbind(tr, ct) else rbind(tr, read_nsw(file.path(dir, "psid_controls.txt")))
  covs <- c("age", "educ", "black", "hisp", "married", "nodegree", "re74", "re75")
  list(name = paste0("LaLonde (", setting, ")"), X = d[, covs], A = d$treat, Y = d$re78, binary = FALSE, benchmark = benchmark)
}

# ---- learners: "glm" (main effects) or "rf" (random forest) ----
learn_g <- function(X, A, newX, learner = "glm", ntree = 100) {
  if (learner == "glm") {
    fit <- suppressWarnings(glm(A ~ ., data = data.frame(A = A, X), family = binomial))
    return(unname(suppressWarnings(predict(fit, newX, type = "response"))))
  }
  fit <- randomForest::randomForest(x = X, y = factor(A, levels = 0:1), ntree = ntree)
  unname(predict(fit, newX, type = "prob")[, "1"])
}
learn_Q <- function(X, A, Y, newX, binary, learner = "glm", ntree = 100) {
  tr <- data.frame(A = A, X); n1 <- data.frame(A = 1, newX); n0 <- data.frame(A = 0, newX)
  if (learner == "glm") {
    fit <- if (binary) suppressWarnings(glm(Y ~ ., data = data.frame(Y = Y, tr), family = binomial))
           else lm(Y ~ ., data = data.frame(Y = Y, tr))
    pr <- function(nd) unname(suppressWarnings(predict(fit, nd, type = "response")))
    return(list(Q1 = pr(n1), Q0 = pr(n0)))
  }
  if (binary) {
    fit <- randomForest::randomForest(x = tr, y = factor(Y, levels = 0:1), ntree = ntree)
    pr <- function(nd) unname(predict(fit, nd, type = "prob")[, "1"])
  } else {
    fit <- randomForest::randomForest(x = tr, y = Y, ntree = ntree)
    pr <- function(nd) unname(predict(fit, nd))
  }
  list(Q1 = pr(n1), Q0 = pr(n0))
}

# full: train on everyone, predict the same people.  crossfit: each prediction comes from the other K - 1 folds
nuisance <- function(d, learner = "glm", scheme = c("full", "crossfit"), K = 5, seed = 1,
                     bounds = c(0.025, 0.975), ntree = 100) {
  scheme <- match.arg(scheme)
  n <- length(d$A)
  set.seed(seed)
  fold <- if (scheme == "full") rep(1L, n) else sample(rep(seq_len(K), length.out = n))
  out <- data.frame(Q1 = numeric(n), Q0 = numeric(n), g_raw = numeric(n))
  for (k in unique(fold)) {
    te <- fold == k; tr <- if (scheme == "full") rep(TRUE, n) else !te
    q <- learn_Q(d$X[tr, , drop = FALSE], d$A[tr], d$Y[tr], d$X[te, , drop = FALSE], d$binary, learner, ntree)
    out$Q1[te] <- q$Q1; out$Q0[te] <- q$Q0
    out$g_raw[te] <- learn_g(d$X[tr, , drop = FALSE], d$A[tr], d$X[te, , drop = FALSE], learner, ntree)
  }
  if (d$binary) { out$Q1 <- pmin(pmax(out$Q1, 0), 1); out$Q0 <- pmin(pmax(out$Q0, 0), 1) }
  out$g <- pmin(pmax(out$g_raw, bounds[1]), bounds[2])
  out$fold <- fold
  out
}

panel <- function(d, nu, label = "") {                        # one set of nuisances -> four estimators
  dd <- list(A = d$A, Y = d$Y)
  a <- est_aipw(dd, nu); t <- est_tmle(dd, nu, ybounds = if (d$binary) c(0, 1) else range(d$Y))
  data.frame(scheme = label, estimator = c("g-computation", "IPTW (Hajek)", "AIPW", "TMLE"),
             est = c(est_plugin(nu), est_ipw(dd, nu), a[["est"]], t[["est"]]),
             se = c(NA, NA, a[["se"]], t[["se"]]), lo = c(NA, NA, a[["lo"]], t[["lo"]]), hi = c(NA, NA, a[["hi"]], t[["hi"]]))
}
residual_check <- function(d, nu) {                           # is the learner grading its own exam?
  QA <- ifelse(d$A == 1, nu$Q1, nu$Q0); H <- d$A / nu$g - (1 - d$A) / (1 - nu$g)
  c(resid_rms = sqrt(mean((d$Y - QA)^2)), correction = mean(H * (d$Y - QA)))
}

# ---- diagnostics: overlap and balance ----
smd <- function(x, A, w = rep(1, length(A))) {                # standardized mean difference
  m1 <- weighted.mean(x[A == 1], w[A == 1]); m0 <- weighted.mean(x[A == 0], w[A == 0])
  s <- sqrt((var(x[A == 1]) + var(x[A == 0])) / 2)
  if (s == 0) 0 else (m1 - m0) / s
}
balance_table <- function(X, A, g) {
  w <- ifelse(A == 1, 1 / g, 1 / (1 - g))
  data.frame(variable = names(X), before = sapply(X, smd, A = A), after_IPW = sapply(X, smd, A = A, w = w), row.names = NULL)
}
overlap_summary <- function(g_raw, A, bounds = c(0.025, 0.975)) {
  gb <- pmin(pmax(g_raw, bounds[1]), bounds[2]); w <- ifelse(A == 1, 1 / gb, 1 / (1 - gb))
  c(min_g = min(g_raw), max_g = max(g_raw), pct_clipped = 100 * mean(g_raw < bounds[1] | g_raw > bounds[2]),
    max_weight = max(w), ESS_treated = sum(w[A == 1])^2 / sum(w[A == 1]^2), ESS_control = sum(w[A == 0])^2 / sum(w[A == 0]^2))
}
