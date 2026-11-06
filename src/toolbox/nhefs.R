# ---- NHEFS: data, candidate learners, sl_lite, nuisances ----
load_nhefs <- function(path = "data/nhefs.csv") {
  d <- subset(read.csv(path), !is.na(wt82_71))                # complete cases for the outcome
  data.frame(Y = d$wt82_71, A = d$qsmk, sex = factor(d$sex), race = factor(d$race), age = d$age,
             education = factor(d$education), smokeintensity = d$smokeintensity, smokeyrs = d$smokeyrs,
             exercise = factor(d$exercise), active = factor(d$active), wt71 = d$wt71)
}
rhs <- list(                                                  # each learner is just a formula
  mean   = "1",
  main   = "as.numeric(sex) + as.numeric(race) + age + as.numeric(education) + smokeintensity + smokeyrs + as.numeric(exercise) + as.numeric(active) + wt71",
  hr     = "sex + race + age + I(age^2) + education + smokeintensity + I(smokeintensity^2) + smokeyrs + I(smokeyrs^2) + exercise + active + wt71 + I(wt71^2)",
  spline = "sex + race + education + exercise + active + splines::ns(age, 3) + splines::ns(smokeintensity, 3) + splines::ns(smokeyrs, 3) + splines::ns(wt71, 3)")
q_formula <- function(r) as.formula(if (r == "1") "Y ~ A" else paste("Y ~ A + A:smokeintensity +", r))
g_formula <- function(r) as.formula(paste("A ~", r))
fit_pred <- function(train, newdata, learner, what) {
  if (what == "g") predict(glm(g_formula(rhs[[learner]]), family = binomial, data = train), newdata, type = "response")
  else predict(lm(q_formula(rhs[[learner]]), data = train), newdata)
}
convex_weights <- function(Z, y) {
  obj <- function(b) { w <- exp(c(0, b)); w <- w / sum(w); mean((y - Z %*% w)^2) }
  b <- optim(rep(0, ncol(Z) - 1), obj, method = "BFGS")$par
  w <- exp(c(0, b)); setNames(w / sum(w), colnames(Z))
}
sl_lite <- function(train, newdata, what, learners = names(rhs), V = 5) {   # inner V-fold CV -> convex weights
  y <- if (what == "g") train$A else train$Y
  fold <- sample(rep(seq_len(V), length.out = nrow(train)))
  Z <- matrix(NA_real_, nrow(train), length(learners), dimnames = list(NULL, learners))
  for (v in seq_len(V)) for (l in learners) Z[fold == v, l] <- fit_pred(train[fold != v, ], train[fold == v, ], l, what)
  w <- convex_weights(Z, y)
  structure(as.vector(sapply(learners, function(l) fit_pred(train, newdata, l, what)) %*% w), weights = w)
}
nhefs_nuisance <- function(train, newdata, learner = "sl_lite", bounds = c(0.025, 0.975)) {
  pr <- function(nd, what) if (learner == "sl_lite") sl_lite(train, nd, what) else fit_pred(train, nd, learner, what)
  g_raw <- pr(newdata, "g")
  data.frame(Q1 = pr(transform(newdata, A = 1), "Q"), Q0 = pr(transform(newdata, A = 0), "Q"),
             g = bound(g_raw, bounds[1], bounds[2]), g_raw = g_raw)
}
nhefs_crossfit <- function(dat, learner = "sl_lite", K = 5, seed = 1, bounds = c(0.025, 0.975)) {
  set.seed(seed)
  fold <- sample(rep(seq_len(K), length.out = nrow(dat)))
  nu <- data.frame(Q1 = rep(NA_real_, nrow(dat)), Q0 = NA_real_, g = NA_real_, g_raw = NA_real_)
  for (k in seq_len(K)) nu[fold == k, ] <- nhefs_nuisance(dat[fold != k, ], dat[fold == k, ], learner, bounds)
  nu
}
nhefs_panel <- function(dat, nu, label) rbind(
  data.frame(estimator = "g-computation", scheme = label, est = est_plugin(nu), se = NA, lo = NA, hi = NA),
  data.frame(estimator = "IPTW (Hajek)",  scheme = label, est = est_ipw(dat, nu), se = NA, lo = NA, hi = NA),
  data.frame(estimator = "AIPW",          scheme = label, t(est_aipw(dat, nu))),
  data.frame(estimator = "TMLE",          scheme = label, t(est_tmle(dat, nu)[1:4])))
nhefs_X <- function(dat) {                                    # numeric covariates (dummies) for balance checks
  X <- as.data.frame(model.matrix(~ sex + race + age + education + smokeintensity + smokeyrs + exercise + active + wt71 - 1, data = dat))
  X[, setdiff(names(X), "sex0"), drop = FALSE]
}
