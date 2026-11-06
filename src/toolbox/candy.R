# ---- magic candy: simulated data, so the truth (Q, g, psi) is known ----
species_levels <- c("Pidgey", "Weedle", "Caterpie", "Eevee")

simulate_candy <- function(n = 800, seed = NULL, strength = 1) {
  if (!is.null(seed)) set.seed(seed)
  species <- factor(sample(species_levels, n, replace = TRUE, prob = c(.40, .20, .15, .25)), levels = species_levels)
  top <- c(Pidgey = 450, Weedle = 260, Caterpie = 260, Eevee = 650)[as.character(species)]
  cp  <- round(10 + top * rbeta(n, 2, 2.5))
  mult <- c(Pidgey = 1.85, Weedle = 1.10, Caterpie = 1.08, Eevee = 2.60)[as.character(species)]
  f0   <- mult * cp * (1 - cp / 2600)                         # evolution curve, bends at high CP
  # trainers prefer strong Pokemon and Eevee: confounding. Larger strength -> g closer to 0 or 1
  g0 <- expit(strength * (-1.8 + 3.2 * (cp / top)) + 0.5 * (species == "Eevee"))
  A  <- rbinom(n, 1, g0)
  tau <- 20 + 0.30 * cp                                       # candy effect: stronger Pokemon gain more
  Y1 <- f0 + tau + rnorm(n, 0, 30)
  Y0 <- f0 + rnorm(n, 0, 30)
  data.frame(species, cp, A, Y = ifelse(A == 1, Y1, Y0), g0, Q1 = f0 + tau, Q0 = f0, Y1, Y0)
}

candy_truth <- function(n = 2e6, seed = 1) {                  # population ATE = E[tau(X)]
  set.seed(seed)
  species <- sample(species_levels, n, replace = TRUE, prob = c(.40, .20, .15, .25))
  top <- c(Pidgey = 450, Weedle = 260, Caterpie = 260, Eevee = 650)[species]
  mean(20 + 0.30 * round(10 + top * rbeta(n, 2, 2.5)))
}
candy_population <- function(n = 20000, seed = 2027, strength = 1) simulate_candy(n, seed, strength)

# ---- nuisance learners ----
knn_Q <- function(train, newdata, a, k = 10) {                # same species, same arm, k nearest CP, average Y
  out <- rep(NA_real_, nrow(newdata))
  for (s in levels(newdata$species)) {
    tr <- train[train$species == s & train$A == a, ]
    id <- which(newdata$species == s)
    if (length(id) == 0) next
    if (nrow(tr) == 0) { out[id] <- mean(train$Y[train$A == a]); next }
    kk <- min(k, nrow(tr))
    D  <- abs(outer(newdata$cp[id], tr$cp, "-"))
    out[id] <- apply(D, 1, function(d) mean(tr$Y[order(d)[seq_len(kk)]]))
  }
  out
}
lin_Q <- function(train, newdata, a) predict(lm(Y ~ A + species + cp, data = train), transform(newdata, A = a))  # one straight line (slide 62)
logit_g <- function(train, newdata) {                        # each species its own intercept and slope
  fit <- glm(A ~ species + species:cp, family = binomial, data = train)
  bound(predict(fit, newdata, type = "response"))
}

fit_nuisance <- function(train, newdata, Qlearner = "knn", k = 10) {
  if (Qlearner == "knn") { Q1 <- knn_Q(train, newdata, 1, k); Q0 <- knn_Q(train, newdata, 0, k) }
  else { Q1 <- lin_Q(train, newdata, 1); Q0 <- lin_Q(train, newdata, 0) }
  data.frame(Q1 = Q1, Q0 = Q0, g = logit_g(train, newdata))
}
nuisance_full <- function(dat, ...) fit_nuisance(dat, dat, ...)          # train and predict on the same Pokemon
nuisance_crossfit <- function(dat, K = 5, seed = NULL, ...) {             # each prediction comes from the other folds
  if (!is.null(seed)) set.seed(seed)
  fold <- sample(rep(seq_len(K), length.out = nrow(dat)))
  nu <- data.frame(Q1 = NA_real_, Q0 = NA_real_, g = NA_real_)[rep(1, nrow(dat)), ]
  for (k in seq_len(K)) { te <- fold == k; nu[te, ] <- fit_nuisance(dat[!te, ], dat[te, ], ...) }
  rownames(nu) <- NULL
  nu
}

all_estimates <- function(dat, k = 10, K = 5) {
  nf <- nuisance_full(dat, k = k); nc <- nuisance_crossfit(dat, K = K, k = k)
  rbind(naive       = c(est = mean(dat$Y[dat$A == 1]) - mean(dat$Y[dat$A == 0]), se = NA, lo = NA, hi = NA),
        linear      = c(est = unname(coef(lm(Y ~ A + species + cp, data = dat))["A"]), se = NA, lo = NA, hi = NA),
        plugin_knn  = c(est = est_plugin(nf), se = NA, lo = NA, hi = NA),
        ipw         = c(est = est_ipw(dat, nf), se = NA, lo = NA, hi = NA),
        aipw_full   = est_aipw(dat, nf),
        tmle_full   = est_tmle(dat, nf)[1:4],
        aipw_cf_DML = est_aipw(dat, nc),
        tmle_cf     = est_tmle(dat, nc)[1:4])
}

# ---- only a simulation can do this: split the error into pieces ----
#   plug-in: psi_plug - psi = sampling + first-order bias + empirical process
#   AIPW   : psi_AIPW - psi = (P_n - P)D + R2 + empirical process
# "P" (population mean) is approximated by a large population `pop`
decompose_candy <- function(dat, pop, k = 10, scheme = c("full", "crossfit"), K = 5, seed = 1) {
  scheme <- match.arg(scheme)
  psi <- candy_truth(); n <- nrow(dat)
  set.seed(seed)
  fold <- if (scheme == "full") rep(1L, n) else sample(rep(seq_len(K), length.out = n))
  nu <- data.frame(Q1 = numeric(n), Q0 = numeric(n), g = numeric(n))
  first_order <- 0; R2 <- 0; gerr <- 0; qerr <- 0
  for (f in unique(fold)) {
    te <- fold == f
    tr <- if (scheme == "full") rep(TRUE, n) else !te
    nu[te, ] <- fit_nuisance(dat[tr, ], dat[te, ], k = k)
    np <- fit_nuisance(dat[tr, ], pop, k = k)                 # the same model, evaluated on the population
    w <- mean(te)
    r2_unit <- (np$g - pop$g0) / np$g * (np$Q1 - pop$Q1) + (np$g - pop$g0) / (1 - np$g) * (np$Q0 - pop$Q0)
    fo_unit <- (np$Q1 - pop$Q1) - (np$Q0 - pop$Q0)
    first_order <- first_order + w * mean(fo_unit)
    R2 <- R2 + w * mean(r2_unit)
    if (f == fold[1]) units <- data.frame(species = pop$species, cp = pop$cp, g_err = np$g - pop$g0,
                                          Q1_err = np$Q1 - pop$Q1, Q0_err = np$Q0 - pop$Q0,
                                          R2_unit = r2_unit, first_order_unit = fo_unit)
    gerr <- gerr + w * mean((np$g - pop$g0)^2)
    qerr <- qerr + w * mean(((np$Q1 - pop$Q1)^2 + (np$Q0 - pop$Q0)^2) / 2)
  }
  plug <- mean(nu$Q1 - nu$Q0)
  aipw <- est_aipw(dat, nu)[["est"]]
  samp_plug <- mean(dat$Q1 - dat$Q0) - psi
  samp_aipw <- mean(eif(dat, data.frame(Q1 = dat$Q1, Q0 = dat$Q0, g = dat$g0), psi))   # (P_n - P)D, since PD = 0
  list(plugin = c(total = plug - psi, sampling = samp_plug, first_order = first_order,
                  emp_process = plug - psi - samp_plug - first_order),
       aipw = c(total = aipw - psi, sampling = samp_aipw, R2 = R2, emp_process = aipw - psi - samp_aipw - R2),
       L2 = c(g = sqrt(gerr), Q = sqrt(qerr), product = sqrt(gerr) * sqrt(qerr)),
       units = units)                                         # per-Pokemon errors (first fold's model), for plots
}

# Neyman orthogonality: move g and Q away from the truth by t, compare the two moments
orthogonality_curves <- function(pop, ts = seq(-1, 1, by = 0.05), dq1 = 40, dq0 = -20, dg = 0.6) {
  g <- pop$g0; Q1 <- pop$Q1; Q0 <- pop$Q0
  psi <- mean(Q1 - Q0)
  gbar <- g + dg * g * (1 - g)
  eif_moment <- function(t, move_g, move_Q) {
    gt <- if (move_g) g + t * (gbar - g) else g
    q1 <- if (move_Q) Q1 + t * dq1 else Q1
    q0 <- if (move_Q) Q0 + t * dq0 else Q0
    mean(q1 - q0 + g / gt * (Q1 - q1) - (1 - g) / (1 - gt) * (Q0 - q0)) - psi
  }
  data.frame(t = ts, plugin_Q = ts * (dq1 - dq0),
             eif_Q_only = sapply(ts, eif_moment, move_g = FALSE, move_Q = TRUE),
             eif_g_only = sapply(ts, eif_moment, move_g = TRUE, move_Q = FALSE),
             eif_both   = sapply(ts, eif_moment, move_g = TRUE, move_Q = TRUE))
}

# smallest possible SE: sqrt(V_eff / n), V_eff = E[sigma^2/g + sigma^2/(1-g)] + Var(tau), sigma = 30
candy_se_bound <- function(pop, n) sqrt((mean(30^2 / pop$g0 + 30^2 / (1 - pop$g0)) + var(pop$Q1 - pop$Q0)) / n)
