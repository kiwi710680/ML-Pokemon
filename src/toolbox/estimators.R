# ---- estimators: one set of nuisance predictions -> different answers ----
# Notation (same as the slides): O = (X, A, Y)
#   Q(a, X) = E[Y | A = a, X]            outcome regression   -> nu$Q1, nu$Q0
#   g(X)    = P(A = 1 | X)               propensity score     -> nu$g
#   H(A, X) = A / g - (1 - A) / (1 - g)  clever covariate
#   D(O; g, Q, psi) = H (Y - Q_A) + Q1 - Q0 - psi   efficient influence function of the ATE
expit <- function(x) 1 / (1 + exp(-x))
bound <- function(p, lo = 0.025, hi = 0.975) pmin(pmax(p, lo), hi)

est_plugin <- function(nu) mean(nu$Q1 - nu$Q0)               # g-computation: predict twice, subtract, average

est_ipw_ht <- function(dat, nu)                               # Horvitz-Thompson IPTW: divide by n
  mean(dat$A * dat$Y / nu$g - (1 - dat$A) * dat$Y / (1 - nu$g))

est_ipw <- function(dat, nu) {                                # Hajek (stabilized) IPTW: divide by the sum of weights
  w1 <- dat$A / nu$g; w0 <- (1 - dat$A) / (1 - nu$g)
  sum(w1 * dat$Y) / sum(w1) - sum(w0 * dat$Y) / sum(w0)
}

eif <- function(dat, nu, psi) {
  H  <- dat$A / nu$g - (1 - dat$A) / (1 - nu$g)
  QA <- ifelse(dat$A == 1, nu$Q1, nu$Q0)
  H * (dat$Y - QA) + nu$Q1 - nu$Q0 - psi
}

wald <- function(psi, D) {                                    # SE from the EIF: sqrt(mean(D^2) / n)
  se <- sqrt(mean(D^2) / length(D))
  c(est = psi, se = se, lo = psi - 1.96 * se, hi = psi + 1.96 * se)
}

est_aipw <- function(dat, nu) {                               # one-step: psi_plug + P_n D  (solves P_n D = 0)
  psi <- mean(eif(dat, nu, 0))
  wald(psi, eif(dat, nu, psi))
}

est_tmle <- function(dat, nu, ybounds = range(dat$Y)) {       # targeting: move Q-hat along H until P_n D = 0
  lo <- ybounds[1]; hi <- ybounds[2]
  sc <- function(y) pmin(pmax((y - lo) / (hi - lo), 1e-6), 1 - 1e-6)
  Ys <- (dat$Y - lo) / (hi - lo)
  Q1s <- sc(nu$Q1); Q0s <- sc(nu$Q0); QAs <- ifelse(dat$A == 1, Q1s, Q0s)
  H  <- dat$A / nu$g - (1 - dat$A) / (1 - nu$g)
  fl <- suppressWarnings(glm(Ys ~ -1 + H, offset = qlogis(QAs), family = quasibinomial()))
  eps <- unname(coef(fl))
  Q1t <- expit(qlogis(Q1s) + eps / nu$g)
  Q0t <- expit(qlogis(Q0s) - eps / (1 - nu$g))
  nu_t <- data.frame(Q1 = lo + (hi - lo) * Q1t, Q0 = lo + (hi - lo) * Q0t, g = nu$g)
  psi <- mean(nu_t$Q1 - nu_t$Q0)
  c(wald(psi, eif(dat, nu_t, psi)), eps = eps)
}

weight_summary <- function(dat, nu) {                         # largest weight and effective sample size (ESS)
  w <- ifelse(dat$A == 1, 1 / nu$g, 1 / (1 - nu$g))
  c(max_weight = max(w),
    ESS_treated = sum(w[dat$A == 1])^2 / sum(w[dat$A == 1]^2), n_treated = sum(dat$A),
    ESS_control = sum(w[dat$A == 0])^2 / sum(w[dat$A == 0]^2), n_control = sum(1 - dat$A))
}
