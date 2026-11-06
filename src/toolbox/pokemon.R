# ---- Pokemon CP (classroom example; train/test split rebuilt from the slides) ----
SPECIES <- c("Pidgey", "Weedle", "Caterpie", "Eevee")
lee_err <- function(y, yhat) sqrt(sum((y - yhat)^2)) / length(y)   # the classroom "Average Error" is sqrt(SSE) / N
read_pokemon <- function(file) { d <- read.csv(file); d$species <- factor(d$species, levels = SPECIES); d }

X_species <- function(d) {                                    # each species its own intercept and slope (4 + 4)
  S <- sapply(SPECIES, function(s) as.numeric(d$species == s)); cbind(S, S * d$cp)
}
X_extended <- function(d) {                                   # 18 parameters: species x (1, cp, cp^2) + hp, height, weight and squares
  S <- sapply(SPECIES, function(s) as.numeric(d$species == s))
  cbind(S, S * d$cp, S * d$cp^2, d$hp, d$hp^2, d$height, d$height^2, d$weight, d$weight^2)
}
ridge_fit <- function(X, y, lambda, n_free = 4) {             # L = sum (y - yhat)^2 + lambda sum w^2; species intercepts not penalized
  p <- ncol(X); pen <- diag(c(rep(0, n_free), rep(sqrt(lambda), p - n_free)), p)
  qr.coef(qr(rbind(X, pen[(n_free + 1):p, , drop = FALSE])), c(y, rep(0, p - n_free)))   # augmented least squares
}
knn_cp <- function(train, newdata, k) sapply(seq_len(nrow(newdata)), function(i) {
  tr <- train[train$species == newdata$species[i], ]
  mean(tr$cp_new[order(abs(tr$cp - newdata$cp[i]))[seq_len(min(k, nrow(tr)))]])
})
pokemon_candidates <- list(                                   # each candidate: training data -> predictions on new data
  "linear (cp only)" = function(tr, nd) predict(lm(cp_new ~ cp, tr), nd),
  "species-linear"   = function(tr, nd) X_species(nd) %*% qr.coef(qr(X_species(tr)), tr$cp_new),
  "18-param OLS"     = function(tr, nd) X_extended(nd) %*% ridge_fit(X_extended(tr), tr$cp_new, 0),
  "18-param ridge"   = function(tr, nd) X_extended(nd) %*% ridge_fit(X_extended(tr), tr$cp_new, 1e4),
  "kNN (k = 3)"      = function(tr, nd) knn_cp(tr, nd, 3))
strat_folds <- function(species, V = 5, seed = 1) {           # V folds, stratified by species
  set.seed(seed); f <- integer(length(species))
  for (s in unique(species)) { id <- which(species == s); f[id] <- sample(rep(seq_len(V), length.out = length(id))) }
  f
}
convex_weights <- function(Z, y) {                            # non-negative weights summing to 1 that minimize CV squared error
  obj <- function(b) { w <- exp(c(0, b)); w <- w / sum(w); mean((y - Z %*% w)^2) }
  b <- optim(rep(0, ncol(Z) - 1), obj, method = "BFGS")$par
  w <- exp(c(0, b)); setNames(w / sum(w), colnames(Z))
}
