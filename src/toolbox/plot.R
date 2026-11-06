# ---- plotting helpers (base graphics only) ----
COL <- list(red = "#D9480F", teal = "#0B7285", ink = "#1C1B19", muted = "#6B665E",
            gold = "#C98A0C", plum = "#7B3F8C", rule = "#DDD6CA")
lab_par <- function(...) par(mar = c(4.5, 5, 1.5, 1), las = 1, bty = "n", col.axis = COL$muted, ...)

overlap_plot <- function(g_raw, A, xlab = "estimated propensity score  g-hat(X)", labels = c("A = 1", "A = 0"), breaks = 40) {
  br <- seq(0, 1, length.out = breaks + 1)                    # mirrored histogram: treated up, control down
  h1 <- hist(pmin(pmax(g_raw[A == 1], 0), 1), breaks = br, plot = FALSE)$counts
  h0 <- hist(pmin(pmax(g_raw[A == 0], 0), 1), breaks = br, plot = FALSE)$counts
  lab_par()
  plot(NA, xlim = c(0, 1), ylim = c(-max(h0), max(h1)), axes = FALSE, xlab = xlab, ylab = "count")
  axis(1); yt <- pretty(c(-max(h0), max(h1))); axis(2, at = yt, labels = abs(yt))
  rect(br[-length(br)], 0, br[-1], h1, col = adjustcolor(COL$red, .8), border = "white")
  rect(br[-length(br)], 0, br[-1], -h0, col = adjustcolor(COL$teal, .8), border = "white")
  abline(h = 0)
  text(0.98, max(h1) * .85, labels[1], adj = 1, col = COL$red, font = 2)
  text(0.98, -max(h0) * .85, labels[2], adj = 1, col = COL$teal, font = 2)
}

forest_plot <- function(lab, est, lo = NA, hi = NA, ref = NULL, ref_lab = "", col = COL$ink, xlab = "estimate", xlim = NULL) {
  n <- length(est); y <- n:1
  rng <- range(c(est, lo, hi, ref), na.rm = TRUE)
  if (is.null(xlim)) xlim <- rng + c(-1, 1) * diff(rng) * 0.08
  op <- par(mar = c(4.5, min(22, max(8, 0.62 * max(nchar(lab)) + 1)), 1.5, 1), las = 1, bty = "n", col.axis = COL$muted)
  on.exit(par(op))
  plot(NA, xlim = xlim, ylim = c(0.5, n + 0.5), axes = FALSE, xlab = xlab, ylab = "")
  abline(h = y, col = "#F0EBE3")
  if (!is.null(ref)) {
    abline(v = ref, lty = 2, col = COL$muted); pos <- (ref[1] - xlim[1]) / diff(xlim)
    mtext(ref_lab, 3, at = ref[1], adj = if (pos > 0.7) 1 else if (pos < 0.3) 0 else 0.5, cex = .8, col = COL$muted)
  }
  ok <- !is.na(lo)
  segments(lo[ok], y[ok], hi[ok], y[ok], lwd = 3, col = rep_len(col, n)[ok])
  points(est, y, pch = 19, cex = 1.5, col = rep_len(col, n))
  axis(1); axis(2, at = y, labels = lab, tick = FALSE, col.axis = COL$ink)
}
