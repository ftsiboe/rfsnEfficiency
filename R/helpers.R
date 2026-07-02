# Small robust summary helpers shared across the scoring pipeline. `trim_mean()`
# is the public workhorse; the weighted-quantile / median / flag-rate helpers
# below back the weighted flavours of trim_mean() and summarise_scores() and are
# kept internal.

#' Trimmed (optionally weighted) mean
#'
#' Mean of \code{x} after discarding the tails below the \code{lo} and above the
#' \code{hi} quantiles. This guards headline score summaries against the handful
#' of producers whose near-zero baseline revenue makes their indices explode and
#' dominate a plain mean (the same trimming used in Tsiboe et al. 2025).
#' Non-finite values are dropped before trimming. The tails are cut at the
#' (unweighted) \code{lo}/\code{hi} sample quantiles of \code{x} -- the extreme
#' values are extreme regardless of weight -- and, when \code{weights} is
#' supplied, the retained values are then averaged with those weights. Equal
#' weights therefore reproduce the unweighted trimmed mean exactly.
#'
#' @param x numeric vector to summarise.
#' @param lo,hi lower/upper quantile cut points (defaults 0.005 / 0.995).
#' @param weights optional numeric vector of non-negative weights, the same
#'   length as \code{x}. \code{NULL} (default) gives the unweighted trimmed mean.
#' @return length-one numeric; \code{NA_real_} when no finite values remain.
#' @examples
#' trim_mean(c(1, 2, 3, 4, 1000))
#' trim_mean(c(1, 2, 3, 4, 1000), weights = c(1, 1, 1, 1, 5))
#' @export
trim_mean <- function(x, lo = 0.005, hi = 0.995, weights = NULL) {
  keep <- is.finite(x)
  if (!is.null(weights)) keep <- keep & is.finite(weights) & weights > 0
  x <- x[keep]
  if (!length(x)) return(NA_real_)
  q <- stats::quantile(x, c(lo, hi), na.rm = TRUE)
  inb <- x >= q[1] & x <= q[2]
  if (is.null(weights)) return(mean(x[inb]))
  weights <- weights[keep]
  stats::weighted.mean(x[inb], weights[inb])
}

# Weighted sample quantile (Hazen / type-5 style): cumulative weight positions
# (cumsum(w) - w/2)/sum(w) with linear interpolation, clamped to the extreme
# observations outside the weighted range. Backs the weighted trim_mean() and
# summarise_scores() medians. Internal.
weighted_quantile <- function(x, w, probs) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  x <- x[keep]; w <- w[keep]
  if (!length(x)) return(rep(NA_real_, length(probs)))
  # approx() needs >= 2 points; a single distinct value is its own quantile.
  if (length(unique(x)) < 2L) return(rep(x[1], length(probs)))
  o <- order(x); x <- x[o]; w <- w[o]
  cw <- (cumsum(w) - 0.5 * w) / sum(w)
  stats::approx(cw, x, xout = probs, rule = 2, ties = "ordered")$y
}

# Weighted median (NULL weights -> ordinary median). Internal.
robust_median <- function(x, weights = NULL) {
  if (is.null(weights)) return(stats::median(x, na.rm = TRUE))
  weighted_quantile(x, weights, 0.5)
}

# Weighted mean of a logical / 0-1 flag (NULL weights -> ordinary mean).
# Internal.
flag_rate <- function(flag, weights = NULL) {
  flag <- as.numeric(flag)
  if (is.null(weights)) return(mean(flag, na.rm = TRUE))
  keep <- is.finite(flag) & is.finite(weights) & weights > 0
  if (!any(keep)) return(NA_real_)
  stats::weighted.mean(flag[keep], weights[keep])
}
