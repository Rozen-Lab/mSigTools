#' Find "best" reconstruction of a target signature or spectrum from a set of signatures.
#'
#' @param target.sig The signature or spectrum to reconstruct;
#'   a non-negative numeric vector or 1-column matrix-like object.
#'
#' @param sig.universe The universe of signatures from which to reconstruct
#'   \code{target.sig}; a non-negative numeric matrix-like object with
#'   \code{nrow(sig.universe) == length(target.sig)}. The sums of each column
#'   must be 1. Must not contain duplicate columns or have other
#'   non-unique quadratic programming solutions (not checked, but will generate
#'   an error from \code{\link[quadprog]{solve.QP}} in package `quadprog`).
#'
#' @param max.subset.size Maximum number of signatures to use to
#'   reconstruct \code{target.sig}.
#'
#' @param method As in \code{\link[philentropy]{dist_one_one}} in
#'   package \code{philentropy}, and used only
#'   to find the final "best" reconstruction. The optimized exposures from which
#'   to selected the "best" reconstruction are calculated using
#'   \code{\link{optimize_exposure_QP}}, which uses \code{\link[quadprog]{solve.QP}}
#'   in package \code{quadprog}.
#'
#' @param trim.less.than After optimizing exposures with
#'   \code{\link{optimize_exposure_QP}}, remove exposures less than
#'   \code{trim.less.than} and then re-optimize.
#'
#' @md
#'
#' @return A list with elements:
#'
#' * \code{optimized.exposure} A numerical vector of the exposures that
#'   give the "best" reconstruction. This vector is empty if there is
#'   an error.
#'
#' * \code{similarity} The similarity between the \code{reconstruction}
#'   (see below) and \code{target.sig} according to the distance
#'   or similarity provided by the \code{method} argument.
#'
#' * \code{method} The value specified for the `method` argument,
#'   or an error message if `optimize.exposure` is empty.
#'
#' * \code{reconstruction} The reconstruction of `target.sig` according to
#'   `optimized.exposure`.
#'
#' @export
#'
#' @details
#'   This function should be fast if you do not specify \code{max.subset.size},
#'   but it will be combinatorially slow if \code{max.subset.size} is large
#'   and \code{trim.less.than} is small or negative. So do not do that.
#'   If `max.subset.size` is `NULL`, then the function just uses \code{\link{optimize_exposure_QP}}.
#'   and then excludes exposures < `trim.less.than`, and then re-runs
#'   \code{\link{optimize_exposure_QP}}. Otherwise, after excluding
#'   exposures < `trim.less.than`, then the function runs \code{\link{optimize_exposure_QP}} on
#'   subsets of signatures of size <= `max.subset.size`, removes exposures < `trim.less.than`,
#'   reruns \code{\link{optimize_exposure_QP}}, calculates the reconstruction and
#'   similarity between the reconstruction and the `target.sig` and returns the information for
#'   the exposures that have the greatest similarity.
#'
#' @examples
#' set.seed(888)
#' sig.u <-
#'   do.call(
#'     cbind,
#'     lapply(1:6, function(x) {
#'       col <- runif(n = 96)
#'       col / sum(col)
#'     })
#'   )
#' rr <- find_best_reconstruction_QP(
#'   target.sig = sig.u[, 1, drop = FALSE],
#'   sig.universe = sig.u[, 2:6]
#' )
#' names(rr)
#' rr$optimized.exposure
#' rr$similarity
#' rr <- find_best_reconstruction_QP(
#'   target.sig = sig.u[, 1, drop = FALSE],
#'   sig.universe = sig.u[, 2:6],
#'   max.subset.size = 3
#' )
#' rr$optimized.exposure
#' rr$similarity
#'
find_best_reconstruction_QP <- function(target.sig,
                                        sig.universe,
                                        max.subset.size = NULL,
                                        method = "cosine",
                                        trim.less.than = 1e-10) {
  if (!isTRUE(
    all.equal.numeric(colSums(sig.universe),
      rep(1, ncol(sig.universe)),
      check.attributes = FALSE
    )
  )) {
    return(list(
      optimized.exposure = numeric(),
      method = "Error: all signature.universe columns must sum to 1"
    ))
  }

  if (is.null(colnames(sig.universe))) {
    colnames(sig.universe) <- paste("v", 1:ncol(sig.universe), sep = "")
  }

  optimize.etc <- function(target.sig, sig.u) {
    optimized.exposure <- optimize_exposure_QP(target.sig, sig.u)
    to.remove <- which(optimized.exposure < trim.less.than)
    if (length(to.remove) > 1) {
      optimized.exposure <- optimized.exposure[-to.remove]
    }
    used.sigs <- sig.u[, names(optimized.exposure), drop = FALSE]
    if (length(to.remove) > 1) {
      optimized.exposure <- optimize_exposure_QP(target.sig, used.sigs)
    }

    recon <- used.sigs %*% optimized.exposure

    dist <- philentropy::dist_one_one(target.sig, recon, method = method)

    return(list(
      optimized.exposure = optimized.exposure,
      similarity = dist,
      method = method,
      reconstruction = recon
    ))
  }

  result1 <- optimize.etc(target.sig, sig.universe)

  if (is.null(max.subset.size)) {
    return(result1)
  }

  cnames <- names(result1[["optimized.exposure"]])

  if (length(cnames) < max.subset.size) {
    max.subset.size <- length(cnames)
  }
  xx <- lapply(
    1:max.subset.size,
    function(m) {
      xxx <- sets::gset_combn(cnames, m = m)
      return(xxx)
    }
  )
  xx <- do.call(sets::gset_union, xx)

  one.sig.set <- function(sig.name.set) {
    # browser()
    rrr <- optimize.etc(
      target.sig,
      sig.universe[, as.character(sig.name.set), drop = FALSE]
    )
    return(rrr)
  }

  rr <- lapply(X = xx, FUN = one.sig.set)
  similarities <- unlist(lapply(rr, `[[`, "similarity"))
  best <- which(similarities == max(similarities))
  return(rr[[best]])
}
