#' Find "best" reconstruction of a target signature or spectrum from a set of signatures.
#'
#' @param target.sig The signature to reconstruct.
#'
#' @param sig.universe The universe of signatures from which to reconstruct
#'   \code{target.sig}.
#'
#' @param max.subset.size Maximum number of signatures to use to
#'   reconstruct \code{target.sig}.
#'
#' @param method As in \code{\link[philentropy]{dist_one_one}}, and used only
#'   to find the final "best" reconstruction. The optimized exposures from which
#'   to selected the "best" reconstruction are calculated using
#'   \code{\link{optimize_exposure_QP}}, which uses \code{\link[quadprog]{solve.QP}}.
#'
#' @param trim.less.than After optimizing exposures with
#'   \code{\link{optimize_exposure_QP}}, remove exposures less than
#'   \code{trim.less.than} and then re-optimize.
#'
#' @return A list .... TODO
#'
#' @export
#'
#' @details
#'   Should normally be fast if you do not specify \code{max.subset.size},
#'   but will be combinatorially slow if \code{max.subset.size} is large
#'   and \code{trim.less.than} is small or negative. So do not do that.
#'   TODO explain algorithm. TODO: tests
#'
#' @examples
#' set.seed(888)
#' sig.u <- do.call(cbind, lapply(1:6, function(x) runif(n = 96)))
#' rr <- best_reconstruction_QP(target.sig = sig.u[ , 1, drop = FALSE],
#'                             sig.universe = sig.u[ , 2:6])
#' names(rr)
#' rr$optimized.exposure
#' rr$similarity
#' rr <- best_reconstruction_QP(target.sig = sig.u[ , 1, drop = FALSE],
#'                             sig.universe = sig.u[ , 2:6],
#'                             max.subset.size = 3)
#' rr$optimized.exposure
#' rr$similarity

best_reconstruction_QP <- function(target.sig,
                                   sig.universe,
                                   max.subset.size = NULL,
                                   method          = "cosine",
                                   trim.less.than  = 1e-10) {

  if (is.null(colnames(sig.universe))) {
    colnames(sig.universe) <- paste("v", 1:ncol(sig.universe))
  }

  optimize.etc <- function(target.sig, sig.u) {
    optimized.exposure <- optimize_exposure_QP(target.sig, sig.u)
    to.remove <- which(optimized.exposure < trim.less.than)
    if (length(to.remove) > 1) {
      optimized.exposure <- optimized.exposure[-to.remove]
    }
    used.sigs <- sig.u[ , names(optimized.exposure), drop = FALSE]
    if (length(to.remove) > 1) {
      optimized.exposure <- optimize_exposure_QP(target.sig, used.sigs)
    }

    recon <- used.sigs %*% optimized.exposure

    dist <- philentropy::dist_one_one(target.sig, recon, method = method)

    return(list(optimized.exposure = optimized.exposure,
                similarity         = dist,
                method             = method,
                reconstruction     = recon))
  }

  result1 <- optimize.etc(target.sig, sig.universe)

  if (is.null(max.subset.size)) {
    return(result1)
  }

  cnames <- names(result1[["optimized.exposure"]])

  if (length(cnames) < max.subset.size) {
    max.subset.size <-length(cnames)
  }
  xx <- lapply(1:max.subset.size,
               function(m) {
                 xxx <- sets::gset_combn(cnames,  m = m)
                 return(xxx)
               })
  xx <- do.call(sets::gset_union, xx)

  one.sig.set <- function(sig.name.set) {
    # browser()
    rrr <- optimize.etc(
      target.sig,
      sig.universe[ , as.character(sig.name.set), drop = FALSE])
    return(rrr)
  }

  rr <- lapply(X = xx, FUN = one.sig.set)
  similarities <- unlist(lapply(rr, `[[`, "similarity"))
  best <- which(similarities == max(similarities))
  return(rr[[best]])
}

if (FALSE) {
  s96 <- cosmicsig::COSMIC_v3.2$signature$GRCh37$SBS96
  .tar <- s96[ , "SBS29", drop = F]
  .u2 <- s96[ , -which(colnames(s96) == "SBS29"), drop = FALSE]
  mSigTools:::best_reconstruction_QP(.tar, sig.universe = .u2)
}
