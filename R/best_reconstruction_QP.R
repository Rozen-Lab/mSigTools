best_reconstruction_QP <- function(target.sig,
                                   sig.universe,
                                   max.subset.size = NULL,
                                   measure         = "cosine") {
  cnames <- colnames(sig.universe)
  if (is.null(cnames)) {
    cnames <- paste("v", 1:ncol(sig.universe))
    colnames(sig.universe) <- cnames
  }
  cnames.subsets <- sets::gset(sets::gset(cnames))
  if (!is.null(max.subset.size)) {
    if (length(cnames) >= max.subset.size) {
      cnames.subsets <- sets::gset_combn(cnames, max.subset.size)
    }
  }

  # NOT DONE:
  # Need to apply best_reconstruction_QP to all subsets in cnames.subsets
  # Calculate measure between reconstructed and target.sig
  # Return a list of all subsets, all subsets cosine, all subsets euclidean
  # best subset, best subsets euclidean, cosine... etc


}
