#' Find best matches (by cosine similarity) of a set of mutational signatures to
#' a set of reference mutational signatures.
#'
#' @details Match signatures in \code{extracted.sigs} to
#'    signatures in \code{reference.sigs} using \code{\link{match_two_sig_sets}}
#'    based on cosine similarity.
#'
#' @param extracted.sigs Mutational signatures discovered by some analysis.
#'    A numerical-matrix-like object with columns as signatures.
#'
#' @param reference.sigs A numerical-matrix-like object with columns
#'    as signatures. This matrix should contain the
#'    reference mutational signatures. For example,
#'    these might be from a synthetic data set or they
#'    could be from reference set of signatures,
#'    such as the signatures at the COSMIC mutational
#'    signatures web site. See CRAN package cosmicsig.
#'
#' @param similarity.cutoff A signature in \code{reference.sigs}
#'    must be matched
#'    by \code{>= similarity.cutoff} by a signature in \code{extracted.sigs}
#'    to be considered detected.
#'
#' @export
#'
#' @return A list with the elements
#'
#' * \code{TP} The number of true positive extracted signatures.
#'
#' * \code{FP} The number of false positive extracted signatures.
#'
#' * \code{FN} The number of false negative reference signatures.
#'
#' * \code{avg.cos.sim} The average cosine similarity of
#'     true positives to their matching reference signatures.
#'
#' * \code{table} A data.frame of extracted signatures
#'     that matched a reference signature.
#'     Each row contains the extracted signature name,
#'     the reference signature name, and the
#'     cosine similarity of the match.
#'
#' * \code{sim.matrix} The numeric distance or similarity
#'     matrix between \code{extracted.sigs} and
#'     \code{reference.sigs} as returned from
#'     \code{\link{sig_dist_matrix}}.
#'
#' * \code{unmatched.ex.sigs} The identifiers of
#'     the extracted signatures that did not match a
#'     reference signature.
#'
#' * \code{unmatched.ref.sigs} The identifiers of
#'     the reference signatures that did not match an
#'     extracted signature.
#'
#' @examples
#' ex.sigs <- matrix(c(0.2, 0.8, 0.3, 0.7, 0.6, 0.4), nrow = 2)
#' colnames(ex.sigs) <- c("ex1", "ex2", "ex3")
#' ref.sigs <- matrix(c(0.21, 0.79, 0.19, 0.81), nrow = 2)
#' colnames(ref.sigs) <- c("ref1", "ref2")
#' TP_FP_FN_avg_sim(
#'   extracted.sigs = ex.sigs,
#'   reference.sigs = ref.sigs,
#'   similarity.cutoff = .9
#' )
#'
#' @md
#'
TP_FP_FN_avg_sim <-
  function(extracted.sigs, reference.sigs, similarity.cutoff = 0.9) {
    tt.and.matrix <-
      match_two_sig_sets(
        extracted.sigs,
        reference.sigs,
        method = "cosine",
        cutoff = similarity.cutoff
      )
    # browser()
    tt <- tt.and.matrix$table
    TP.sigs <- tt[, 1]
    FP.sigs <- setdiff(colnames(extracted.sigs), TP.sigs)
    FN.sigs <- setdiff(colnames(reference.sigs), tt[, 2])
    tt[, 3] <- 1 - tt[, 3]
    colnames(tt) <- c("ex.sig", "ref.sig", "sim")
    return(list(
      TP = length(TP.sigs),
      FP = length(FP.sigs),
      FN = length(FN.sigs),
      avg.cos.sim = mean(tt[, 3]),
      table = tt,
      sim.matrix = 1 - tt.and.matrix$orig.matrix,
      unmatched.ex.sigs = FP.sigs,
      unmatched.ref.sigs = FN.sigs
    ))
  }
