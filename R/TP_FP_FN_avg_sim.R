# Functions to find best matches (by cosine similarity) between
# sets of mutational signatures.

#' Return the numbers of true positives (TP), false positives (FP), 
#' false negatives (FN), 
#' and average cosine similarity between extracted
#' and ground truth signatures.
#' 
#' @details Match signatures in \code{extracted.sigs} to
#'    signatures in \code{ground.truth.sigs} using the function
#'    \code{\link[clue]{solve_LSAP}}, which uses the 
#'    "Hungarian" (a.k.a "Kuhn–Munkres") algorithm
#'    \url{https://en.wikipedia.org/wiki/Hungarian_algorithm},
#'    which optimizes the total cost associated with the links
#'    between nodes.
#'    The function first computes the
#'    all-pairs cosine similarity matrix between the two
#'    sets of signatures, then converts cosine similarities
#'    to cosine distances (including \code{similarity.cutoff})
#'    by subtracting from 1, then
#'    sets distances > the converted cutoff to very large values.
#'    It then applies \code{\link[clue]{solve_LSAP}} to the resulting
#'    matrix to compute an optimal matching between 
#'    \code{extracted.sigs} and \code{ground.truth.sigs}.
#' 
#' @param extracted.sigs Mutational signatures discovered by some analysis.
#'    A numerical-matrix-like object with columns as signatures.
#' 
#' @param ground.truth.sigs Ground-truth mutational signatures from
#'    a synthetic data set. A numerical-matrix-like object with columns
#'    as signatures.
#'    
#' @param similarity.cutoff A signature in \code{ground.truth.sigs}
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
#' * \code{FN} The number of false negative ground-truth signatures.
#' 
#' * \code{avg.cos.sim} Average cosine similarity of
#'     true positives to their matching ground
#'     truth signatures.
#'     
#' * \code{table} Table of extracted signature name,
#'     ground-truth signature name, and associated 
#'     cosine similarity.
#'
#' * \code{sim.matrix} The similarity matrix corresponding
#'     to the input signatures.
#' 
#' @examples
#' ex.sigs <- matrix(c(0.2, 0.8, 0.3, 0.7, 0.6, 0.4), nrow = 2)
#' colnames(ex.sigs) <- c("ex1", "ex2", "ex3")
#' gt.sigs <- matrix(c(0.21, 0.79, 0.19, 0.81), nrow = 2)
#' colnames(gt.sigs) <- c("gt1", "gt2")
#' TP_FP_FN_avg_sim(extracted.sigs     = ex.sigs,
#'                  ground.truth.sigs = gt.sigs,
#'                  similarity.cutoff = .9)

TP_FP_FN_avg_sim <- 
  function(extracted.sigs, ground.truth.sigs, similarity.cutoff = 0.9) {
  tt.and.matrix <- 
    match_two_sig_sets(extracted.sigs, 
                       ground.truth.sigs, 
                       cutoff = similarity.cutoff)
  # browser()
  tt <- tt.and.matrix$table
  TP.sigs <- tt[ , 1]
  FP.sigs <- setdiff(colnames(extracted.sigs), TP.sigs)
  FN.sigs <- setdiff(colnames(ground.truth.sigs), tt[ , 2])
  tt[ , 3] <- 1 - tt[ , 3]
  colnames(tt) <- c("ex.sig", "gt.sig", "sim")
  return(list(TP          = length(TP.sigs),
              FP          = length(FP.sigs),
              FN          = length(FN.sigs),
              avg.cos.sim =  mean(tt[ , 3]),
              table       = tt,
              sim.matrix  = 1 - tt.and.matrix$orig.matrix))
}
