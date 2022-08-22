#' Find an optimal matching between two sets of signatures subject to a maximum distance.
#'
#' @param x1 A numerical-matrix-like object with columns as signatures.
#'
#' @param x2 A numerical-matrix-like object with columns as signatures.
#'   Needs to have the same number of rows as \code{x1}.
#'
#' @param method As for the \code{\link[philentropy]{distance}} function in package
#'   \code{philenropy}.
#'
#' @param convert.sim.to.dist If \code{method} specifies a similarity
#'   rather than a distance, use this function to convert the
#'   similarity to a distance.
#'
#' @param cutoff A maximum distance or minimum similarity over which to
#'   pair signatures between \code{x1} and \code{x2}.
#'
#' @details Match signatures between \code{x1} and \code{x2}
#'     using the function
#'    \code{\link[clue]{solve_LSAP}}, which uses the
#'    "Hungarian" (a.k.a "Kuhn–Munkres") algorithm
#'    \url{https://en.wikipedia.org/wiki/Hungarian_algorithm},
#'    which optimizes the total cost associated with the links
#'    between nodes.
#'    The functions converts similarities to distances,
#'    and generates a distance matrix between the two
#'    sets of signatures.
#'    It sets distances > \code{cutoff} to very large values.
#'    It then applies \code{\link[clue]{solve_LSAP}} to the resulting
#'    matrix to compute a matching between
#'    \code{x1} and \code{x2} that minimizes the sum of the
#'    distances.
#'
#' @export
#'
#' @examples
#' ex.sigs <- matrix(c(0.2, 0.8, 0.3, 0.7, 0.6, 0.4), nrow = 2)
#' colnames(ex.sigs) <- c("ex1", "ex2", "ex3")
#' gt.sigs <- matrix(c(0.21, 0.79, 0.19, 0.81), nrow = 2)
#' colnames(gt.sigs) <- c("gt1", "gt2")
#' match_two_sig_sets(ex.sigs, gt.sigs, cutoff = .9)
#'
match_two_sig_sets <-
  function(x1,
           x2,
           method = "cosine",
           convert.sim.to.dist = function(x) {
             return(1 - x)
           },
           cutoff = 0.9) {
    # browser()
    dd <- sig_dist_matrix(x1, x2, method = method)
    if (!is.null(convert.sim.to.dist)) {
      dd <- convert.sim.to.dist(dd)
      cutoff <- convert.sim.to.dist(cutoff)
    }

    return(internal_matches(dd, cutoff))
  }
