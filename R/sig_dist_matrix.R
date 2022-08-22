#' Compute a matrix of distances / similarities between two sets of signatures.
#'
#' @param x1 The first set of signatures (a numerical matrix-like object
#'     in which each column is a signature).
#'
#' @param x2 The second set of signatures, similar data type to \code{x1},
#'     and must have the same number of rows as \code{x1}.
#'
#' @param method As for the \code{\link[philentropy]{distance}} function in package
#'   \code{philenropy}.
#'
#' @return A matrix with dimensions \code{ncol(x1)} X \code{ncol(x2)} with
#'   each element representing the distance or similarity (depending on \code{method})
#'   between the column in \code{x1} and a column in \code{x2}.
#'
#' @export
#'
#' @examples
#' ex.sigs <- matrix(c(0.2, 0.8, 0.3, 0.7, 0.4, 0.6), nrow = 2)
#' colnames(ex.sigs) <- c("ex1", "ex2", "ex3")
#' gt.sigs <- matrix(c(0.21, 0.79, 0.19, 0.81), nrow = 2)
#' colnames(gt.sigs) <- c("gt1", "gt2")
#' sig_dist_matrix(ex.sigs, gt.sigs)
#'
sig_dist_matrix <- function(x1, x2, method = "cosine") {
  mm <- cbind(x1, x2)
  dd <- suppressMessages(
    philentropy::distance(t(mm), method = method, use.row.names = TRUE)
  )
  dd2 <- dd[1:ncol(x1), , drop = FALSE] # Use the rows that represent the elements of x1
  dd3 <- dd2[, -(1:ncol(x1)), drop = FALSE] # Use that columns that represent elements of x2
  return(dd3)
}
