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
#' @return A numeric matrix with dimensions
#'   \code{ncol(x1)} X \code{ncol(x2)}.
#'   Each element represents the distance or
#'   similarity (depending on \code{method})
#'   between a column in \code{x1} and a column in \code{x2}.
#'
#' @export
#'
#' @examples
#' ex.sigs <- matrix(c(0.2, 0.8, 0.3, 0.7, 0.4, 0.6), nrow = 2)
#' colnames(ex.sigs) <- c("ex1", "ex2", "ex3")
#' ref.sigs <- matrix(c(0.21, 0.79, 0.19, 0.81), nrow = 2)
#' colnames(ref.sigs) <- c("ref1", "ref2")
#' sig_dist_matrix(ex.sigs, ref.sigs)
#'
sig_dist_matrix <- function(x1, x2, method = "cosine") {
  mm <- cbind(x1, x2)
  dd <- suppressMessages(
    philentropy::distance(t(mm), method = method, use.row.names = TRUE)
  )

  # In some very rare cases, some cosine similarities in dd may be slightly
  # greater than 1 e.g. 1.00000000000000022204460492503130808472633361816406250
  # To avoid error later, we change those values to 1
  if (method == "cosine") {
    dd[dd > 1] <- 1
  }

  dd2 <- dd[1:ncol(x1), , drop = FALSE] # Use the rows that represent the elements of x1
  dd3 <- dd2[, -(1:ncol(x1)), drop = FALSE] # Use the columns that represent elements of x2
  return(dd3)
}
