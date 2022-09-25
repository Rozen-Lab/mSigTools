test_that("test internal_matches", {
  tmp.dd <- matrix(
    c(0.99, 0.95, 0.89, 0.91),
    nrow = 2
  )
  rownames(tmp.dd) <- c("x1", "x2")
  colnames(tmp.dd) <- c("ref1", "ref2")
  tmp.dd <- 1 - tmp.dd
  rr <- internal_matches(tmp.dd, cutoff = 0.1)
  expect_equal(
    rr,
    list(
      table = structure(
        list(
          x1 = c("x1", "x2"), x2 = c("ref1", "ref2"),
          dist = c(0.01, 0.09)
        ),
        row.names = 1:2, class = "data.frame"
      ),
      orig.matrix =
        structure(c(0.01, 0.05, 0.11, 0.09),
          .Dim = c(2L, 2L),
          .Dimnames = list(c("x1", "x2"), c("ref1", "ref2"))
        ),
      modified.matrix =
        structure(c(0.01, 0.05, 9e+99, 0.09),
          .Dim = c(2L, 2L),
          .Dimnames = list(c("x1", "x2"), c("ref1", "ref2"))
        )
    )
  )
})
