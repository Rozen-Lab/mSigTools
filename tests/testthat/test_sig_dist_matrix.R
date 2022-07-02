test_that("test sig_dist_matrix", {
  ex.sigs <- matrix(c(0.2, 0.8, 0.3, 0.7), nrow = 2)
  colnames(ex.sigs) <- c("ex1", "ex2")
  gt.sigs <- matrix(c(0.21, 0.79, 0.19, 0.81), nrow = 2)
  colnames(gt.sigs) <- c("gt1", "gt2")
  rr <- sig_dist_matrix(ex.sigs, gt.sigs)
  expect_equal(
    rr,
    structure(c(0.999889952921527, 
                0.989494716817794, 
                0.999893768683133, 
                0.98481531879193), .Dim = c(2L, 2L), 
              .Dimnames = list(c("ex1",  "ex2"), c("gt1", "gt2"))))
  ex.sigs2 <- cbind(ex.sigs, ex3 = c(0.18, 0.82))
  rr <- sig_dist_matrix(ex.sigs2, gt.sigs)
  testthat::expect_equal(
    rr,
    structure(c(0.999889952921527, 
                0.989494716817794, 
                0.999044022249416, 
                0.999893768683133, 
                0.98481531879193, 
                0.999897506866566), 
              .Dim = 3:2, 
              .Dimnames = list(c("ex1", "ex2", "ex3"), c("gt1", "gt2")))
  )
})

