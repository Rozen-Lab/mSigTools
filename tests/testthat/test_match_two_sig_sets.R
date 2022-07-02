test_that("test match_two_sig_sets", {
  ex.sigs <- matrix(c(0.2, 0.8, 0.3, 0.7), nrow = 2)
  colnames(ex.sigs) <- c("ex1", "ex2")
  gt.sigs <- matrix(c(0.21, 0.79, 0.19, 0.81), nrow = 2)
  colnames(gt.sigs) <- c("gt1", "gt2")
  rr <- match_two_sig_sets(ex.sigs, gt.sigs, cutoff = .985)
  expect_equal(
    rr,
    list(table = structure(
      list(x1 = c("ex1", "ex2"), x2 = c("gt2",  "gt1"),
           dist = c(0.000106231316866934, 0.0105052831822057)),
      row.names = 1:2, class = "data.frame"),
      orig.matrix = structure(c(0.000110047078473174, 0.0105052831822057, 
                                0.000106231316866934, 0.0151846812080705),
                              .Dim = c(2L, 2L     ),
                              .Dimnames = list(c("ex1", "ex2"), c("gt1", "gt2"))),
      modified.matrix =
        structure(c(0.000110047078473174,
                    0.0105052831822057, 
                    0.000106231316866934, 9e+99), 
                  .Dim = c(2L,      2L), 
                  .Dimnames = list(c("ex1", "ex2"), c("gt1", "gt2")))))
  ex.sigs2 <- cbind(ex.sigs, ex3 = c(0.18, 0.82))
  rr <- match_two_sig_sets(ex.sigs2, gt.sigs, cutoff = .985)
  testthat::expect_equal(
    rr,
    list(table = structure(
      list(x1 = c("ex1", "ex3"), x2 = c("gt1",  "gt2"),
           dist = c(0.000110047078473174,
                    0.000102493133434356)),
      row.names = 1:2,
      class = "data.frame"),      
      orig.matrix = 
        structure(c(0.000110047078473174,
                    0.0105052831822057,
                    0.000955977750583648,
                    
                    0.000106231316866934, 
                    0.0151846812080705,  
                    0.000102493133434356),
                  .Dim = 3:2, 
                  .Dimnames = list(c("ex1","ex2", "ex3"), c("gt1", "gt2"))), 
         modified.matrix = 
           structure(c(0.000110047078473174,      0.0105052831822057,
                       0.000955977750583648, 0.000106231316866934,
                       9e+99, 0.000102493133434356),
                     .Dim = 3:2,
                     .Dimnames =
                       list(c("ex1", "ex2", "ex3"), c("gt1", "gt2"))))
  )
})

