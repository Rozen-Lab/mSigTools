test_that("test TP_FP_FN_avg_sim", {
  ex.sigs <- matrix(c(0.2, 0.8, 0.3, 0.7), nrow = 2)
  colnames(ex.sigs) <- c("ex1", "ex2")
  gt.sigs <- matrix(c(0.21, 0.79, 0.19, 0.81), nrow = 2)
  colnames(gt.sigs) <- c("gt1", "gt2")
  rr <- TP_FP_FN_avg_sim(
    extracted.sigs    = ex.sigs,
    reference.sigs    = gt.sigs,
    similarity.cutoff = .985
  )
  expect_equal(
    rr,
    list(TP = 2L,
         FP = 0L,
         FN = 0L,
         avg.cos.sim = 0.994694242750464,
         table = structure(
           list(ex.sig = c("ex1", "ex2"),
                ref.sig = c("gt2",      "gt1"),
                sim = c(0.999893768683133, 0.989494716817794)),
           row.names = 1:2, class = "data.frame"),
         sim.matrix = structure(c(0.999889952921527, 0.989494716817794,
                                  0.999893768683133, 0.98481531879193),
                                .Dim = c(2L, 2L),
                                .Dimnames = list(
                                  c("ex1", "ex2"), c("gt1", "gt2"))),
         unmatched.ex.sigs = character(0),
         unmatched.ref.sigs = character(0))
  )

  ex.sigs2 <- cbind(ex.sigs, ex3 = c(0.18, 0.82))
  rr <- TP_FP_FN_avg_sim(
    extracted.sigs = ex.sigs2,
    reference.sigs = gt.sigs,
    similarity.cutoff = .985
  )
  expect_equal(
    rr,
    list(TP = 2L, FP = 1L, FN = 0L,
         avg.cos.sim = 0.999893729894046,
         table = structure(
           list(ex.sig = c("ex1", "ex3"), ref.sig = c("gt1",      "gt2"),
                sim = c(0.999889952921527, 0.999897506866566)),
           row.names = 1:2, class = "data.frame"),
         sim.matrix = structure(
           c(0.999889952921527, 0.989494716817794,
             0.999044022249416, 0.999893768683133,
             0.98481531879193, 0.999897506866566     ),
           .Dim = 3:2,
           .Dimnames = list(c("ex1", "ex2", "ex3"), c("gt1",      "gt2"))),
         unmatched.ex.sigs = "ex2",
         unmatched.ref.sigs = character(0))
  )
})
