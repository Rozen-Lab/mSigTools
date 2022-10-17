# mSigTools 1.0.6
* Added function `optimize_exposure_QP`.
* Added `best_reconstruction_QP`.
* Fixed a bug in function `sig_dist_matrix` when in some very rare cases, some
  cosine similarities can be slightly greater than 1.
* Fixed a bug in function `plot_exposure_internal` not to plot sample names when
there are no column names in`exposure`.
* Removed package `PCAWG7` from Suggests field in DESCRIPTION. Added `ICAMS` as suggested package.
* Added new dependency package `sets` as it is required in function `best_reconstruction_QP`.

# mSigTools 1.0.5 (submitted to CRAN)
* Updated test functions in tests/testthat/ folder.

# mSigTools 1.0.4
* Fixed test in test_exposure_related_functions.R.

# mSigTools 1.0.3
* Added return value for exported functions `match_two_sig_sets` and
`write_exposure`.

# mSigTools 1.0.2
* Added reference for "Hungarian algorithm" in package DESCRIPTION.

# mSigTools 1.0.1
* Updated documentation for function `TP_FP_FN_avg_sim`.

# mSigTools 1.0.0
* First stable release.
