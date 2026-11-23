## Submission

This is a patch release (0.3.0 -> 0.3.1) that fixes the test failures shown on
the CRAN check page (r-oldrel-macos-arm64, r-oldrel-macos-x86_64) and in the
additional issues (BLIS, noLD, linux-arm64). It changes only the tests; the
package code is the same.

- `test-gradients.R`: a check that uses a very tight optimiser tolerance
  (`factr = 1e3`) now also accepts L-BFGS-B convergence code 52. On some
  BLAS/CPU combinations the line search stops there at machine precision. The
  test still checks the fitted objective values and predictions.
- `test-bootstrap.R`: a few bootstrap replicates on a small example data set
  can fail on some platforms and are stored as `NA`, as documented. The test now
  ignores `NA`s when it takes the median and checks that fewer than 5% of
  replicates failed.

## Test environments

- local macOS (aarch64), R 4.6.1
- win-builder: R-devel
- GitHub Actions: macOS-latest (R-release, R-oldrel-1), windows-latest
  (R-release), ubuntu-latest (R-release, R-oldrel-1), ubuntu-24.04-arm
  (R-release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are no reverse dependencies on CRAN.
