## Submission

This is an update from 0.1.5 to 0.2.0. In this version I have:

- Fixed several bugs (default model selection, convergence check, steady state
  for the `neg.exp.limit` model, bootstrap failures, time-to-threshold calculation)
- Added standard S3 methods (`print`, `logLik`, `nobs`, `residuals`) for fitted objects
- Made model fitting roughly 3-4x faster
- Removed the dependency on `methods`

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are no reverse dependencies on CRAN.
