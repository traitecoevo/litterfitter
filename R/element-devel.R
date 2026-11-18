# ---- Experimental element models (import-devel branch only) -----------------
#
# Unfinished models carried over from the `import` branch (2022-2024, Baptiste
# Wijas and Will Cornwell) and ported to the current code so work can continue.
# Not for release until the open questions in issue #30 are resolved.


# Manzoni et al. (2008) stoichiometric model of element release.
#
# x: proportional mass (carbon) remaining; l: litter C:N ratio; e: decomposer
# carbon use efficiency. Passes through 1 at x = 1.
#
# Changes from the import branch:
#   - e bounded to (0.01, 0.99). On the branch the bounds were [0.01, 200], but
#     the exponent 1 / (1 - e) is only meaningful for 0 < e < 1: e = 1 divides
#     by zero and e > 1 flips the shape of the curve.
#
# TODO (see #30):
#   - The critical C:N ratio is hard-coded as 10 (the `10 / l` terms). Decide
#     whether it should be a parameter, a user-supplied constant, or derived
#     from litter and decomposer stoichiometry.
#   - Check the formula against Manzoni et al. (2008).
#   - Decide sensible bounds for l (currently 0.1-200, as on the branch).
stoch.model <- function(x, l, e, upper = c(200, 0.99), lower = c(0.1, 0.01)) {
  x * (10 / l) + (1 - (10 / l)) * x^(1 / (1 - e))
}


# Exponential model from the import branch: a^x + b.
#
# Not wired into fit_element(): as written it equals a + b at x = 1 rather than
# 1, and the branch bounds (a in [1, 1e6], b in [0.1, 400]) give values far
# outside proportional element remaining.
#
# TODO (see #30): establish the scientific basis and intended use, and decide
# whether it should be normalised (e.g. (a^x + b) / (a + b)) or dropped.
exp.model <- function(x, a, b, upper = c(1e6, 400), lower = c(1, 0.1)) {
  a^x + b
}
