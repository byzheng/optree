# optree 0.1.1

* Added `max_len` support to `v_xypair(min_len = 1, max_len = NULL)` to optionally enforce an upper bound on paired vector length.
* Added constructor argument validation for `v_xypair()` (`min_len`/`max_len` type and range checks, and `max_len >= min_len`).
* Added constructor argument validation for `v_numeric_vector()` (`min_len` must be a non-negative whole number and `finite` must be a single logical value).
* Expanded validator tests for new argument validation and `max_len` behavior.
* Updated validator documentation in the vignette and README to reflect the new API and validation rules.

# optree 0.1.0

* This is the first release of optree.

