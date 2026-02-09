#' Create a hierarchical, mutable options manager
#'
#' `create_options_manager()` creates a runtime configuration manager that
#' supports **nested options**, **group validation**, and **resetting to defaults**.
#' It is ideal for managing complex, interdependent settings in R packages or projects.
#'
#' @param defaults A named list specifying the default values of the options.
#'   Nested lists can be used to represent hierarchical groups of related options.
#' @param validators An optional named list of functions used to validate options.
#'   Each function should take a single argument (the value being set) and
#'   throw an error if the value is invalid. Names correspond to option paths,
#'   e.g., `"thermaltime"` for a top-level group.
#'
#' @return A list with three functions:
#' \describe{
#'   \item{\code{get(name = NULL)}}{Retrieve the current value of an option. Use a
#'         dot-separated string for nested options, e.g., \code{"thermaltime.x"}.
#'         If \code{name} is NULL, returns all current options.}
#'   \item{\code{set(...)} }{Update one or more options by name. Accepts
#'         named arguments where names can be dot-separated for nested options,
#'         e.g., \code{thermaltime = list(x = ..., y = ...)}. Validators are
#'         automatically applied if provided.}
#'   \item{\code{reset()}}{Reset all options to their default values.}
#' }
#'
#' @details
#' This manager allows you to safely store and update **related groups of options**.
#' For example, a `thermaltime` group might have `x` and `y` vectors that must
#' always have the same length. Using validators ensures that these relationships
#' are maintained whenever options are updated.
#'
#' The manager supports **merge-aware updates**, meaning that if a nested list
#' is provided, only the specified elements are updated while others are preserved.
#'
#' @examples
#' # Define a validator for a group
#' thermaltime_validator <- function(value) {
#'   if (!is.list(value) || !all(c("x","y") %in% names(value))) {
#'     stop("thermaltime must be a list with both x and y")
#'   }
#'   if (length(value$x) != length(value$y)) stop("thermaltime x and y must have same length")
#' }
#'
#' # Create a manager
#' canola <- create_options_manager(
#'   defaults = list(
#'     thermaltime = list(x = c(2,30,35), y = c(0,28,0)),
#'     frost_threshold = 0
#'   ),
#'   validators = list(
#'     "thermaltime" = thermaltime_validator
#'   )
#' )
#'
#' # Access and update
#' canola$get("thermaltime.x")
#' canola$set(thermaltime = list(x = c(5,25,40), y = c(0,20,0)))
#'
#' # Reset to defaults
#' canola$reset()
#'
#' @export
create_options_manager <- function(defaults, validators = list()) {
    state <- new.env(parent = emptyenv())
    state$options <- defaults

    # Helper to get nested value
    get_nested <- function(lst, keys) {
        for (k in keys) {
            if (!k %in% names(lst)) {
                return(NULL)
            }
            lst <- lst[[k]]
        }
        lst
    }

    # Helper to set nested value with optional validation
    set_nested <- function(lst, keys, value, full_keys = keys) {
        if (length(keys) == 1) {
            # Run validator if exists
            validator_key <- paste(full_keys, collapse = ".")
            if (!is.null(validators[[validator_key]])) {
                validators[[validator_key]](value)
            }

            # Merge if both are lists
            current <- lst[[keys]]
            if (is.list(current) && is.list(value)) {
                for (n in names(value)) current[[n]] <- value[[n]]
                lst[[keys]] <- current
            } else {
                lst[[keys]] <- value
            }
            return(lst)
        }

        key <- keys[1]
        if (is.null(lst[[key]])) lst[[key]] <- list()
        lst[[key]] <- set_nested(lst[[key]], keys[-1], value, full_keys)
        lst
    }

    # Manager functions
    list(
        get = function(name = NULL) {
            if (is.null(name)) {
                return(state$options)
            }
            keys <- strsplit(name, "\\.")[[1]]
            get_nested(state$options, keys)
        },
        set = function(...) {
            args <- list(...)
            if (is.null(names(args)) || any(names(args) == "")) stop("All arguments must be named")
            for (key in names(args)) {
                keys <- strsplit(key, "\\.")[[1]]
                state$options <- set_nested(state$options, keys, args[[key]])
            }
            invisible(state$options)
        },
        reset = function() {
            state$options <- defaults
            invisible(state$options)
        }
    )
}
