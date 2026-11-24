#' Create a hierarchical, mutable options manager
#'
#' `create_options_manager()` creates a runtime configuration manager that
#' supports **nested options**, **group validation**, and **resetting to defaults**.
#' It is ideal for managing complex, interdependent settings in R packages or projects.
#'
#' @param defaults A named list specifying the default values of the options.
#'   Nested lists can be used to represent hierarchical groups of related options.
#'   A group containing `"_as_collection" = TRUE` is a collection of named
#'   records and predefined non-list options. The marker is schema metadata
#'   and is not included in the options.
#' @param validators An optional named list of functions used to validate options.
#'   Each function should take a single argument (the value being set) and
#'   throw an error if the value is invalid. Names correspond to option paths,
#'   e.g., `"thermaltime"` for a top-level group. For a collection, define
#'   each required record field using `"<collection path>._default.<field>"`.
#'   Each field validator receives the complete field value.
#'
#' @return A list with three functions:
#' \describe{
#'   \item{\code{get(name = NULL)}}{Retrieve the current value of an option. Use a
#'         dot-separated string for nested options, e.g., \code{"thermaltime.x"}.
#'         If \code{name} is NULL, returns all current options.}
#'   \item{\code{set(...)} }{Update one or more options by name. Accepts named arguments
#'         in two formats: (1) dot-separated paths like \code{"phenology.thermaltime.y" = ...}
#'         or (2) nested lists like \code{thermaltime = list(x = ..., y = ...)}. 
#'         Both styles can be mixed in a single call. Validators are automatically 
#'         applied if provided.}
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
#' **Dot-separated path notation**: The \code{set()} function now accepts path strings
#' like \code{"phenology.thermaltime.y" = c(0, 25, 0)}, which are automatically 
#' converted to nested lists internally. This provides a more concise syntax for 
#' updating deeply nested options without reconstructing the entire hierarchy.
#'
#' **Transactional updates**: If validation fails during a \code{set()} call, all
#' changes are rolled back and the options remain in their previous state. This ensures
#' that the options manager is always in a consistent state.
#'
#' **Collections**: A collection can contain predefined non-list options and
#' named list records with exactly the fields declared by its `_default`
#' validators. Non-list options may have ordinary validators and cannot be
#' changed into lists. New names must be records, and records cannot be changed
#' into non-list values. Existing records are checked
#' when the manager is created. New records must supply every field; existing
#' records may be updated partially. Only changed records are revalidated,
#' although ordinary validators still run as usual. Collections cannot be nested
#' inside collection records. `reset()` removes added records and restores the
#' original collection. Empty collections are supported. Record names must be
#' unique, non-empty and contain no dots. `_default` and `_as_collection` are
#' reserved names within collections. Validators for individual records are not
#' supported; use `_default` for record fields or validate the whole collection.
#'
#' @examples
#' # Define a validator for a group
#' thermaltime_validator <- function(value) {
#'     if (!is.list(value) || !all(c("x", "y") %in% names(value))) {
#'         stop("thermaltime must be a list with both x and y")
#'     }
#'     if (length(value$x) != length(value$y)) stop("thermaltime x and y must have same length")
#' }
#'
#' # Create a manager
#' canola <- create_options_manager(
#'     defaults = list(
#'         thermaltime = list(x = c(2, 30, 35), y = c(0, 28, 0)),
#'         frost_threshold = 0
#'     ),
#'     validators = list(
#'         "thermaltime" = thermaltime_validator
#'     )
#' )
#'
#' # Access and update (both methods work)
#' canola$get("thermaltime.x")
#'
#' # Method 1: Use dot-separated path strings (concise!)
#' canola$set("thermaltime.y" = c(0, 25, 0))
#' canola$set("thermaltime.x" = c(5, 25, 40))
#'
#' # Method 2: Use nested list (traditional way)
#' canola$set(thermaltime = list(x = c(5, 25, 40), y = c(0, 20, 0)))
#'
#' # Method 3: Mix both styles in one call
#' canola$set(
#'   "thermaltime.x" = c(10, 30, 45),
#'   frost_threshold = -2
#' )
#'
#' # Reset to defaults
#' canola$reset()
#'
#' sources <- create_options_manager(
#'     defaults = list(items = list(
#'         "_as_collection" = TRUE,
#'         folder = "collection",
#'         code_base = list(type = "files", enabled = TRUE)
#'     )),
#'     validators = list(
#'         "items.folder" = v_character_scalar(),
#'         "items._default.type" = v_character_scalar(),
#'         "items._default.enabled" = v_logical_scalar()
#'     )
#' )
#' sources$set("items.folder" = "new-collection")
#' sources$set("items.crop_model" = list(type = "files", enabled = FALSE))
#' sources$set("items.crop_model.enabled" = TRUE)
#' sources$reset()
#'
#' @export
create_options_manager <- function(defaults, validators = list()) {
    if (!is.list(defaults) || is.null(names(defaults))) {
        stop("`defaults` must be a named list", call. = FALSE)
    }

    check_names(defaults)

    check_no_dots(defaults)

    if (!is.list(validators)) {
        stop("`validators` must be a named list of functions", call. = FALSE)
    }

    if (length(validators)) {
        if (is.null(names(validators)) || any(names(validators) == "")) {
            stop("All validators must be named", call. = FALSE)
        }

        bad <- !vapply(validators, is.function, logical(1))
        if (any(bad)) {
            stop(
                sprintf(
                    "Validators must be functions: %s",
                    paste(names(validators)[bad], collapse = ", ")
                ),
                call. = FALSE
            )
        }
    }

    extracted <- extract_collections(defaults)
    defaults <- extracted$defaults
    collections <- collection_schemas(extracted$paths, validators, defaults)
    record_validators <- names(validators)[vapply(
        names(validators), is_collection_field_validator, logical(1), paths = extracted$paths
    )]
    validators <- validators[!names(validators) %in% record_validators]
    validate_paths(defaults, names(validators))

    for (path in extracted$paths) {
        items <- get_collection_at_path(defaults, path)
        validate_collection_names(items, path)
        for (name in names(items)) {
            if (is.list(items[[name]])) {
                validate_collection_record(items[[name]], collections[[collection_key(path)]], c(path, name))
            }
        }
    }

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
    set_nested <- function(lst, keys, value, full_keys = keys, defaults) {
        key <- keys[1]

        # check top-level key exists
        if (!key %in% names(defaults)) {
            stop(sprintf("Option '%s' is not defined", paste(full_keys, collapse = ".")), call. = FALSE)
        }

        if (length(keys) == 1) {
            # Leaf or group
            current <- lst[[key]]
            if (is.list(current) && is.list(value)) {
                # Check for unknown keys at this level
                unknown_keys <- setdiff(names(value), names(defaults[[key]]))
                if (length(unknown_keys) > 0) {
                    stop(sprintf(
                        "Unknown sub-option(s) for '%s': %s",
                        paste(full_keys, collapse = "."),
                        paste(unknown_keys, collapse = ", ")
                    ), call. = FALSE)
                }
                # Merge only known keys
                for (n in names(value)) current[[n]] <- value[[n]]
                lst[[key]] <- current
            } else {
                lst[[key]] <- value
            }
            return(lst)
        }

        # Recurse deeper
        lst[[key]] <- set_nested(lst[[key]], keys[-1], value, full_keys, defaults[[key]])
        lst
    }

    # Manager functions
    list(
        get = function(name = NULL) {
            if (is.null(name)) {
                return(state$options)
            }

            keys <- strsplit(name, "\\.")[[1]]
            cur <- state$options

            for (k in keys) {
                if (!is.list(cur) || !k %in% names(cur)) {
                    stop(
                        sprintf("Option '%s' is not defined", name),
                        call. = FALSE
                    )
                }
                cur <- cur[[k]]
            }

            cur
        },
        set = function(...) {
            args <- list(...)
            if (is.null(names(args)) || any(names(args) == "")) {
                stop("All arguments must be named")
            }

            # Convert dot-separated paths to nested lists
            processed_args <- list()
            for (nm in names(args)) {
                if (is.null(args[[nm]]) && !grepl("\\.", nm) &&
                    collection_key(character()) %in% names(collections)) {
                    if (nm %in% names(defaults) && !is.list(defaults[[nm]])) {
                        processed_args[nm] <- list(NULL)
                        next
                    }
                    stop(sprintf("Record '%s' must be a named list", nm), call. = FALSE)
                }
                if (is.null(args[[nm]]) && !grepl("\\.", nm) &&
                    length(collections) &&
                    any(names(collections) == nm | startsWith(names(collections), paste0(nm, ".")))) {
                    stop(sprintf("Option '%s' contains a collection and must be a group", nm), call. = FALSE)
                }
                if (grepl("\\.", nm)) {
                    # Path with dots: convert to nested structure
                    keys <- strsplit(nm, "\\.")[[1]]
                    nested <- path_to_nested_list(keys, args[[nm]])
                    # Merge into processed_args
                    processed_args <- merge_into_list(processed_args, nested)
                } else {
                    # Top-level key
                    if (nm %in% names(processed_args)) {
                        # Merge if key already exists
                        if (is.list(args[[nm]]) && is.list(processed_args[[nm]])) {
                            processed_args[[nm]] <- merge_lists(processed_args[[nm]], args[[nm]])
                        } else {
                            processed_args[[nm]] <- args[[nm]]
                        }
                    } else {
                        processed_args[[nm]] <- args[[nm]]
                    }
                }
            }

            # Build the complete candidate before committing any changes.
            candidate <- state$options
            if (collection_key(character()) %in% names(collections)) {
                candidate <- validate_and_merge(
                    current = candidate, update = processed_args, defaults = defaults,
                    path = character(), collections = collections
                )
            } else {
                for (nm in names(processed_args)) {
                    if (!nm %in% names(defaults)) {
                        stop(sprintf("Option '%s' is not defined", nm), call. = FALSE)
                    }

                    candidate[[nm]] <- validate_and_merge(
                        current = candidate[[nm]], update = processed_args[[nm]],
                        defaults = defaults[[nm]], path = nm, collections = collections
                    )
                }
            }
            run_validators(candidate, validators)
            state$options <- candidate
            invisible(state$options)
        },
        reset = function() {
            state$options <- defaults
            invisible(state$options)
        }
    )
}


validate_and_merge <- function(current, update, defaults, path, collections = list()) {
    key <- collection_key(path)
    if (key %in% names(collections)) {
        validate_collection_names(update, path)
        for (name in names(update)) {
            record_path <- c(path, name)
            changes <- update[[name]]
            if (name %in% names(defaults) && !is.list(defaults[[name]])) {
                if (is.list(changes)) {
                    stop(sprintf("Option '%s' cannot be a list", paste(record_path, collapse = ".")), call. = FALSE)
                }
                current[name] <- list(changes)
                next
            }
            if (!is.list(changes) || is.data.frame(changes) ||
                is.null(names(changes)) || any(names(changes) == "") ||
                anyDuplicated(names(changes))) {
                stop(sprintf("Record '%s' must be a named list", paste(record_path, collapse = ".")), call. = FALSE)
            }
            unknown <- setdiff(names(changes), names(collections[[key]]))
            if (length(unknown)) {
                stop(sprintf("Unknown field(s) for '%s': %s", paste(record_path, collapse = "."), paste(unknown, collapse = ", ")), call. = FALSE)
            }
            record <- if (name %in% names(current)) {
                merge_lists(current[[name]], changes)
            } else {
                changes
            }
            validate_collection_record(record, collections[[key]], record_path)
            current[name] <- list(record)
        }
        return(current)
    }

    if (!is.list(update)) {
        prefix <- if (length(path)) paste0(paste(path, collapse = "."), ".") else ""
        if (length(collections) && any(startsWith(names(collections), prefix))) {
            stop(sprintf("Option '%s' contains a collection and must be a group", paste(path, collapse = ".")), call. = FALSE)
        }
        return(update)
    }

    if (!is.list(defaults)) {
        stop(sprintf(
            "Option '%s' is not a group and cannot accept sub-options",
            paste(path, collapse = ".")
        ), call. = FALSE)
    }

    unknown <- setdiff(names(update), names(defaults))
    if (length(unknown) > 0) {
        stop(sprintf(
            "Unknown sub-option(s) for '%s': %s",
            paste(path, collapse = "."),
            paste(unknown, collapse = ", ")
        ), call. = FALSE)
    }

    for (nm in names(update)) {
        current[[nm]] <- validate_and_merge(
            current = current[[nm]],
            update = update[[nm]],
            defaults = defaults[[nm]],
            path = c(path, nm),
            collections = collections
        )
    }

    current
}

collection_key <- function(path) {
    if (length(path)) paste(path, collapse = ".") else "."
}

extract_collections <- function(defaults) {
    paths <- list()
    extract <- function(value, path, in_collection = FALSE) {
        if (!is.list(value)) {
            return(value)
        }
        marked <- "_as_collection" %in% names(value)
        if (marked) {
            if (in_collection) {
                stop("Collections cannot be nested inside collection records", call. = FALSE)
            }
            if (!identical(value[["_as_collection"]], TRUE)) {
                stop(sprintf("Collection marker at '%s' must be TRUE", paste(path, collapse = ".")), call. = FALSE)
            }
            paths[[length(paths) + 1L]] <<- path
            value[["_as_collection"]] <- NULL
            validate_collection_names(value, path)
        }
        for (name in names(value)) {
            value[name] <- list(extract(value[[name]], c(path, name), in_collection || marked))
        }
        value
    }
    result <- extract(defaults, character())
    list(defaults = result, paths = paths)
}

is_collection_field_validator <- function(name, paths) {
    keys <- strsplit(name, "\\.")[[1]]
    any(vapply(paths, function(path) {
        length(keys) > length(path) &&
            identical(keys[seq_along(path)], path) &&
            keys[length(path) + 1L] == "_default"
    }, logical(1)))
}

collection_schemas <- function(paths, validators, defaults) {
    schemas <- list()
    for (path in paths) {
        schemas[collection_key(path)] <- list(list())
    }
    for (name in names(validators)) {
        keys <- strsplit(name, "\\.")[[1]]
        if (!is_collection_field_validator(name, paths)) {
            for (path in paths) {
                if (length(keys) > length(path) &&
                    identical(keys[seq_along(path)], path)) {
                    items <- get_collection_at_path(defaults, path)
                    child <- keys[length(path) + 1L]
                    if (length(keys) != length(path) + 1L ||
                        !child %in% names(items) || is.list(items[[child]])) {
                        stop(sprintf("Validator '%s' must use _default for collection record fields", name), call. = FALSE)
                    }
                }
            }
            next
        }
        matching_paths <- which(vapply(paths, function(path) {
            length(keys) > length(path) &&
                identical(keys[seq_along(path)], path) &&
                keys[length(path) + 1L] == "_default"
        }, logical(1)))
        path <- paths[[matching_paths[1L]]]
        if (length(keys) != length(path) + 2L) {
            stop(sprintf("Invalid collection field validator path '%s'", name), call. = FALSE)
        }
        key <- collection_key(path)
        field <- keys[length(keys)]
        if (!key %in% names(schemas) || field %in% c("", "_default", "_as_collection")) {
            stop(sprintf("Invalid collection field validator path '%s'", name), call. = FALSE)
        }
        if (field %in% names(schemas[[key]])) {
            stop(sprintf("Duplicate collection field validator '%s'", name), call. = FALSE)
        }
        schemas[[key]][field] <- validators[name]
    }
    for (path in paths) {
        if (!length(schemas[[collection_key(path)]])) {
            stop(sprintf("Collection '%s' must have at least one _default field validator", paste(path, collapse = ".")), call. = FALSE)
        }
    }
    schemas
}

get_collection_at_path <- function(options, path) {
    for (key in path) {
        options <- options[[key]]
    }
    options
}

validate_collection_names <- function(items, path) {
    if (!is.list(items) || is.data.frame(items) ||
        (length(items) && (is.null(names(items)) || any(names(items) == "") ||
            anyDuplicated(names(items)) || any(grepl("\\.", names(items))) ||
            any(names(items) %in% c("_default", "_as_collection"))))) {
        stop(sprintf("Collection '%s' must be a named list with unique names without dots", paste(path, collapse = ".")), call. = FALSE)
    }
}

validate_collection_record <- function(record, validators, path) {
    label <- paste(path, collapse = ".")
    if (!is.list(record) || is.data.frame(record) || is.null(names(record)) ||
        any(names(record) == "") || anyDuplicated(names(record))) {
        stop(sprintf("Record '%s' must be a named list", label), call. = FALSE)
    }
    missing <- setdiff(names(validators), names(record))
    unknown <- setdiff(names(record), names(validators))
    if (length(missing)) {
        stop(sprintf("Missing field(s) for '%s': %s", label, paste(missing, collapse = ", ")), call. = FALSE)
    }
    if (length(unknown)) {
        stop(sprintf("Unknown field(s) for '%s': %s", label, paste(unknown, collapse = ", ")), call. = FALSE)
    }
    has_marker <- function(value) {
        is.list(value) && ("_as_collection" %in% names(value) ||
            any(vapply(value, has_marker, logical(1))))
    }
    if (has_marker(record)) {
        stop("Collections cannot be nested inside collection records", call. = FALSE)
    }
    for (field in names(validators)) {
        tryCatch(
            validators[[field]](record[[field]]),
            error = function(e) {
                stop(sprintf("Invalid field '%s.%s': %s", label, field, conditionMessage(e)), call. = FALSE)
            }
        )
    }
}

# Helper function: convert a path and value into a nested list structure
# e.g., path_to_nested_list(c("phenology", "thermal_time", "y"), c(0, 25, 0))
#       => list(phenology = list(thermal_time = list(y = c(0, 25, 0))))
path_to_nested_list <- function(keys, value) {
    if (length(keys) == 0) {
        return(value)
    }
    
    result <- list(value)
    names(result) <- keys[length(keys)]
    
    for (i in (length(keys) - 1):1) {
        tmp <- list(result)
        names(tmp) <- keys[i]
        result <- tmp
    }
    
    result
}

# Helper function: recursively merge two lists
merge_lists <- function(x, y) {
    for (nm in names(y)) {
        if (nm %in% names(x) && is.list(x[[nm]]) && is.list(y[[nm]])) {
            x[[nm]] <- merge_lists(x[[nm]], y[[nm]])
        } else {
            x[nm] <- list(y[[nm]])
        }
    }
    x
}

# Helper function: merge a nested list into the main argument list
merge_into_list <- function(main, nested) {
    keys <- names(nested)
    if (length(keys) == 1 && is.list(nested[[1]])) {
        key <- keys[1]
        if (key %in% names(main) && is.list(main[[key]]) && is.list(nested[[key]])) {
            main[[key]] <- merge_lists(main[[key]], nested[[key]])
        } else {
            main[[key]] <- nested[[key]]
        }
    }
    main
}

run_validators <- function(options, validators) {

    for (path in names(validators)) {
        keys <- strsplit(path, "\\.")[[1]]
        value <- Reduce(`[[`, keys, options)
        validators[[path]](value)
    }
}


validate_paths <- function(defaults, paths) {
    for (p in paths) {
        keys <- strsplit(p, "\\.")[[1]]
        cur <- defaults
        for (k in keys) {
            if (!is.list(cur) || !k %in% names(cur)) {
                stop(
                    sprintf("Validator path '%s' does not exist in defaults", p),
                    call. = FALSE
                )
            }
            cur <- cur[[k]]
        }
    }
}


check_no_dots <- function(x, path = NULL) {
    if (!is.list(x)) {
        return()
    }

    nms <- names(x)
    if (is.null(nms) || any(nms == "")) {
        stop(sprintf("All names must be non-empty at path '%s'", paste(path, collapse = ".")), call. = FALSE)
    }

    if (any(grepl("\\.", nms))) {
        stop(
            sprintf(
                "Option names must not contain '.' at path '%s': %s",
                paste(path, collapse = "."),
                paste(nms[grepl("\\.", nms)], collapse = ", ")
            ),
            call. = FALSE
        )
    }

    # Recurse into nested lists
    for (i in seq_along(x)) {
        check_no_dots(x[[i]], c(path, nms[i]))
    }
}

check_names <- function(x, path = NULL) {
    if (!is.list(x)) {
        return()
    }

    nms <- names(x)
    if (is.null(nms)) {
        stop(sprintf("All lists must be named at path '%s'", paste(path, collapse = ".")), call. = FALSE)
    }

    # check empty
    if (any(nms == "")) {
        stop(sprintf("All names must be non-empty at path '%s'", paste(path, collapse = ".")), call. = FALSE)
    }

    # check for dots
    if (any(grepl("\\.", nms))) {
        stop(
            sprintf(
                "Option names must not contain '.' at path '%s': %s",
                paste(path, collapse = "."),
                paste(nms[grepl("\\.", nms)], collapse = ", ")
            ),
            call. = FALSE
        )
    }

    # recurse
    for (i in seq_along(x)) {
        check_names(x[[i]], c(path, nms[i]))
    }
}
