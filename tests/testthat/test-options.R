# Test options manager functionality
canola <- create_options_manager(
    defaults = list(
        thermaltime = list(
            x = c(2, 30, 35),
            y = c(0, 28, 0)
        ),
        frost_threshold = 0
    ),
    validators = list(
        "thermaltime" = v_xypair(),
        "frost_threshold" = v_numeric_scalar()
    )
)


test_that("get errors on unknown option path", {
    expect_error(
        canola$get("phenology.unknown"),
        "Option 'phenology.unknown' is not defined"
    )
    expect_error(
        canola$get("unknown-parameter"),
        "Option 'unknown-parameter' is not defined"
    )
})

test_that("get returns correct default values", {
    expect_equal(canola$get("thermaltime.x"), c(2, 30, 35))
    expect_equal(canola$get("thermaltime.y"), c(0, 28, 0))
    expect_equal(canola$get("frost_threshold"), 0)
})

test_that("set updates values correctly", {
    canola$set(thermaltime = list(x = c(5, 25, 40), y = c(0, 20, 0)))
    expect_equal(canola$get("thermaltime.x"), c(5, 25, 40))
    expect_equal(canola$get("thermaltime.y"), c(0, 20, 0))

    canola$set(frost_threshold = -2)
    expect_equal(canola$get("frost_threshold"), -2)
})

test_that("set enforces group validation", {
    # Missing y
    expect_no_error(
        canola$set(thermaltime = list(x = c(1, 2, 3)))
    )

    # x and y different length
    expect_error(
        canola$set(thermaltime = list(x = c(1, 2), y = c(0, 1, 2)))
    )
})

test_that("reset restores defaults", {
    canola$reset()
    expect_equal(canola$get("thermaltime.x"), c(2, 30, 35))
    expect_equal(canola$get("thermaltime.y"), c(0, 28, 0))
    expect_equal(canola$get("frost_threshold"), 0)
})

test_that("cannot set unknown top-level options", {
    expect_error(
        canola$set(nonexistent_option = 123),
        "Option 'nonexistent_option' is not defined"
    )
})

test_that("cannot set unknown nested options", {
    expect_error(
        canola$set(thermaltime = list(
            x = c(5, 25, 40),
            y = c(0, 20, 0),
            z = 999 # not defined in defaults
        )),
        "Unknown sub-option\\(s\\) for 'thermaltime': z"
    )
})


# Test more complex nested structure
# Create a CANOLA options manager
canola <- create_options_manager(
    defaults = list(
        phenology = list(
            thermaltime = list(
                x = c(2, 30, 35),
                y = c(0, 28, 0)
            )
        ),
        frost_threshold = 0
    ),
    validators = list(
        "phenology.thermaltime" = v_xypair(),
        "frost_threshold" = v_numeric_scalar()
    )
)

test_that("cannot set unknown top-level options", {
    expect_error(
        canola$set(nonexistent_option = 123),
        "Option 'nonexistent_option' is not defined"
    )
})

test_that("cannot set unknown nested options", {
    expect_error(
        canola$set(phenology = list(
            thermaltime = list(
                x = c(5, 25, 40),
                y = c(0, 20, 0),
                z = 999 # not defined in defaults
            )
        )),
        "Unknown sub-option\\(s\\) for 'phenology.thermaltime': z"
    )
})

test_that("can set only predefined nested options", {
    canola$set(phenology = list(
        thermaltime = list(
            x = c(5, 25, 40),
            y = c(0, 20, 0)
        )
    ))
    expect_equal(canola$get("phenology.thermaltime.x"), c(5, 25, 40))
    expect_equal(canola$get("phenology.thermaltime.y"), c(0, 20, 0))
})

test_that("partial nested updates respect predefined keys", {
    # only update x
    canola$set(phenology = list(
        thermaltime = list(
            x = c(1, 2, 3)
        )
    ))
    expect_equal(canola$get("phenology.thermaltime.x"), c(1, 2, 3))
    expect_equal(canola$get("phenology.thermaltime.y"), c(0, 20, 0)) # unchanged
})

# Test dot-separated path notation in set()
test_that("set accepts dot-separated paths for nested options", {
    canola$reset()
    
    # Set a deeply nested value using dot notation
    canola$set("phenology.thermaltime.x" = c(10, 20, 30))
    expect_equal(canola$get("phenology.thermaltime.x"), c(10, 20, 30))
    # y should be unchanged
    expect_equal(canola$get("phenology.thermaltime.y"), c(0, 28, 0))
    
    # Set another nested value
    canola$set("phenology.thermaltime.y" = c(1, 2, 3))
    expect_equal(canola$get("phenology.thermaltime.y"), c(1, 2, 3))
})

test_that("set can mix dot-separated paths and nested lists", {
    canola$reset()
    
    # Mix both styles in one call
    canola$set(
        "phenology.thermaltime.x" = c(5, 15, 25),
        frost_threshold = -3
    )
    
    expect_equal(canola$get("phenology.thermaltime.x"), c(5, 15, 25))
    expect_equal(canola$get("frost_threshold"), -3)
})

test_that("dot-separated paths enforce validation", {
    canola$reset()
    
    # First set x to different length
    
    expect_error(
        canola$set("phenology.thermaltime.x" = c(1, 2))
    )
    expect_equal(canola$get("phenology.thermaltime.x"), c(2, 30, 35)) # should be unchanged
    # Now y has different length, should fail validation
    expect_error(
        canola$set("phenology.thermaltime.y" = c(0, 28))
    )
    expect_equal(canola$get("phenology.thermaltime.y"), c(0, 28, 0)) # should be unchanged
})

test_that("dot-separated paths reject unknown options", {
    expect_error(
        canola$set("phenology.thermaltime.z" = 999),
        "Unknown sub-option\\(s\\) for 'phenology.thermaltime': z"
    )
    
    expect_error(
        canola$set("phenology.unknown_group.x" = 1),
        "Unknown sub-option\\(s\\) for 'phenology': unknown_group"
    )
    
    expect_error(
        canola$set("nonexistent.path" = 1),
        "Option 'nonexistent' is not defined"
    )
})

test_that("set rolls back on validation failure", {
    canola$reset()
    
    # Set valid values first
    canola$set("phenology.thermaltime.x" = c(5, 15, 25))
    canola$set("phenology.thermaltime.y" = c(0, 10, 0))
    
    original_x <- canola$get("phenology.thermaltime.x")
    original_y <- canola$get("phenology.thermaltime.y")
    
    # Try to set x with mismatched length - should fail and rollback
    expect_error(
        canola$set("phenology.thermaltime.x" = c(1, 2))
    )
    
    # Verify state is unchanged after failed validation
    expect_equal(canola$get("phenology.thermaltime.x"), original_x)
    expect_equal(canola$get("phenology.thermaltime.y"), original_y)
    
    # Try another invalid update with traditional nested list syntax
    expect_error(
        canola$set(phenology = list(thermaltime = list(x = c(1), y = c(1, 2, 3))))
    )
    
    # State should still be unchanged
    expect_equal(canola$get("phenology.thermaltime.x"), original_x)
    expect_equal(canola$get("phenology.thermaltime.y"), original_y)
})





# Check the invalid cases of parameter defined

test_that("create_options_manager rejects empty top-level names", {
    expect_error(
        create_options_manager(list()),
        "`defaults` must be a named list"
    )
})

test_that("create_options_manager rejects empty nested names", {
    expect_error(
        create_options_manager(list(
            phenology = list(123)
        )),
        "All lists must be named at path 'phenology'"
    )
})

test_that("create_options_manager rejects names containing dots", {
    expect_error(
        create_options_manager(list(
            "thermaltime.x" = 123
        )),
        "Option names must not contain '.'"
    )

    expect_error(
        create_options_manager(list(
            phenology = list("thermaltime.x" = 123)
        )),
        "Option names must not contain '.'"
    )
})

test_that("create_options_manager rejects validators that are not functions", {
    expect_error(
        create_options_manager(
            defaults = list(a = 1),
            validators = list(a = "not_a_function")
        ),
        "Validators must be functions"
    )
})

test_that("create_options_manager rejects validators with non-existent paths", {
    expect_error(
        create_options_manager(
            defaults = list(a = 1),
            validators = list("b" = function(x) x)
        ),
        "Validator path 'b' does not exist"
    )

    expect_error(
        create_options_manager(
            defaults = list(a = list(x = 1)),
            validators = list("a.y" = function(x) x)
        ),
        "Validator path 'a.y' does not exist"
    )
})

test_that("whole-options validation checks defaults and requires a function", {
    validate <- function(options) {
        if (!identical(options$a, options$b)) stop("options must match")
    }
    expect_error(
        create_options_manager(list(a = 1, b = 2), options_validator = validate),
        "options must match"
    )
    expect_error(
        create_options_manager(list(a = 1), options_validator = TRUE),
        "`options_validator` must be a function or NULL",
        fixed = TRUE
    )
    expect_no_error(create_options_manager(list(a = 1), options_validator = NULL))
})

test_that("whole-options validation checks merged updates atomically", {
    manager <- create_options_manager(
        defaults = list(
            thermaltime = list(x = c(2, 30, 35), y = c(0, 28, 0)),
            thermaltime2 = list(x = c(2, 30, 35), y = c(0, 28, 0)),
            frost_threshold = 0
        ),
        validators = list(
            thermaltime = v_xypair(),
            thermaltime2 = v_xypair(),
            frost_threshold = v_numeric_scalar()
        ),
        options_validator = function(options) {
            if (!identical(options$thermaltime$x, options$thermaltime2$x)) {
                stop("thermaltime x values must match")
            }
        }
    )

    before <- manager$get()
    expect_error(
        manager$set("thermaltime.x" = c(5, 25, 40), frost_threshold = -2),
        "thermaltime x values must match"
    )
    expect_equal(manager$get(), before)
    manager$set(
        "thermaltime.x" = c(5, 25, 40),
        "thermaltime2.x" = c(5, 25, 40),
        frost_threshold = -2
    )
    expect_equal(manager$get("thermaltime.x"), c(5, 25, 40))
    expect_equal(manager$get("thermaltime2.x"), c(5, 25, 40))
    expect_equal(manager$get("frost_threshold"), -2)
    manager$reset()
    expect_equal(manager$get(), before)
})

test_that("whole-options validation runs after individual validators", {
    calls <- new.env()
    calls$count <- 0L
    manager <- create_options_manager(
        list(a = 1),
        validators = list(a = v_numeric_scalar()),
        options_validator = function(options) {
            calls$count <- calls$count + 1L
        }
    )
    expect_equal(calls$count, 1L)
    expect_error(manager$set(a = "invalid"), "single numeric")
    expect_equal(calls$count, 1L)
    manager$set(a = 2)
    expect_equal(calls$count, 2L)
    manager$reset()
    expect_equal(calls$count, 2L)
})

test_that("whole-options validation sees merged collections without schema markers", {
    manager <- create_options_manager(
        list(items = list("_as_collection" = TRUE, first = list(type = "files")), limit = 1),
        list("items._default.type" = v_character_scalar()),
        options_validator = function(options) {
            if ("_as_collection" %in% names(options$items)) stop("unexpected marker")
            if (length(options$items) > options$limit) stop("too many items")
        }
    )
    before <- manager$get()
    expect_error(manager$set("items.second" = list(type = "files")), "too many items")
    expect_equal(manager$get(), before)
    manager$set("items.second" = list(type = "files"), limit = 2)
    expect_equal(manager$get("items.second.type"), "files")
})

collection_manager <- function(counter = NULL) {
    create_options_manager(
        defaults = list(
            collection = list(
                folder = "collection",
                items = list(
                    "_as_collection" = TRUE,
                    code_base = list(type = "files", enabled = TRUE),
                    crop_model = list(type = "files", enabled = FALSE)
                )
            ),
            other = 1
        ),
        validators = list(
            "collection.items._default.type" = function(value) {
                if (!is.null(counter)) counter$count <- counter$count + 1L
                v_character_scalar()(value)
            },
            "collection.items._default.enabled" = v_logical_scalar()
        )
    )
}

test_that("collections add and update complete records and reset", {
    manager <- collection_manager()
    expect_false("_as_collection" %in% names(manager$get("collection.items")))
    manager$set("collection.items.history" = list(type = "history", enabled = FALSE))
    expect_equal(manager$get("collection.items.history"), list(type = "history", enabled = FALSE))
    manager$set("collection.items.history.enabled" = TRUE)
    expect_equal(manager$get("collection.items.history"), list(type = "history", enabled = TRUE))
    manager$reset()
    expect_error(manager$get("collection.items.history"), "not defined")
    expect_false(manager$get("collection.items.crop_model.enabled"))
})

test_that("collection validation checks only changed records", {
    counter <- new.env()
    counter$count <- 0L
    manager <- collection_manager(counter)
    expect_equal(counter$count, 2L)
    counter$count <- 0L
    manager$set(other = 2)
    expect_equal(counter$count, 0L)
    manager$set("collection.items.code_base.enabled" = FALSE)
    expect_equal(counter$count, 1L)
    manager$set(
        "collection.items.code_base.type" = "new",
        "collection.items.code_base.enabled" = TRUE,
        "collection.items.crop_model.enabled" = TRUE
    )
    expect_equal(counter$count, 3L)
})

test_that("collections can contain predefined non-list options", {
    count <- new.env()
    count$value <- 0L
    manager <- create_options_manager(
        list(items = list(
            "_as_collection" = TRUE,
            folder1 = "folder1",
            folder2 = c("folder2", "folder22"),
            code = list(type = "files")
        )),
        list(
            "items.folder1" = v_character_scalar(),
            "items.folder2" = v_character_vector(),
            "items._default.type" = function(value) {
                count$value <- count$value + 1L
                v_character_scalar()(value)
            }
        )
    )
    expect_equal(count$value, 1L)
    manager$set("items.folder1" = "new-folder", "items.folder2" = c("a", "b"))
    expect_equal(count$value, 1L)
    expect_equal(manager$get("items.folder1"), "new-folder")
    expect_equal(manager$get("items.folder2"), c("a", "b"))
    manager$set("items.new" = list(type = "history"), "items.folder1" = "other")
    expect_equal(count$value, 2L)
    expect_equal(manager$get("items.new.type"), "history")
    manager$reset()
    expect_equal(manager$get("items.folder1"), "folder1")
    expect_error(manager$get("items.new"), "not defined")
})

test_that("collection options and records keep their roles and updates are atomic", {
    manager <- create_options_manager(
        list(items = list("_as_collection" = TRUE, folder = "initial", code = list(type = "files"))),
        list("items.folder" = v_character_scalar(), "items._default.type" = v_character_scalar())
    )
    before <- manager$get()
    expect_error(manager$set("items.folder" = list(type = "files")), "cannot be a list")
    expect_error(manager$set("items.folder.type" = "files"), "cannot be a list")
    expect_error(manager$set("items.code" = "not a record"), "must be a named list")
    expect_error(manager$set("items.new" = "not a record"), "must be a named list")
    expect_error(manager$set("items.folder" = c("a", "b")), "single character")
    expect_error(manager$set("items.folder" = "changed", "items.new" = list(wrong = TRUE)))
    expect_equal(manager$get(), before)
    expect_error(
        create_options_manager(
            list(items = list("_as_collection" = TRUE, code = list(type = "files"))),
            list("items._default.type" = v_character_scalar(), "items.code.type" = v_character_scalar())
        ),
        "must use _default"
    )
})

test_that("root collection options can hold NULL", {
    manager <- create_options_manager(
        list("_as_collection" = TRUE, folder = "initial", optional = NULL, code = list(type = "files")),
        list(folder = v_character_scalar(), "_default.type" = v_character_scalar())
    )
    manager$set(folder = "updated")
    expect_equal(manager$get("folder"), "updated")
    expect_error(manager$set(folder = 1), "single character")
    manager$set(optional = "value")
    expect_equal(manager$get("optional"), "value")
    manager$set(optional = NULL)
    expect_null(manager$get("optional"))
    expect_true("optional" %in% names(manager$get()))
    expect_error(manager$set(code = NULL), "must be a named list")
})

test_that("collection records reject missing, unknown, and invalid fields atomically", {
    manager <- collection_manager()
    before <- manager$get()
    expect_error(manager$set("collection.items.new" = list(type = "files")), "Missing field")
    expect_error(manager$set("collection.items.new" = list(type = "files", enabled = TRUE, extra = 1)), "Unknown field")
    expect_error(manager$set("collection.items.code_base.enabled" = "yes"), "collection.items.code_base.enabled")
    expect_error(
        manager$set("collection.items.code_base.type" = "files", "collection.items.code_base.enabled" = NULL),
        "collection.items.code_base.enabled"
    )
    expect_error(manager$set("collection.items.code_base.extra" = 1), "Unknown field")
    expect_error(manager$set(other = 2, "collection.items.new" = list(type = "files")), "Missing field")
    expect_equal(manager$get(), before)
    expect_error(manager$set(collection = 42), "contains a collection")
    expect_error(manager$set(collection = NULL), "contains a collection")
    expect_equal(manager$get(), before)
})

test_that("collection schema and initial records are checked", {
    expect_error(
        create_options_manager(list(items = list("_as_collection" = TRUE))),
        "at least one _default field validator"
    )
    expect_error(
        create_options_manager(
            list(items = list("_as_collection" = TRUE, bad = list(type = 1))),
            list("items._default.type" = v_character_scalar())
        )
    )
    expect_error(
        create_options_manager(
            list(items = list("_as_collection" = TRUE, bad = list(type = "x"))),
            list("items._default.type" = v_character_scalar(), "items._default.enabled" = v_logical_scalar())
        ),
        "Missing field"
    )
    expect_error(
        create_options_manager(
            list(items = list("_as_collection" = TRUE)),
            list("items._default.type.nested" = v_character_scalar())
        ),
        "Invalid collection field validator path"
    )
    expect_error(
        create_options_manager(
            list(items = list("_as_collection" = TRUE, bad = list("_as_collection" = TRUE))),
            list("items._default.type" = v_character_scalar())
        ),
        "cannot be nested"
    )
})

test_that("nested collections are rejected in added records", {
    manager <- create_options_manager(
        list(items = list("_as_collection" = TRUE)),
        list("items._default.settings" = function(value) NULL)
    )
    expect_error(
        manager$set("items.new" = list(settings = list("_as_collection" = TRUE))),
        "cannot be nested"
    )
    expect_length(manager$get("items"), 0)
})

test_that("empty collections and root collections accept new records", {
    manager <- create_options_manager(
        list(items = list("_as_collection" = TRUE)),
        list("items._default.type" = v_character_scalar())
    )
    manager$set("items.new" = list(type = "files"))
    expect_equal(manager$get("items.new.type"), "files")
    manager$reset()
    expect_length(manager$get("items"), 0)

    root <- create_options_manager(
        list("_as_collection" = TRUE),
        list("_default.type" = v_character_scalar())
    )
    root$set(new = list(type = "files"))
    expect_equal(root$get("new.type"), "files")
})

test_that("collection-wide validators still run and failed updates roll back", {
    manager <- create_options_manager(
        list(items = list("_as_collection" = TRUE, first = list(type = "files"))),
        list(
            "items._default.type" = v_character_scalar(),
            items = function(value) {
                if (length(value) > 1) stop("too many items")
            }
        )
    )
    expect_error(manager$set("items.second" = list(type = "files")), "too many items")
    expect_error(manager$get("items.second"), "not defined")
})

test_that("collection names are unique and valid paths", {
    duplicated <- structure(
        list(TRUE, list(type = "a"), list(type = "b")),
        names = c("_as_collection", "same", "same")
    )
    expect_error(
        create_options_manager(
            list(items = duplicated),
            list("items._default.type" = v_character_scalar())
        ),
        "unique names"
    )
    manager <- collection_manager()
    expect_error(
        manager$set(collection = list(items = list("bad.name" = list(type = "files", enabled = TRUE)))),
        "unique names"
    )
})

test_that("ordinary options named _default remain supported", {
    manager <- create_options_manager(
        list("_default" = list(type = "files")),
        list("_default.type" = v_character_scalar())
    )
    manager$set("_default.type" = "history")
    expect_equal(manager$get("_default.type"), "history")

    nested <- create_options_manager(
        list("_default" = list(items = list("_as_collection" = TRUE))),
        list("_default.items._default.type" = v_character_scalar())
    )
    nested$set("_default.items.new" = list(type = "files"))
    expect_equal(nested$get("_default.items.new.type"), "files")
})
