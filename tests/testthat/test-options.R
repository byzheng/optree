# Test options manager functionality
thermaltime_validator <- function(value) {
    if (!is.list(value) || !all(c("x", "y") %in% names(value))) {
        stop("thermaltime must be a list with both x and y")
    }
    if (length(value$x) != length(value$y)) stop("thermaltime x and y must have same length")
}

canola <- create_options_manager(
    defaults = list(
        thermaltime = list(
            x = c(2, 30, 35),
            y = c(0, 28, 0)
        ),
        frost_threshold = 0
    ),
    validators = list(
        "thermaltime" = thermaltime_validator
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
        canola$set(thermaltime = list(x = c(1, 2), y = c(0, 1, 2))),
        "thermaltime x and y must have same length"
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
thermaltime_validator <- function(value) {
    if (!is.list(value) || !all(c("x", "y") %in% names(value))) {
        stop("thermaltime must be a list with both x and y")
    }
    if (length(value$x) != length(value$y)) stop("thermaltime x and y must have same length")
}

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
        "phenology.thermaltime" = thermaltime_validator
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



