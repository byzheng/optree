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
    expect_error(
        canola$set(thermaltime = list(x = c(1, 2, 3))),
        "thermaltime must be a list with both x and y"
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
