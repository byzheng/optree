# Example: Using dot-separated paths in set()
# This demonstrates the new feature added to optree

library(optree)

# Define a validator for thermaltime group
thermaltime_validator <- function(value) {
  if (!is.list(value) || !all(c("x", "y") %in% names(value))) {
    stop("thermaltime must be a list with both x and y")
  }
  if (length(value$x) != length(value$y)) {
    stop("thermaltime x and y must have same length")
  }
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

cat("=== Initial values ===\n")
print(canola$get())

cat("\n=== Example 1: Using dot-separated path ===\n")
canola$set("phenology.thermaltime.x" = c(5, 25, 40))
cat("After setting 'phenology.thermaltime.x' to c(5, 25, 40):\n")
cat("x:", canola$get("phenology.thermaltime.x"), "\n")
cat("y:", canola$get("phenology.thermaltime.y"), "(unchanged)\n")

cat("\n=== Example 2: Update y with dot path ===\n")
canola$set("phenology.thermaltime.y" = c(0, 20, 0))
cat("After setting 'phenology.thermaltime.y' to c(0, 20, 0):\n")
cat("x:", canola$get("phenology.thermaltime.x"), "\n")
cat("y:", canola$get("phenology.thermaltime.y"), "\n")

cat("\n=== Example 3: Mix both styles ===\n")
canola$set(
  "phenology.thermaltime.x" = c(10, 30, 45),
  frost_threshold = -2
)
cat("After mixed call:\n")
cat("x:", canola$get("phenology.thermaltime.x"), "\n")
cat("frost_threshold:", canola$get("frost_threshold"), "\n")

cat("\n=== Example 4: Traditional nested list still works ===\n")
canola$set(phenology = list(
  thermaltime = list(
    x = c(15, 35, 50),
    y = c(1, 25, 1)
  )
))
cat("After nested list update:\n")
print(canola$get("phenology.thermaltime"))

cat("\n=== Example 5: Reset to defaults ===\n")
canola$reset()
print(canola$get())

cat("\n=== Example 6: Validation still works with dot paths ===\n")
tryCatch({
  canola$set("phenology.thermaltime.x" = c(1, 2))  # length 2
  canola$set("phenology.thermaltime.y" = c(0, 1, 2))  # length 3 - should fail
}, error = function(e) {
  cat("ERROR (expected):", e$message, "\n")
})

cat("\nAll examples completed successfully!\n")
