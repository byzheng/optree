[![R-CMD-check.yaml](https://github.com/byzheng/optree/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/byzheng/optree/actions/workflows/R-CMD-check.yaml)[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/optree)](https://cran.r-project.org/package=optree)

[![](http://cranlogs.r-pkg.org/badges/grand-total/optree?color=green)](https://cran.r-project.org/package=optree)
[![](http://cranlogs.r-pkg.org/badges/last-month/optree?color=green)](https://cran.r-project.org/package=optree)
[![](http://cranlogs.r-pkg.org/badges/last-week/optree?color=green)](https://cran.r-project.org/package=optree)



# optree

**A lightweight R package for hierarchical runtime configuration**

optree provides a flexible, mutable, hierarchical options manager for R. It allows packages and projects to define **nested configuration options**, enforce **group validation rules**, and easily **reset to defaults**. Ideal for complex, interdependent settings like crop models, phenology simulations, or multi-module projects.

---

## Features

- **Hierarchical, nested options** (`a.b.c`)  
- **Dot-separated path notation** for both `get()` and `set()` operations  
- **Runtime mutable configuration**  
- **Merge-aware updates** (update only part of a nested group)  
- **Transactional updates** (rollback on validation failure)  
- **Field, group, and whole-options validation** for consistency
- **Validated collections** of dynamically added, consistently structured records
- **Reset all options** to defaults with one call  
- **Minimal dependencies**, lightweight and easy to use  

---

## Installation

### From CRAN

```r
install.packages('optree')
```

### From GitHub (development version)

```r
remotes::install_github('byzheng/optree')
```

---

## Documentation

For detailed documentation and reference, visit: https://optree.bangyou.me

---

## Getting Started

1. Create an options manager

```r
library(optree)

# Create the CANOLA options manager with built-in validators
canola <- create_options_manager(
  defaults = list(
    phenology = list(
      thermaltime = list(
        x = c(2, 30, 35),
        y = c(0, 28, 0)
      )
    ),
    frost_threshold = 0,
    model_name = "canola_v1",
    backend = "cpu"
  ),
  validators = list(
    "phenology.thermaltime" = v_xypair(min_len = 3),
    "frost_threshold" = v_numeric_range(min = -5, max = 5),
    "model_name" = v_character_scalar(),
    "backend" = v_enum(c("cpu", "gpu"))
  )
)
```

2. Access options

```r
# Get a single leaf
canola$get("phenology.thermaltime.x")

# Get the entire group
canola$get("phenology.thermaltime")
```


3. Update options

```r
# Method 1: Use dot-separated paths (NEW!)
canola$set("phenology.thermaltime.x" = c(5,25,40))
canola$set("phenology.thermaltime.y" = c(0,20,0))

# Method 2: Use nested list (traditional way)
canola$set(phenology = list(
  thermaltime = list(
    x = c(10,20,30),
    y = c(0,10,0)
  )
))

# Mix both styles in one call
canola$set(
  "phenology.thermaltime.x" = c(15,25,35),
  frost_threshold = -2
)

# Update top-level option
canola$set(frost_threshold = -2)
```

Validator example:

```r
# v_enum validator - value must be one of the allowed choices
canola$set(backend = "quantum")
# Error: must be one of: cpu, gpu

# v_numeric_range validator - frost_threshold must be between -5 and 5
canola$set(frost_threshold = 10)
# Error: must be between -5 and 5

# v_xypair validator - x and y must have same length and meet min/max length constraints
canola$set(phenology = list(thermaltime = list(
  x = c(1, 2),
  y = c(0, 1, 2)
)))
# Error: 'x' and 'y' must have the same length

# v_character_scalar validator - model_name must be non-empty string
canola$set(model_name = "")
# Error: must not be empty
```

5. Transactional safety

```r
# Set valid values
canola$set("phenology.thermaltime.x" = c(5,25,40))
canola$set("phenology.thermaltime.y" = c(0,20,0))

# Try an invalid update - will fail and rollback
canola$set("phenology.thermaltime.x" = c(1,2))  # length mismatch
# Error: x and y must have same length

# Options remain unchanged after failed validation
canola$get("phenology.thermaltime.x")  # Still c(5,25,40)
```

6. Reset options

```r
canola$reset()
# Returns all defaults
canola$get()
```

## Whole-options validation

Use `options_validator` for rules involving different option paths. It takes
the complete options list and calls `stop()` when the values are invalid:

```r
paired <- create_options_manager(
  defaults = list(
    thermaltime = list(x = c(2, 30, 35), y = c(0, 28, 0)),
    thermaltime2 = list(x = c(2, 30, 35), y = c(0, 28, 0))
  ),
  validators = list(
    thermaltime = v_xypair(),
    thermaltime2 = v_xypair()
  ),
  options_validator = function(options) {
    if (!identical(options$thermaltime$x, options$thermaltime2$x)) {
      stop("thermaltime x values must match")
    }
  }
)

# Update both values together so the merged options remain valid
paired$set(
  "thermaltime.x" = c(5, 25, 40),
  "thermaltime2.x" = c(5, 25, 40)
)
```

The whole-options validator runs on defaults at creation and on the complete
merged candidate after individual validators on every `set()`. A failed
validation leaves all options unchanged. It is optional and skipped when
`NULL`; `reset()` restores the validated defaults without rerunning it.
Ordinary path validators run on updates, not on defaults at creation.
See the [Validators vignette](vignettes/validators.Rmd) for more examples.

## Collections

Use a collection when you need to add named records at runtime. Mark a group
with `_as_collection = TRUE` in `defaults`, then define the required fields
with `_default` validators:

```r
library(optree)

sources <- create_options_manager(
  defaults = list(items = list(
    "_as_collection" = TRUE,
    folder = "collection",
    code = list(type = "files", enabled = TRUE)
  )),
  validators = list(
    "items.folder" = v_character_scalar(),
    "items._default.type" = v_character_scalar(),
    "items._default.enabled" = v_logical_scalar()
  )
)

sources$set("items.folder" = "new-collection")
sources$set("items.models" = list(type = "files", enabled = FALSE))
sources$set("items.models.enabled" = TRUE)
sources$get("items.models")
```

Predefined non-list values, such as `folder`, are ordinary options. All records
must have exactly the declared fields. New and changed records are validated
without revalidating unchanged records; `reset()` restores the initial collection.
See the [Collections vignette](vignettes/collections.Rmd) for details.


## Advantages over existing approaches

| Feature | optree | settings | base R |
| Store nested lists | ✅ Named lists | ✅ | ✅ |
| Nested-path get/set | ✅ | ❌ | ❌ |
| Partial nested updates | ✅ | Manual | Manual |
| Custom group validation | ✅ | ✅ | Manual |
| Dynamic validated collections | ✅ Built-in | Manual | Manual |


## Built-in Validators

optree provides a collection of ready-to-use validators for common validation patterns:

- **`v_numeric_scalar()`** – Validates single numeric values
- **`v_numeric_range(min, max)`** – Validates numeric values within a range
- **`v_numeric_vector(min_len, finite)`** – Validates numeric vectors with length/finiteness constraints (`min_len` must be a non-negative whole number; `finite` must be a single logical)
- **`v_logical_scalar()`** – Validates single logical values
- **`v_character_scalar()`** – Validates single non-empty character values
- **`v_enum(choices)`** – Validates that value is one of predefined choices
- **`v_xypair(min_len, max_len = NULL)`** – Validates paired x/y lists with optional upper length bound (`max_len` must be `NULL` or a non-negative whole number)

For more details and examples, see the [Validators Vignette](vignettes/validators.Rmd).

## Contributing

Contributions are welcome! Please submit issues or pull requests via GitHub.

## License

MIT License – see LICENSE file for details.