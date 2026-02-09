# optree

**A lightweight R package for hierarchical runtime configuration**

optree provides a flexible, mutable, hierarchical options manager for R. It allows packages and projects to define **nested configuration options**, enforce **group validation rules**, and easily **reset to defaults**. Ideal for complex, interdependent settings like crop models, phenology simulations, or multi-module projects.

---

## Features

- **Hierarchical, nested options** (`a.b.c`)  
- **Dot-separated path notation** for both `get()` and `set()` operations  
- **Runtime mutable configuration**  
- **Merge-aware updates** (update only part of a nested group)  
- **Group and field validation** for consistency  
- **Reset all options** to defaults with one call  
- **Minimal dependencies**, lightweight and easy to use  

---

## Installation

Currently on [Github](https://github.com/byzheng/optree) only. Install with:

```r
remotes::install_github('byzheng/optree')
```


## Getting Started

1. Create an options manager

```r
library(optree)


# Define a validator for thermaltime group
thermaltime_validator <- function(value) {
  if (!is.list(value) || !all(c("x","y") %in% names(value))) {
    stop("thermaltime must be a list with both x and y")
  }
  if (length(value$x) != length(value$y)) stop("x and y must have same length")
}


# Create the CANOLA options manager
canola <- create_options_manager(
  defaults = list(
    phenology = list(
      thermaltime = list(
        x = c(2,30,35),
        y = c(0,28,0)
      )
    ),
    frost_threshold = 0
  ),
  validators = list(
    "phenology.thermaltime" = thermaltime_validator
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
# Will fail because x and y lengths mismatch
canola$set(phenology = list(thermaltime = list(
  x = c(1,2),
  y = c(0,1,2)
)))
# Error: x and y must have same length
```

4. Reset options

```r
canola$reset()
# Returns all defaults
canola$get()
```


## Advantages over existing approaches

| Feature | optree | settings::options_manager | base R options() |
|---------|--------|---------------------------|------------------|
| Hierarchical options | ✅ | Limited | ❌ |
| Merge-aware updates | ✅ | ❌ | ❌ |
| Runtime mutable | ✅ | ✅ | ✅ |
| Group validation | ✅ | Custom only | ❌ |
| Arbitrary depth | ✅ | Limited | ❌ |

## Contributing

Contributions are welcome! Please submit issues or pull requests via GitHub.

## License

MIT License – see LICENSE file for details.