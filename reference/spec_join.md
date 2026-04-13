# Join multiple spectral tibbles

\`spec_join()\` combines two or more spectral tibbles that share the
same wavenumber column. The function performs a full join operation,
keeping all wavenumber values from all input tibbles.

## Usage

``` r
spec_join(..., wn_col = NULL, join_type = "full", suffix = c(".x", ".y"))
```

## Arguments

- ...:

  Two or more tibbles containing spectral data

- wn_col:

  Character string specifying the wavenumber column name. If NULL, uses
  the globally set wavenumber column from \`set_spec_wn()\`.

- join_type:

  Character string specifying the type of join operation. Options:
  "full" (default), "inner", "left". See Details.

- suffix:

  Character vector of length 2 specifying suffixes to add to
  non-wavenumber columns when there are naming conflicts. Default:
  c(".x", ".y")

## Value

A tibble containing the joined spectral data

## Details

Join types: - "full": Keep all wavenumber values from all tibbles
(default) - "inner": Keep only wavenumber values present in all
tibbles - "left": Keep wavenumber values from the first tibble

The function will show warnings about the wavenumber column being used
(similar to other tidyspec functions) and will handle missing values
appropriately.

## Examples

``` r
if (FALSE) { # \dontrun{
# Set wavenumber column
set_spec_wn("Wavenumber")

# Join two spectral datasets
joined_data <- spec_join(CoHAspec, other_spec_data)

# Join multiple datasets with inner join
joined_data <- spec_join(spec1, spec2, spec3, join_type = "inner")

# Specify wavenumber column explicitly
joined_data <- spec_join(spec1, spec2, wn_col = "Wavenumber")
} # }
```
