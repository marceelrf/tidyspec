# Set the Default Wavenumber Column

Defines a default wavenumber column name to be used by other functions
in the package when \`wn_col\` is not explicitly provided.

## Usage

``` r
set_spec_wn(col_name)
```

## Arguments

- col_name:

  A string specifying the name of the wavenumber column.

## Value

Invisibly returns the assigned column name.

## See also

\[spec_select()\]

## Examples

``` r
set_spec_wn("Wavenumber")
#> Successfully set 'Wavenumber' as the default wavenumber column.
```
