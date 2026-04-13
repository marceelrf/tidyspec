# Check the Currently Set Wavenumber Column

This function checks which column has been set as the default wavenumber
column. If no column has been set, it prints a message prompting the
user to define one.

## Usage

``` r
check_wn_col(show_val = FALSE)
```

## Arguments

- show_val:

  Logical. If \`TRUE\`, the function returns the name of the current
  wavenumber column. Default is \`FALSE\`.

## Value

If \`show_val = TRUE\`, returns the name of the wavenumber column
(character). Otherwise, it only prints a message.

## Examples

``` r
set_spec_wn("Wavenumber")  # Set the wavenumber column
#> Successfully set 'Wavenumber' as the default wavenumber column.
check_wn_col()             # Check which column is set
#> The current wavenumber column is: Wavenumber
check_wn_col(show_val = TRUE)  # Check and return the column name
#> The current wavenumber column is: Wavenumber
#> [1] "Wavenumber"
```
