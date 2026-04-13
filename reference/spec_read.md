# Read Spectral Data from Various File Formats

This function reads spectral data from a file, automatically detecting
the format and using either \`readr\` or \`readxl\` functions. It also
sets the specified wavenumber column as the default using
\`set_spec_wn()\`.

## Usage

``` r
spec_read(file, wn_col, ...)
```

## Arguments

- file:

  Path to the file to be read.

- wn_col:

  Character. Name of the column containing wavenumber values. This
  column will be set as the default wavenumber column.

- ...:

  Additional arguments passed to \`readr::read_delim()\`,
  \`readr::read_csv()\`, or \`readxl::read_excel()\`, depending on the
  file format.

## Value

A tibble containing the spectral data.

## Details

The function automatically determines the file format based on the
extension: - \`.txt\`, \`.csv\`, \`.tsv\`, and \`.csv2\` are read using
\`readr\` functions. - \`.xls\` and \`.xlsx\` are read using
\`readxl::read_excel()\`.

If the specified \`wn_col\` does not exist in the data, an error is
returned.

## Examples

``` r
# \donttest{
  file_path <- system.file("extdata", "CoHAspec.csv", package = "tidyspec")
  df <- spec_read(file_path, wn_col = "Wavenumber")
#> Rows: 1868 Columns: 5
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> dbl (5): Wavenumber, CoHA01, CoHA025, CoHA05, CoHA100
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Successfully set 'Wavenumber' as the default wavenumber column.
#> File loaded successfully! Wavenumber column set to: Wavenumber
  check_wn_col() # Verifica se a coluna de wavenumber está definida
#> The current wavenumber column is: Wavenumber
# }
```
