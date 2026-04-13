# Scree plot for PCA results

Creates a customizable scree plot based on a \`prcomp\` object showing
variance explained by each component.

## Usage

``` r
spec_pca_screeplot(
  pca,
  n = 10,
  show_labels = TRUE,
  show_cumulative = TRUE,
  bar_color = "steelblue",
  line_color = "darkred",
  show_kaiser = FALSE,
  title = "Scree Plot",
  subtitle = NULL,
  accuracy = 1
)
```

## Arguments

- pca:

  A PCA object returned by \[prcomp()\].

- n:

  Number of components to display. Defaults to 10.

- show_labels:

  Logical. Show percentage labels on bars? Default is TRUE.

- show_cumulative:

  Logical. Show cumulative variance line? Default is TRUE.

- bar_color:

  Fill color for bars. Default is "steelblue".

- line_color:

  Color of the cumulative line and points. Default is "darkred".

- show_kaiser:

  Logical. Show Kaiser criterion line? Default is FALSE.

- title:

  Plot title. Default is "Scree Plot".

- subtitle:

  Optional plot subtitle.

- accuracy:

  Number of decimal places for variance percentages. Default is 1.

## Value

A ggplot2 scree plot object.

## Examples

``` r
pca <- prcomp(USArrests, scale. = TRUE)
spec_pca_screeplot(pca, n = 4)

spec_pca_screeplot(pca, show_kaiser = TRUE, bar_color = "darkblue")
```
