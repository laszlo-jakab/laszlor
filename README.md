# laszlor

This package contains general utility functions I use. For now, it contains custom functions for formatting `R` regression output for display.

The workhorse function is `RegTable`, which takes a list of regression model outputs (e.g. `lm` or `felml` objects), and gives a neat `data.table` of results. Similar to `stargazer`, but the output is more flexible. 
