# table.glue 0.0.5

* fixed an error that caused `table_value` to pass non-environment objects to `glue::glue()` on Ubuntu.

# table.glue 0.0.4

* internally, prefer `glue_data` over `glue` when data source is a list (Thanks to @jennybc for the [PR](https://github.com/bcjaeger/table.glue/pull/1)!).

# table.glue 0.0.3

* updated `table_value()` to allow formatting of missing values.

* included `bracket_insert` functions to put labels inside of intervals.

# table.glue 0.0.2

* Added a `NEWS.md` file to track changes to the package.

* included `inline` helper functions to make table data easier to report

* included `nhanes` data

# table.glue 0.0.1

* core functions for rounding specifications

* some helper functions for rounding particular things, e.g., p-values.
