# Changelog

## 0.0.6

- Added `time_expr_parser` to `Time_expr` API

- Added `duration_expr_parser` to `Duration` API

- Removed branching time point expression

- Adjusted grammar for branching time slot expression

- Replaced `skip_space` with `skip_white` so empty spaces across lines are skipped as well

## 0.0.5

- Fixed leap year definition

- Upgraded time expr grammar

- Added ability to pick fragments to enable in time expr API

- Refactored `Search_param` module

## 0.0.4

- Removed year field from cron expression. The year field was previously incorrectly present.

## 0.0.3 (unpublished)

- Extended time pattern expression to include cron expression

- Fixed time pattern search

  - Previously incorrect date times were returned when only year level is specified

- Adjusted `Time_pattern` module interface

- Added code to automatically adjust search parameter during time pattern search when applicable

## 0.0.2

- Added non-integer support for numbers in duration string at day, hour, minute level

- Adjusted `Time` module API

## 0.0.1

- Base version
