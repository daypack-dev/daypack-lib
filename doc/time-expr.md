# Time expression specification

## Syntax

```
hms =
  | hour
  | hour:minute
  | hour:minute:second

hms_mode =
  | "am"
  | "pm"
  | ... (all other variants with different casing of above)

weekday =
  | "monday"
  | "tuesday"
  | "wednesday"
  | "thursday"
  | "friday"
  | "saturday"
  | "sunday"
  | ... (all prefixes that allow unique match of any of above)
  | ... (all other variants with different casing of above)

month_day =
  | "1" | ... | "31"

day =
  | weekday
  | month_day

month =
  | "january"
  | "february"
  | "march"
  | "april"
  | "may"
  | "june"
  | "july"
  | "august"
  | "september"
  | "october"
  | "november"
  | "december"
  | ... (all prefixes that allow unique match of any of above)
  | ... (all other variants with different casing of above)

tpe =
  | hms [hms_mode]
  | day hms [hms_mode]

tse =
  | tpe "to" tpe
```

## Semantics

We have following semantic functions:
- `eval_tpe : tpe -> int64 Seq.t`
- `eval_te : te -> (int64 * int64) Seq.t`

We define the set of semantic equations as follows:
