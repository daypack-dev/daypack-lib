## Time

```
{{           literal {
{year}       year
{mon:Xxx}    abbreviated month name (e.g. Jan), casing of 'x' controls the casing
{mon:Xx*}    full month name (e.g. January), casing of first 'x' controls casing of first letter,
             casing of second 'x' controls casing of following letters
{mday:cX}    month day (e.g.  1) character 'c' before 'X' is used for padding
             (leave out character for no padding)
{wday:Xxx}   abbreviated weekday name (e.g. Sun), the casing of 'x' controls the casing
{wday:Xx*}   full weekday name (e.g. Sunday), casing of first 'x' controls casing of first letter,
             casing of second 'x' controls casing of following letters
{hour:cX}    24-hour, character 'c' before 'X' determines padding
{12hour:cX}  12-hour, character 'c' before 'X' determines padding
{min:cX}     minutes, character 'c' before 'X' determines padding
{sec:cX}     seconds, character 'c' before 'X' determines padding
```

## Time slot

Same as above, but first keyword inside `{}` is prefixed with `s` for start time, `e` for end time,
e.g. `{syear}`, `{emday:0X}`

