# Supplementary documentation

The documentation here contains info and examples which aim to help one get started

Refer to the library documentation for information on individual types and functions

## Index

- [Time](time.md)

  - Basic time handling

- [Time slots](time_slots.md)

  - Time slots/intervals manipulations for general use and also for performing set operations

- Time finding

  - [Time pattern](time_pattern.md)

    - Finding time or time segments using a pattern similar to cron expression
    
  - [Time expression](time_expr.md)

    - A small language for specifying complex time segments
    
    - Aims to be a more expressive and user-friendly layer over time pattern

- [Schedule](sched.md)

  - Schedule implementation which provides:
  
    - Bookkeeping of task, task instances, task segments

    - Progress tracking
    
    - Scheduling requests (essentially time constraints)
    
    - Time based indexing of scheduled items, allowing efficient navigation of the agenda based on time
