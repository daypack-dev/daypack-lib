# Supplementary documentation

The documentation here contains info and examples which aim to help one get started

Refer to the library documentation for information on individual types and functions

## Index

- [Time](time.md)

  - Basic time handling

- [Time segments](time_segments.md)

  - Time segments manipulations for general use and also for performing set operations

- Time finding

  - [Time pattern](time_pattern.md)

    - Finding time or time segments using a pattern similar to cron expression
    
  - [Time expression](time_expr.md)

    - A small language for specifying complex time segments
    
    - Aims to be a more expressive and user-friendly layer over time pattern

- [Schedule](sched.md)

  - Schedule implementation which provides:
  
    - General bookkeeping of task, task instances, task segments, progress tracking
    
    - Scheduling requests (essentially time constraints)
    
    - Time based indexing of scheduled items, allowing efficient viewing of a "slice" of the agenda
