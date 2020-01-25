# Daypack

Daypack is a basic constraint solving automatic personal task scheduler

This repo contains the core library of Daypack `daypack_lib`, and the cli frontend `daypc`.
Other frontends (e.g. Electron, web) are underway.

`daypack_lib` was primarily developed for basic personal task scheduling (or day planning),
but we are exploring its use in more general scheduling scenarios as well

## Note: Daypack is still WIP

The core scheduling functionalities are largely finished,
but facilities for usage of library in frontend, and the frontend itself
are still underway

## Goal

Daypack as a user-facing personal task scheduler program aims to be ergonomic to use, and relatively featureful

Daypack as a library aims to be powerful enough to accomodate everyday personal task scheduling needs

## Features

Overview

- Automatic scheduling

- Manual scheduling

- (WIP) Multiple user (supported by library, but frontend adoption is WIP)

- (WIP) Taking transit time into account during scheduling

- Strict time preferences to indicate when tasks can be scheduled

Specific types of constraints (or scheduling requests) supported

- `Fixed`

  - Manual scheduling, specifies a task segment starts at a fixed time point

  - E.g. "Meeting starts at 2pm"

- `Shift`

  - Daypack shifts the task segment(s) around and tries to find a spot
  
  - E.g. "Homework takes 2 hours, schedule it for me between 9am-5pm of next 3 days"

- `Split_and_shift`

  - Daypack splits task segment into smaller segments (with some specificed minimum size),
    then shifts them around and tries to find a spot

  - E.g. "This work needs 5 hours of work, I need it done by the end of this week,
    split and shift for me, but all split segments must be at least 1 hour"

- `Split_even`

  - Daypack splits a task segment into evenly sized smaller segments across some specified
    buckets/boundaries with shifting

    - If some buckets are not usable, then Daypack tries to split across remaining
      buckets with larger even splits

  - E.g. "I want to exercise 5 hours, split it evenly across next 7 days, boundaries
    being 1pm-5pm of each day, and then shift"

    - If one day ends up being too full to be used, then Daypack splits across 6 days instead,
      and so on

- `Time_share`

  - Interleave multiple task segments with some specified interval size

  - E.g. "Interleave task A, B, C across 1pm-4pm with interval size of 30 mins" produces
    the following agenda

    - | Time slots    | Task   |
      | ---           | ---    |
      | 1:00pm-1:30pm | Task A |
      | 1:30pm-2:00pm | Task B |
      | 2:00pm-2:30pm | Task C |
      | 2:30pm-3:00pm | Task A |
      | 3:00pm-3:30pm | Task B |
      | 3:30pm-4:00pm | Task C |

- `Push_toward`

  - Similar to shifting, but tries positions closest to a specified time first

  - E.g. "I need this done, which takes 15mins, it needs to be done between 4pm-10pm,
    but I want it as close to 6pm as possible"

## Architecture and limitations

Daypack only uses a backtracking search procedure with pruning (implemented using lazy sequences),
and does not use any advanced or potentially more efficient constraint solving techniques

It is subsequently inferior to a lot of other automatic task scheduling software,
and cannot accomodate very complex scheduling scenarios

Nevertheless, it might still be useful as a simple and standalone personal task scheduler

More detailed docs on the way

Some of the features that Daypack does __NOT__ support

- Resource allocation

  - Doesn't seem to be a useful item for personal TODO list

## Getting started

#### Installation

__TODO__

#### User guide

It is recommended that you at least read through the first chapter to understand how Daypack works internally

This will allow you to understand the behaviour of Daypack more precisely, whether as a user of the frontends
a user of the library

__TODO__

## Demos

__TODO__

## Contributions

#### Ideas

Got a feature request? Feel free to open an issue to start a discussion.

Please note that since Daypack was never designed to be a general solver, there
are things prohibitively expensive to properly implement as a result (short of
adding a general solver into Daypack),
which we may cite as a reason should we reject your feature request

We ask for your understanding should that be the case

#### Code

Code contributions are welcome. Please note that by submitting your original work, you agree to
license your work under the MIT license.

## Acknowledgements

- Cli frontend is heavily inspired by [Taskwarrior](https://taskwarrior.org/), which one of the authors heavily used

- We became aware of [Eva](https://github.com/Procrat/eva) later on as well, and took inspiration from its UI/UX design choices and feature set

  - The underlying architecture was independently designed and developed however

## LICENSE

MIT
