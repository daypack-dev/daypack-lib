# Daypack

Daypack is a lazily bruteforcing automatic personal task scheduler

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

Daypack as a library aims to be powerful enough to accomodate basic scheduling needs in myraid envrionments

## Features

- Automatic scheduling

- Manual scheduling

- (WIP) Multiple user (supported by library, but frontend adoption is WIP)

- (WIP) Taking transit time into account during scheduling (WIP)

- Time slots to indicate when tasks can be scheduled

  - Essentially strict time preference

## Characteristics and limitations

Daypack does not utilise any constraint solving or AI, and subsequently inferior
to a lot of other automatic task scheduling software feature wise, and cannot accomodate
complex scheduling scenarios

But we believe it might still be useful as a personal task scheduler due to the potentially leaner memory
profile (space complexity of bruteforcing procedure is linear to number of total task
segments derived from scheduling requests)

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

## Architecture and design

- As mentioned above, Daypack is not very smart, and just bruteforces for solutions

  - Essentially it does a Depth-first Search on a schedule tree, where

    - Each node represents a schedule

    - Each branch represents a possible way of handling a task scheduling request,
      i.e. possible scheduling choice that leads to another schedule

  - Computing the entire tree obviously explodes in time complexity as well as memory complexity
  
  - The solution used is lazy construction of said tree, which in essence means

    - Only one branch of the tree is stored in the memory at any given point (branch picked follows DFS pattern),
      this flattens the memory complexity

    - Still carry the same time complexity for generation of a single schedule,
      though generating first schedule is a lot faster than generating all schedules in practice/most cases
 
  - Finally, it's not just completely random bruteforcing,
    `daypack_lib` has a (heavy) set of utilities for dealing with time, and
    can for example extract free time slots.
    A lot of pruning of computation is done as a result.

- More detailed docs on the way

## Contributions

#### Ideas

Got a feature request? Feel free to open an issue to start a discussion.

Please note that since Daypack was never designed to be a full blown "solver", there
are things prohibitively expensive to properly implement as a result (short of
adding a proper solver into Daypack),
which we may cite as a reason should we reject your feature request.

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
