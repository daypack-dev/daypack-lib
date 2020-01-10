# Daypack

Daypack is a lazily bruteforcing automatic personal task scheduler

This repo contains the core library of Daypack `daypack_lib`, and the cli frontend `daypc`.
Other frontends (e.g. Electron, web) are underway.

`daypack_lib` was primarily developed for personal task scheduling (or day planning),
but we are exploring its use in more general scheduling scenarios as well,
e.g. computational tasks scheduling

## Note: Daypack is still WIP

The core scheduling functionalities are largely finished,
but facilities for usage of library in frontend, and the frontend itself
are still underway

## Goal

Daypack as a user-facing personal task scheduler program aims to be ergonomic to use, and relatively featureful

Daypack as a library aims to be powerful enough to accomodate scheduling needs in myraid envrionments

## Features

- Automatic scheduling while allowing manual intervention

  - This feature is accessed by putting in a "Fixed" schedluing requests
    (see user guide for more details)

  - This feature means you can ask Daypack to schedule first few tasks
    automatically, but let you schedule next few ones yourself, and
    hand back control over to Daypack, and so on

- Multiple user (supported by library, but frontend adoption underway)

- Taking transit time into account during scheduling

- Time segments to indicate when tasks should be scheduled

## Getting started

#### Installation

**TODO**

#### User guide

It is recommended that you at least read through the first chapter to understand how Daypack works internally

This will allow you to understand the behaviour of Daypack more precisely, whether as a user of the frontends
a user of the library

**TODO**

## Demos

**TODO**

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
 
  - Final remark would be that it's also not just completely random bruteforcing,
    Daypack lib does have a heavy set of utilities for dealing with time, and
    can for example extract free time slots
    
    - In other words, a lot of pruning is done during the computation of the tree

- More detailed docs on the way

## Acknowledgements

- Cli frontend is heavily inspired by [Taskwarrior](https://taskwarrior.org/), which one of the authors heavily used

- We became aware of [Eva](https://github.com/Procrat/eva) later on as well, and took inspiration from its UI/UX design choices and feature set

  - The underlying architecture was independently designed and developed however

- GUI version is inspired by graphical calendar designs used by programs such as **TODO**

## LICENSE

MIT
