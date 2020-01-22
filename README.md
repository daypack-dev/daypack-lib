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

-

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
