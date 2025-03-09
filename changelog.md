## [0.5] - UNRELEASED

- instance MonadException ComposeT

## [0.4] - 2024-12-15

- fix for GHC 9.8, 9.10
- add refPutRestore

## [0.3] - 2023-07-25

- LifecycleT: improve to make closing more robust
- add bracketNoMask_

## [0.2] - 2023-07-31

- fix "Prod" type, add more functions
- WithT: rename from TransformT, add more instances & functions
- refRestore etc.: more general because no masking of async exceptions
- remove some unnecessary classes and constraints

## [0.1] - 2022-09-12

- initial release
