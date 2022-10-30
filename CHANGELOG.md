# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [2.3.2] - October 29, 2022

### Changed

- Makes optimized function slightly more optimized.

## [2.3.1] - October 29, 2022

### Added

- Adds optimized versions of several functions with less currying.

## [2.3.0] - September 22, 2022

### Changed

- Makes `fold`, `sampleOn`, and `fix` nicer to work with.

## [2.2.0] - September 10, 2022

### Changed

- Simplifies signatures from `AnEvent m` to `Event` and gets rid of unnecessary functions.

## [2.1.0] - August 21, 2022

### Added

- A new `Zora` monad backing `AnEvent` that simplifies types and typeclasses.
- A monkey-patchable backdoor for most event functions that makes deubgging slightly easier.

## [2.0.0] - June 20, 2022

### Added

- A CHANGELOG.
- A basic Elliott/Hudak/Freeman FRP implementation.