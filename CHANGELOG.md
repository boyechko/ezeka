# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

## [0.2.0] - 2025-07-18

### Fixed

- `ezeka-toggle-header-read-only` makes the full file content writeable.
- Breadcrumbs: multiple fixes to improve behavior when dropping or following trails.
- `ezeka-note-moved-p` no longer incorrectly matches IDs outside of `oldnames`.
- Various bugs in `ezeka-header-rubric-regexp`, `ezeka-format-metadata`,
  `ezeka-kill-ring-save-link`, etc.
- `ezeka-octavo` commands now avoid requiring `vertico` but use it when available.

### Added

- `README.md` with installation instructions and basic reference.
- `CHANGELOG.md` with major milestones.
- `ezeka-system-log-trail` and `ezeka-view-system-log-entries` to display
  relevant system log entries.
- `ezeka-add-change-log-entry` and helpers make it easier to add change log
  entries interactively.
- `ezeka-insert-link-functions` hook that is run with the link being inserted.
- Breadcrumbs are dropped for the link being inserted.
- `ezeka-insert-snippet-text` demotes headings, respects `:noheadings:` tag, and
  puts content of Summary section in a block quote rather than comment block.
- `ezeka-pull-link-forward` and `ezeka-pull-link-backward` to more easily
  manipulate links in writing.
- `ezeka--create-placeholder` now adds a "placeholder" tag when creating a
  placeholder note.

### Refactored

- Move code from `ezeka.el` (was 5361 lines) into `ezeka-base.el`, `ezeka-
  compose.el`, `ezeka-file.el`, `ezeka-meta.el`, `ezeka-syslog.el`, ending up
  with "only" 2430 lines.
- Organize tests into separate files with alphabetical order.

### Removed

- Long-unused files (`ezeka-citar.el`, `ezeka-md.el`, `ezeka-deft.el`, `_ezeka-
  wip.el`) and obsolete functions no longer relevant for the core behavior.

## [0.1.1] - 2022-08-31

### Changed

- Rename "combined title" metadata key `title` to `rubric`.

## [0.1.0] - 2022-08-17

### Added

- Eclectic Zettelkasten is officially born.
- `ezeka-zk.el` for integration with Grant Rosson's Zk and Zk-Index.

### Changed

- Rename package from `zettel` to `ezeka` (i.e. Eclectic Zettelkasten)
- Split code relying on other packages into their own files:
  - `ezeka-citar.el`
  - `ezeka-deft.el`
  - `ezeka-ivy.el`
  - `ezeka-md.el`

## [0.0.1] - 2015-08-31

### Changed

- Move Zettelkasten-related code to `zettel.el`.
- Create Git repository.
