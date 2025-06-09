# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

### Fixed

- `ezeka-breadcrumbs-find-linear-trail` fully works.
- `ezeka-toggle-header-read-only` makes the full file content writeable.
- Various bugs in `ezeka-header-rubric-regexp`, `ezeka-format-metadata`,
  `ezeka-kill-ring-save-link`, `ezeka--breadcrumbs-string`, etc.

### Added

- `README.md` with installation instructions and basic reference.
- `CHANGELOG.md` with major milestones.
- `ezeka-system-log-trail` and `ezeka-view-system-log-entries` to display
  relevant system log entries.
- `ezeka--read-change-log-entry` to make adding change log entries more
  interactive.
- `ezeka-insert-link-functions` hook that is run with the link being inserted.
- Breadcrumbs are dropped for the link being inserted.
- `ezeka-insert-snippet-text` demotes headings, respects `:noheadings:` tag, and
  puts content of Summary section in a block quote rather than comment block.

### Refactored

- Begin modularizing `ezeka.el` (was 5361 lines) into:
  - `ezeka-base.el`
  - `ezeka-file.el`
  - `ezeka-meta.el`
  - `ezeka-syslog.el`
  - `ezeka-compose.el`

### Removed

- `ezeka--link-with-metadata`
- `ezeka-org-set-todo-properties`
- `ezeka-generate-n-new-ids`

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
