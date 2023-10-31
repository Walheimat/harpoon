# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [v0.3.1]

### Added

- Custom variable `harpoon-log` that when set to `t` will log
  information during macro expansion to a custom log buffer (that can
  be switched to using `harpoon-pop-to-log-buffer`.
- Warnings can now be suppressed when `harpoon-suppress-warnings` is
  `t`.

### Fixed

- Completion now defaults for old cons format.

## [v0.3.0]

### Added

- Plist passed to `:lsp` now honors `:function` to set function other
  than `harpoon-lsp-function` and `:dir-ignore-list` to set variable
  other than `harpoon-lsp-dir-ignore-list`.
- Value passed to `:completion` can now also be a plist with keys
  `:provider`, `:delay`, `:prefix` and `:auto` to configure completion
  (`corfu` is still the only supported provider).
- New key `:checker` to set the function to call for syntax checks. It
  can also be set using new custom variable
  `harpoon-checker-function`.

## [v0.2.0]

### Added

- Switched to using `dinghy`.
- Custom variable `harpoon-completion-provider` that defaults to
  current only option `'corfu`. See below.

### Changed

- Keyword `corfu` is now `completion` to allow for eventually
  considering other completion libraries.

## [v0.1.0]

Initial version as an extraction of my config package.
