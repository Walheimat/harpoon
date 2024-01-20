# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- `typescript-mode` is now considered to bee tree-sat.
- Variable `harpoon-hook--sisters` to register additional hooks to
  set up.

### Changed

- Messages are now shown after an idle delay of 1 second.

## [0.3.3]

### Added

- Keyword `:flat` to skip setting up completion and syntax checking.
- The defaults for `:completion` are now customizable through
  `harpoon-completion-{delay,prefix}`.
- Variable `harpoon-ligature-provider` which defaults to `ligature`.
  Leaves space open for alternatives and their implementation.
- Variable `harpoon-lsp-provider` which defaults to `lsp-mode`.

### Removed

- Keywords `:corfu` and `:major` have been removed. Use `:completion`
  and `:bind` instead.
- The defaults for `harpoon-lsp-{function,dir-ignore-list}` are now
  set to the `lsp-mode` defaults. The default for
  `harpoon-checker-function` is `flycheck-mode`.
- Variables `harpoon-lsp-{function,dir-ignore-list}`.
- Variable `harpoon-completion-auto` in order to respect the global
  value (of `corfu-auto` for example).
- Variable `harpoon-completion-key` that would locally bind it to
  `completion-at-point`.

### Fixed

- Only actually created functions are logged as created.

## [v0.3.2]

### Added

- Variables `harpoon-bind-name-suffix` and `harpoon-bind-function`.
  The latter will be called to construct a symbol to bind to the
  prefix key. The function by default constructs it using the prior.
- Keyword `:bind` now accepts a symbol that will be bound to
  (superseding its construction).

### Changed

- Keyword `:major` was replaced by `:bind`. Variable
  `harpoon-major-key` was replaced by `harpoon-bind-key`.
- Custom variable `harpoon-prefer-tabs` was renamed to
  `harpoon-tabs-prefer`.

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
