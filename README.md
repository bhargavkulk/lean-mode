# lean-mode

A small Lean 4 major mode for Emacs.

It provides:

- syntax highlighting for Lean;
- enhanced relative indentation;
- a Lean input method with the included `data/abbreviations.json`; and
- Eglot integration with `lake serve`.

## Requirements

- Emacs 29.1 or newer;
- Eglot (included with Emacs 29+); and
- a Lean project using Lake, with `lake` available on `PATH`.

## Installation with Elpaca

The `data/*.json` entry is intentional: Elpaca's default file set includes
Elisp files but not arbitrary JSON data files.

```elisp
(use-package lean-mode
  :ensure (:host github
           :repo "bhargavkulk/lean-mode"
           :files (:defaults "data/*.json"))
  :demand t)
```

After installation, files ending in `.lean` use `lean-mode`. In a Lean
project, the mode starts Eglot automatically when it finds a
`lean-toolchain` file at the project root.

## Input method

The input method is named `Lean`. For example, typing `\\alpha` produces `α`.
Use `M-x set-input-method RET Lean` to activate it manually.

## License

Apache License 2.0. See [LICENSE](LICENSE).
