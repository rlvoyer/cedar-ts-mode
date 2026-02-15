# cedar-ts-mode

An Emacs major mode for editing [Cedar policy language](https://www.cedarpolicy.com/) files, powered by tree-sitter.

## Features

- **Syntax highlighting** — full font-lock support for keywords, strings, numbers, operators, types, annotations, builtins, function calls, and properties
- **Indentation** — automatic indentation for policy bodies, conditions, record/set literals, and argument lists
- **Navigation** — `beginning-of-defun` / `end-of-defun` (`C-M-a` / `C-M-e`) to jump between policies; sentence and sexp navigation within policies
- **Imenu** — index of policies labeled by effect and `@id` annotation (e.g., `permit: grant-photo-access`)
- **Comment support** — `M-;` for toggling `//` comments

## Requirements

- Emacs 30.1 or later (with built-in tree-sitter support)
- The `cedar` tree-sitter grammar

## Installation

### 1. Install the tree-sitter grammar

The grammar source is registered automatically when this package is loaded. Simply run:

```
M-x treesit-install-language-grammar RET cedar RET
```

This downloads and compiles the grammar from [chrnorm/tree-sitter-cedar](https://github.com/chrnorm/tree-sitter-cedar).

### 2. Install the package

#### Manual

Clone this repository and add it to your `load-path`:

```elisp
(add-to-list 'load-path "/path/to/cedar-ts-mode")
(require 'cedar-ts-mode)
```

#### use-package (with manual path)

```elisp
(use-package cedar-ts-mode
  :load-path "/path/to/cedar-ts-mode")
```

#### use-package (with vc-use-package, Emacs 30+)

```elisp
(use-package cedar-ts-mode
  :vc (:url "https://github.com/rlvoyer/cedar-ts-mode" :branch "main"))
```

#### use-package (with straight.el)

```elisp
(use-package cedar-ts-mode
  :straight (cedar-ts-mode :type git :host github :repo "rlvoyer/cedar-ts-mode"))
```

## Configuration

### Indent offset

The default indentation is 2 spaces. To change it:

```elisp
(setq cedar-ts-mode-indent-offset 4)
```

### Font-lock levels

By default, levels 1-3 are enabled (comments, keywords, strings, builtins, annotations, constants, numbers, types, functions, properties). To enable level 4 (operators, brackets, delimiters):

```elisp
(add-hook 'cedar-ts-mode-hook
          (lambda () (treesit-font-lock-recompute-features '(operator bracket delimiter))))
```

## License

GNU General Public License v3.0 or later. See [LICENSE](LICENSE) for the full text.
