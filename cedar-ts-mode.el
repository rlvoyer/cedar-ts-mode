;;; cedar-ts-mode.el --- Tree-sitter support for Cedar policy files -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Robert Voyer

;; Author: Robert Voyer
;; Keywords: languages cedar tree-sitter
;; URL: https://github.com/rlvoyer/cedar-ts-mode
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A major mode for editing Cedar policy language files (.cedar), powered
;; by Emacs's built-in tree-sitter support.  Provides syntax highlighting,
;; indentation, navigation, and Imenu integration.
;;
;; Requires the tree-sitter grammar for Cedar.  Install it with:
;;
;;   M-x treesit-install-language-grammar RET cedar RET
;;
;; The grammar source is registered automatically when this package is loaded.

;;; Code:

(require 'treesit)

(defgroup cedar-ts nil
  "Tree-sitter support for Cedar policy files."
  :group 'languages
  :prefix "cedar-ts-mode-")

(defcustom cedar-ts-mode-indent-offset 2
  "Number of spaces for each indentation level in `cedar-ts-mode'."
  :type 'integer
  :group 'cedar-ts)

;; Grammar source registration
(add-to-list 'treesit-language-source-alist
             '(cedar "https://github.com/chrnorm/tree-sitter-cedar"))

;;; Syntax table

(defvar cedar-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 12" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?{ "(}" table)
    (modify-syntax-entry ?} "){" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?_ "_" table)
    table)
  "Syntax table for `cedar-ts-mode'.")

;;; Font-lock

(defun cedar-ts-mode--font-lock-settings ()
  "Return tree-sitter font-lock settings for Cedar."
  (treesit-font-lock-rules

   :language 'cedar
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'cedar
   :feature 'keyword
   '([(permit) (forbid) (when) (unless)] @font-lock-keyword-face
     ["if" "then" "else"] @font-lock-keyword-face
     ["has" "like" "is"] @font-lock-keyword-face)

   :language 'cedar
   :feature 'string
   '((str) @font-lock-string-face)

   :language 'cedar
   :feature 'builtin
   '([(principal) (action) (resource) (context)
      (all_principals) (all_actions) (all_resources)]
     @font-lock-builtin-face)

   :language 'cedar
   :feature 'annotation
   :override t
   '((annotation) @font-lock-preprocessor-face)

   :language 'cedar
   :feature 'constant
   '([(true) (false)] @font-lock-constant-face)

   :language 'cedar
   :feature 'number
   '((int) @font-lock-number-face)

   :language 'cedar
   :feature 'type
   '((entity type: (path) @font-lock-type-face)
     (principal_is_constraint right: (path) @font-lock-type-face)
     (resource_is_constraint right: (path) @font-lock-type-face)
     (is_expression right: (path) @font-lock-type-face))

   :language 'cedar
   :feature 'function
   '((call_expression
      function: (selector_expression
                 field: (field_identifier) @font-lock-function-call-face))
     (ext_fun_call (path) @font-lock-function-call-face)
     ["contains" "containsAll"] @font-lock-function-call-face)

   :language 'cedar
   :feature 'property
   '((selector_expression field: (field_identifier) @font-lock-property-use-face))

   :language 'cedar
   :feature 'operator
   '(["==" "!=" "<" "<=" ">" ">=" "&&" "||" "+" "-" "*" "!" "in"]
     @font-lock-operator-face)

   :language 'cedar
   :feature 'bracket
   '(["(" ")" "{" "}" "[" "]"] @font-lock-bracket-face)

   :language 'cedar
   :feature 'delimiter
   '(["::" "," ";" "."] @font-lock-delimiter-face)))

;;; Indentation

(defvar cedar-ts-mode--indent-rules
  `((cedar
     ((parent-is "source_file") column-0 0)
     ;; Closing brackets align with opening line
     ((node-is ")") parent-bol 0)
     ((node-is "}") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ;; Scope node indented within policy
     ((node-is "scope") parent-bol cedar-ts-mode-indent-offset)
     ;; Scope children (constraints) align with scope start
     ((parent-is "scope") parent-bol 0)
     ;; Condition body (expressions inside when/unless braces)
     ((parent-is "condition") parent-bol cedar-ts-mode-indent-offset)
     ;; Collections
     ((parent-is "entlist") parent-bol cedar-ts-mode-indent-offset)
     ((parent-is "record_literal") parent-bol cedar-ts-mode-indent-offset)
     ((parent-is "set_literal") parent-bol cedar-ts-mode-indent-offset)
     ((parent-is "argument_list") parent-bol cedar-ts-mode-indent-offset)
     ;; Policy-level children (annotation, condition, effect, parens, semicolon)
     ((parent-is "policy") parent-bol 0)
     (no-node parent-bol 0)))
  "Tree-sitter indentation rules for Cedar.")

;;; Navigation & Imenu

(defun cedar-ts-mode--policy-name (node)
  "Return a descriptive name for a policy NODE for Imenu.
Extracts the effect (permit/forbid) and the @id annotation value
if present."
  (let ((effect "policy")
        (annotation-id nil))
    ;; Find the effect (permit or forbid)
    (dolist (child (treesit-node-children node t))
      (when (string= (treesit-node-type child) "effect")
        (let ((effect-child (treesit-node-child child 0 t)))
          (when effect-child
            (setq effect (treesit-node-type effect-child))))))
    ;; Look for @id annotation
    (dolist (child (treesit-node-children node t))
      (when (string= (treesit-node-type child) "annotation")
        (let ((text (treesit-node-text child)))
          (when (string-match "@id(\"\\([^\"]*\\)\")" text)
            (setq annotation-id (match-string 1 text))))))
    (if annotation-id
        (format "%s: %s" effect annotation-id)
      (format "%s (line %d)" effect
              (line-number-at-pos (treesit-node-start node))))))

;;; Mode definition

;;;###autoload
(define-derived-mode cedar-ts-mode prog-mode "Cedar"
  "Major mode for editing Cedar policy files, powered by tree-sitter.

Requires the `cedar' tree-sitter grammar.  Install it with:
  M-x treesit-install-language-grammar RET cedar RET"
  :syntax-table cedar-ts-mode--syntax-table
  :group 'cedar-ts

  (unless (treesit-ready-p 'cedar)
    (error "Tree-sitter grammar for Cedar is not available.
Install it with: M-x treesit-install-language-grammar RET cedar RET"))

  (treesit-parser-create 'cedar)

  ;; Font-lock
  (setq-local treesit-font-lock-settings (cedar-ts-mode--font-lock-settings))
  (setq-local treesit-font-lock-feature-list
              '((comment)
                (keyword string builtin)
                (annotation constant number type function property)
                (operator bracket delimiter)))

  ;; Indentation
  (setq-local treesit-simple-indent-rules cedar-ts-mode--indent-rules)
  (setq-local cedar-ts-mode-indent-offset cedar-ts-mode-indent-offset)

  ;; Navigation
  (setq-local treesit-defun-type-regexp "policy")
  (setq-local treesit-simple-imenu-settings
              '(("Policy" "\\`policy\\'" nil cedar-ts-mode--policy-name)))
  (setq-local treesit-sentence-type-regexp
              (regexp-opt '("condition" "annotation"
                            "principal_constraint" "action_constraint"
                            "resource_constraint")))
  (setq-local treesit-sexp-type-regexp
              (regexp-opt '("binary_expression" "unary_expression"
                            "selector_expression" "has_expression"
                            "like_expression" "is_expression"
                            "if_then_else" "call_expression"
                            "contains_expression" "contains_all_expression"
                            "index_expression" "ext_fun_call"
                            "parenthesized_expression"
                            "entity" "set_literal" "record_literal")))

  ;; Comments
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "//+\\s-*")

  ;; Electric
  (setq-local electric-indent-chars
              (append "{}()[];," electric-indent-chars))

  (treesit-major-mode-setup))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cedar\\'" . cedar-ts-mode))

(provide 'cedar-ts-mode)

;;; cedar-ts-mode.el ends here
