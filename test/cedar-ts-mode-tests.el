;;; cedar-ts-mode-tests.el --- Tests for cedar-ts-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Robert Voyer

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; ERT tests for cedar-ts-mode.  Requires the `cedar' tree-sitter grammar
;; to be installed.  Run with:
;;
;;   emacs -batch -l ert -l cedar-ts-mode.el -l test/cedar-ts-mode-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'treesit)
(require 'cedar-ts-mode)

(defvar cedar-ts-mode-test--sample-dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defvar cedar-ts-mode-test--sample-file
  (expand-file-name "sample.cedar" cedar-ts-mode-test--sample-dir))

(defmacro cedar-ts-mode-test--with-buffer (content &rest body)
  "Create a temporary buffer with CONTENT in `cedar-ts-mode', then run BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (cedar-ts-mode)
     (font-lock-ensure)
     ,@body))

;;; Grammar availability

(ert-deftest cedar-ts-mode-test-grammar-available ()
  "The cedar tree-sitter grammar should be available."
  (skip-unless (treesit-available-p))
  (should (treesit-ready-p 'cedar)))

;;; Mode activation

(ert-deftest cedar-ts-mode-test-auto-mode ()
  "Files ending in .cedar should activate cedar-ts-mode."
  (skip-unless (treesit-ready-p 'cedar))
  (let ((buf (find-file-noselect cedar-ts-mode-test--sample-file)))
    (unwind-protect
        (with-current-buffer buf
          (should (eq major-mode 'cedar-ts-mode)))
      (kill-buffer buf))))

(ert-deftest cedar-ts-mode-test-mode-name ()
  "The mode line should show \"Cedar\"."
  (skip-unless (treesit-ready-p 'cedar))
  (cedar-ts-mode-test--with-buffer "permit (principal, action, resource);"
    (should (string= mode-name "Cedar"))))

;;; Comment settings

(ert-deftest cedar-ts-mode-test-comment-settings ()
  "Comment variables should be set correctly."
  (skip-unless (treesit-ready-p 'cedar))
  (cedar-ts-mode-test--with-buffer ""
    (should (string= comment-start "// "))
    (should (string= comment-end ""))
    (should (string= comment-start-skip "//+\\s-*"))))

;;; Font-lock

(ert-deftest cedar-ts-mode-test-fontify-keyword-permit ()
  "The `permit' keyword should be fontified."
  (skip-unless (treesit-ready-p 'cedar))
  (cedar-ts-mode-test--with-buffer
      "permit (principal, action, resource);"
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face)
                'font-lock-keyword-face))))

(ert-deftest cedar-ts-mode-test-fontify-keyword-forbid ()
  "The `forbid' keyword should be fontified."
  (skip-unless (treesit-ready-p 'cedar))
  (cedar-ts-mode-test--with-buffer
      "forbid (principal, action, resource);"
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face)
                'font-lock-keyword-face))))

(ert-deftest cedar-ts-mode-test-fontify-comment ()
  "Comments should be fontified."
  (skip-unless (treesit-ready-p 'cedar))
  (cedar-ts-mode-test--with-buffer
      "// this is a comment\npermit (principal, action, resource);"
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face)
                'font-lock-comment-face))))

(ert-deftest cedar-ts-mode-test-fontify-string ()
  "Strings should be fontified."
  (skip-unless (treesit-ready-p 'cedar))
  (cedar-ts-mode-test--with-buffer
      "permit (principal == User::\"alice\", action, resource);"
    (goto-char (point-min))
    (search-forward "\"alice\"")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'font-lock-string-face))))

(ert-deftest cedar-ts-mode-test-fontify-builtin ()
  "Builtin variables (principal, action, resource, context) should be fontified."
  (skip-unless (treesit-ready-p 'cedar))
  (cedar-ts-mode-test--with-buffer
      "permit (principal, action, resource) when { context.mfa };"
    (goto-char (point-min))
    (search-forward "principal")
    ;; Check the face at the start of "principal" in scope
    (let ((face (get-text-property (match-beginning 0) 'face)))
      (should (eq face 'font-lock-builtin-face)))))

(ert-deftest cedar-ts-mode-test-fontify-annotation ()
  "Annotations should be fontified."
  (skip-unless (treesit-ready-p 'cedar))
  (cedar-ts-mode-test--with-buffer
      "@id(\"test\")\npermit (principal, action, resource);"
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face)
                'font-lock-preprocessor-face))))

(ert-deftest cedar-ts-mode-test-fontify-constant ()
  "Boolean constants should be fontified."
  (skip-unless (treesit-ready-p 'cedar))
  (cedar-ts-mode-test--with-buffer
      "permit (principal, action, resource) when { true };"
    (goto-char (point-min))
    (search-forward "true")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'font-lock-constant-face))))

;;; Indentation

(ert-deftest cedar-ts-mode-test-indent-policy-body ()
  "Content inside a policy should be indented."
  (skip-unless (treesit-ready-p 'cedar))
  (cedar-ts-mode-test--with-buffer
      "permit (\nprincipal,\naction,\nresource\n);"
    (indent-region (point-min) (point-max))
    (goto-char (point-min))
    (forward-line 1)
    (should (= (current-indentation) cedar-ts-mode-indent-offset))))

(ert-deftest cedar-ts-mode-test-indent-closing-paren ()
  "Closing parenthesis should align with the opening line."
  (skip-unless (treesit-ready-p 'cedar))
  (cedar-ts-mode-test--with-buffer
      "permit (\n  principal,\n  action,\n  resource\n);"
    (indent-region (point-min) (point-max))
    (goto-char (point-min))
    (search-forward ");")
    (beginning-of-line)
    (should (= (current-indentation) 0))))

(ert-deftest cedar-ts-mode-test-indent-condition ()
  "Content inside a condition should be indented."
  (skip-unless (treesit-ready-p 'cedar))
  (cedar-ts-mode-test--with-buffer
      "permit (principal, action, resource)\nwhen {\ncontext.ok\n};"
    (indent-region (point-min) (point-max))
    (goto-char (point-min))
    (search-forward "context")
    (beginning-of-line)
    (should (= (current-indentation) cedar-ts-mode-indent-offset))))

;;; Navigation

(ert-deftest cedar-ts-mode-test-defun-navigation ()
  "Should navigate between policies with beginning/end-of-defun."
  (skip-unless (treesit-ready-p 'cedar))
  (cedar-ts-mode-test--with-buffer
      "permit (principal, action, resource);\nforbid (principal, action, resource);"
    (goto-char (point-max))
    (beginning-of-defun)
    (should (looking-at "forbid"))
    (beginning-of-defun)
    (should (looking-at "permit"))))

;;; Imenu

(ert-deftest cedar-ts-mode-test-imenu-with-id ()
  "Imenu should use @id annotation value when present."
  (skip-unless (treesit-ready-p 'cedar))
  (cedar-ts-mode-test--with-buffer
      "@id(\"my-policy\")\npermit (principal, action, resource);"
    (let ((index (treesit-simple-imenu)))
      (should index)
      (let* ((policy-group (assoc "Policy" index))
             (entries (cdr policy-group)))
        (should policy-group)
        (should (cl-some (lambda (entry)
                           (string-match "permit.*my-policy" (car entry)))
                         entries))))))

(ert-deftest cedar-ts-mode-test-imenu-without-id ()
  "Imenu should fall back to effect + line number when no @id."
  (skip-unless (treesit-ready-p 'cedar))
  (cedar-ts-mode-test--with-buffer
      "forbid (principal, action, resource);"
    (let ((index (treesit-simple-imenu)))
      (should index)
      (let* ((policy-group (assoc "Policy" index))
             (entries (cdr policy-group)))
        (should policy-group)
        (should (cl-some (lambda (entry)
                           (string-match "forbid.*line" (car entry)))
                         entries))))))

(provide 'cedar-ts-mode-tests)

;;; cedar-ts-mode-tests.el ends here
