;;; elisp-utils.el --- Utilities to develop better Emacs Lisp -*- lexical-binding: t -*-

;; Author: Pierre Glandon
;; Maintainer: Pierre Glandon
;; Version: 0.0.1

;; This file is not part of GNU Emacs

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

;;; Code:

(require 'projectile)

(setq elisp-utils//selected-function-name "yolo")

(defun elisp-utils//get-project-name ()
  "Return current project name using Projectile."
  (projectile-project-name))

(defun elisp-utils/insert-project-name (&optional at-point)
  "Insert project name under point or at AT_POINT if specified."
  (interactive)
  (let ((insert-point (or at-point (point))))
    (goto-char insert-point)
    (elisp-utils/better-insert (elisp-utils//get-project-name))))

(defun elisp-utils/better-insert (text)
  "Insert TEXT at point and automatically add a whitespace if none is present."
  (when (not (equal(char-before) ?\s))
    (insert " "))
  (insert text))

(defun elisp-utils//insert-ert-deftest-snippet (fun-name)
  "Insert ert deftest snippet at point."
  (yas-expand-snippet (format "
(ert-deftest %s/${2:test-case}()
\"Should ${3:do something when something.}\"
$0)
" fun-name))
  )

(defun elisp-utils//get-function-name ()
  "Return function name under point, nil if none."
  (save-excursion
    (beginning-of-defun)
    (forward-char)
    (let ((word-at-point (thing-at-point 'word t)))
      (if (not (equal word-at-point "defun"))
          (message "Not inside a function.")
        (forward-char 6)
        (car (last
              (s-split "/" (thing-at-point 'sexp t) t)
              ))
      )
    )
  )
)

(defun elisp-utils//get-current-file-test-file ()
  "Return current file associated test file using ert-runner conventions."
    (concat (f-parent (buffer-file-name)) "/test/" (f-base(buffer-file-name)) "-test" ".el")
  )



(provide 'elisp-utils)

;;; elisp-utils.el ends here
