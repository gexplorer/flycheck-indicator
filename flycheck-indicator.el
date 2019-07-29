;;; flycheck-indicator.el --- A fancy mode line indicator for `flycheck-mode'

;; Author: Eder Elorriaga <gexplorer8@gmail.com>
;; Version: 1.0
;; Keywords: flycheck, mode-line

;; This program is free software; you can redistribute it and/or modify
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

;; Colorized mode line indicator with icons to display the `flycheck-mode' status.
;;
;; This package provides the `flycheck-indicator-mode' minor mode which
;; displays a colorized mode line with icons for `flycheck-mode' status.
;;
;; To enable this mode in Flycheck, add it to `flycheck-mode-hook':
;;
;; (add-hook 'flycheck-mode-hook 'flycheck-indicator-mode)

;;; Code:

(require 'flycheck)

;;; Customization

(defgroup flycheck-indicator nil
  "A fancy mode line indicator for `flycheck-mode'."
  :prefix "flycheck-indicator-"
  :group 'flycheck)

(defgroup flycheck-indicator-faces nil
  "Faces used by `flycheck-indicator-mode'."
  :prefix "flycheck-indicator-"
  :group 'flycheck-indicator)

(defface flycheck-indicator-disabled '((t :inherit font-lock-comment-face))
  "Disabled indicator face."
  :group 'flycheck-indicator-faces)

(defface flycheck-indicator-running '((t :inherit font-lock-keyword-face))
  "Running indicator face."
  :group 'flycheck-indicator-faces)

(defface flycheck-indicator-success'((t :inherit font-lock-builtin-face))
  "Success indicator face."
  :group 'flycheck-indicator-faces)

(defface flycheck-indicator-error '((t :inherit flycheck-error-list-error))
  "Error indicator face."
  :group 'flycheck-indicator-faces)

(defface flycheck-indicator-warning '((t :inherit flycheck-error-list-warning))
  "Warning indicator face."
  :group 'flycheck-indicator-faces)

(defface flycheck-indicator-info '((t :inherit flycheck-error-list-checker-name))
  "Info indicator face."
  :group 'flycheck-indicator-faces)

(defcustom flycheck-indicator-icon-error ?·
  "The character used for errors indicator."
  :group 'flycheck-indicator
  :type 'character)

(defcustom flycheck-indicator-icon-warning ?·
  "The character used for warnings indicator."
  :group 'flycheck-indicator
  :type 'character)

(defcustom flycheck-indicator-icon-info ?·
  "The character used for info indicator."
  :group 'flycheck-indicator
  :type 'character)

(defvar flycheck-indicator-status-faces
  '((not-checked . flycheck-indicator-disabled)
    (no-checker . flycheck-indicator-disabled)
    (running . flycheck-indicator-running)
    (finished . flycheck-indicator-success)
    (errored . flycheck-indicator-error)
    (interrupted . flycheck-indicator-error)
    (suspicious . flycheck-indicator-error)))

(defvar flycheck-indicator-old-mode-line nil
  "The former value of `flycheck-mode-line'.")

(defvar flycheck-indicator-mode-line
  '(:eval
    (let-alist (flycheck-count-errors flycheck-current-errors)
      (let* ((status flycheck-last-status-change)
             (info (or .info 0))
             (warnings (or .warning 0))
             (errors (or .error 0)))
        (flycheck-indicator--formatter info warnings errors status))))
  "The fancy value of `flycheck-mode-line'.")

;;; Utility functions
(defun flycheck-indicator--status-formatter (status)
  "Get a colorized text for STATUS."
  (let ((status-icon (symbol-name status))
        (status-face (alist-get status flycheck-indicator-status-faces)))
    (list
     " ["
     (propertize (format "%s" status-icon)
                 'font-lock-face status-face)
     "]")))

(defun flycheck-indicator--errors-formatter (info warnings errors)
  "Get a colorized text for INFO, WARNINGS and ERRORS."
  `(
    " ["
    ,@(if (> info 0)
          (list (propertize (format "%c%s" flycheck-indicator-icon-info info)
                            'font-lock-face 'flycheck-indicator-info)))
    ,@(if (> warnings 0)
          (list
           (if (> info 0) " " "")
           (propertize (format "%c%s" flycheck-indicator-icon-warning warnings)
                       'font-lock-face 'flycheck-indicator-warning)))
    ,@(if (> errors 0)
          (list
           (if (> (+ info warnings) 0) " " "")
           (propertize (format "%c%s" flycheck-indicator-icon-error errors)
                       'font-lock-face 'flycheck-indicator-error)))
    "]"))

(defun flycheck-indicator--formatter (info warnings errors status)
  "Get a colorized text for STATUS with INFO WARNINGS and ERRORS."
  (if (or (not (equal status 'finished))
          (= 0 (+ info warnings errors)))
      (flycheck-indicator--status-formatter status)
    (flycheck-indicator--errors-formatter info warnings errors)
    ))

;;;###autoload
(define-minor-mode flycheck-indicator-mode
  "Minor mode to get a fancy mode line indicator for `flycheck-mode'.

When called interactively, toggle
`flycheck-indicator-mode'.  With prefix ARG, enable
`flycheck-indicator-mode' if ARG is positive, otherwise
disable it.

When called from Lisp, enable `flycheck-indicator-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `flycheck-indicator-mode'.
Otherwise behave as if called interactively."
  :init-value nil
  :keymap nil
  :lighter nil
  :group 'flycheck-indicator
  :global t
  (cond
   ((and flycheck-indicator-mode
         (not (eq flycheck-mode-line flycheck-indicator-mode-line)))
    (setq flycheck-indicator-old-mode-line flycheck-mode-line)
    (setq flycheck-mode-line flycheck-indicator-mode-line))
   ((and (not flycheck-indicator-mode)
         (eq flycheck-mode-line flycheck-indicator-mode-line))
    (setq flycheck-mode-line flycheck-indicator-old-mode-line)
    (setq flycheck-indicator-old-mode-line nil))))

(provide 'flycheck-indicator-mode)
;;; flycheck-indicator.el ends here
