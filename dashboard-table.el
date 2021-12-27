;;; dashboard-table.el --- tabular data with section names -*- lexical-binding: t; -*-

;; Author: Thomas Hisch <t.hisch@gmail.com>
;; Maintainer: Thomas Hisch <t.hisch@gmail.com>
;; URL: https://github.com/thisch/dashboard-table.el
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (magit "2.13.1"))
;; Keywords: extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along with
;; this program; see the file LICENSE. If not, write to the write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; What's the benefit of this mode?
;; -> sections
;; -> per-section query functions

;; TODO document user interface
;; TODO do I want users/developers to create a new major mode?
;; let's assume that a single buffer is needed per major mode
;;
;; the user can specify a mapping from section-title to query-function
;; The columns

;; define macros that can be used for creating major modes

;; TODO option for displaying header line for every section
;; TODO allow using different header-columns for different sections
;; TODO use different backend for dashboard-table instead of tabulated-list-mode. Maybe eww?

;;; Code:

(require 'magit) ;; only for a single face

(defvar dashboard-table-buffer-name "*dashboard-table*" nil)
(defvar dashboard-table-query-alist
  '(("Assigned to me" . "assignee:self (-is:wip OR owner:self OR assignee:self) is:open -is:ignored")
    ("Work in progress" . "is:open owner:self is:wip")
    ("Outgoing reviews" . "is:open owner:self -is:wip -is:ignored")
    ("Incoming reviews" .  "is:open -owner:self -is:wip -is:ignored (reviewer:self OR assignee:self)")
    ("CCed On" . "is:open -is:ignored cc:self")
    ("Recently closed" . "is:closed -is:ignored (-is:wip OR owner:self) (owner:self OR reviewer:self OR assignee:self OR cc:self) limit:15"))
  "Query search string that is used for the data shown in the dashboard-table.")

(defvar dashboard-table-columns
  [("Number" 8)
   ("Subject" 55)
   ]
  "Column-names and column-sizes of the dashboard.")

(defvar dashboard-table-section-title-column 1
  "column index (zero-indexed) of the section title"
  ;; This variable exists because the built-in tabulated-list mode,
  ;; which is used as backend for this mode, doesn't have support for
  ;; displaying section titles.

  ;; TODO get rid of this variable and add support for section titles to
  ;; tabulated-list-mode.
)

(defface dashboard-table-section
  '((t (:inherit 'magit-section-heading)))
  "Used for the section names in the dashboard."
  :group 'faces)

(defun dashboard-table--get-section-data (section)
  "Return a list with \"tabulated-list-entries\"."
  (seq-map (lambda (row)
             `(nil [,(format "S%s-row%d-abc" section row), "def"]))
             '(2 4 6)))

(defun dashboard-table--section-title-row (section-title num-entries)
  (let ((row (make-vector (length dashboard-table-columns) "")))
    ;; now set the section title in the desired column
    (aset row dashboard-table-section-title-column
          (propertize
           (format "%s (%d)" section-title num-entries)
           'face 'gerrit-section))
    `((nil ,row))))

(defun dashboard-table--get-list-entries ()
  "Get the all entries used for \"tabulated-list-entries\"."
  (seq-reduce (lambda (acc conscell)
                (let ((section-data
                       (dashboard-table--get-section-data (cdr conscell))))
                  (append acc
                          (dashboard-table--section-title-row (car conscell)
                                                              (length section-data))
                          section-data)))
              dashboard-table-query-alist '()))

(defun dashboard-table--refresh ()
  "Refresh dashboard."
  (interactive)
  (setq tabulated-list-format dashboard-table-columns)
  (setq tabulated-list-entries (dashboard-table--get-list-entries))
  (tabulated-list-init-header)
  (tabulated-list-print))

(defun dashboard-table--refresh--and-point-restore ()
  "Refresh dashboard and restore current position of point."
  (interactive)
  (let ((ppos (point)))
    (dashboard-table--refresh)
    (goto-char ppos)))

(defvar dashboard-table-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'dashboard-table--refresh--and-point-restore)
    ;; <C-down> -> forward-paragraph
   map))

(define-derived-mode dashboard-table-mode tabulated-list-mode "dashboard-table"
  "dashboard-table mode"
  (use-local-map dashboard-table-mode-map)

  ;; all lines that don't start with a changenr are header-lines that are
  ;; treated as the beginning of a paragraph
  (setq-local paragraph-start "^[^0-9]")

  ;; some variables have to be made buffer-local s.t. refreshing of
  ;; dashboards works as expected.
  (setq-local dashboard-table-columns dashboard-table-columns)
  (setq-local dashboard-table-query-alist dashboard-table-query-alist)

  (dashboard-table--refresh))

;;;###autoload
(defun dashboard-table ()
  "Show a dashboard in a new buffer."
  (interactive)
  (switch-to-buffer dashboard-table-buffer-name)
  (dashboard-table-mode))

(provide 'dashboard-table)
;;; dashboard-table.el ends here
