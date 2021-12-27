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
;; -> per-section getter functions

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
;; TODO support multiline rows (if the value doesn't fit within one row)

;;; Code:

(require 'magit) ;; only for a single face

(defvar-local dashboard-table-buffer-name "*dashboard-table*" nil)
(defvar-local dashboard-table-section-alist nil
  "Mapping from section names to user-defined section data.")

(defvar-local dashboard-table-columns nil
  "Column-names and column-sizes of the dashboard.
See the docstring of `tabulated-list-format'.")

(defvar-local dashboard-table-section-name-column 0
  "column index (zero-indexed) of the section title."
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

(defvar-local dashboard-table-get-section-data-function (lambda ())
  "Return a list with \"tabulated-list-entries\".")

(defun dashboard-table--section-name-row (section-name num-entries)
  (let ((row (make-vector (length dashboard-table-columns) "")))
    ;; now set the section title/name in the desired column
    (aset row dashboard-table-section-name-column
          (propertize
           (format "%s (%d)" section-name num-entries)
           'face 'dashboard-table-section))
    `((nil ,row))))

(defun dashboard-table--get-list-entries ()
  "Get the all entries used for \"tabulated-list-entries\"."
  (seq-reduce (lambda (acc conscell)
                (let ((section-data
                       (funcall dashboard-table-get-section-data-function (cdr conscell))))
                  (append acc
                          (dashboard-table--section-name-row (car conscell)
                                                              (length section-data))
                          section-data)))
              dashboard-table-section-alist '()))

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
  (use-local-map dashboard-table-mode-map)  ;; all lines that don't start with a changenr are header-lines that are
  ;; treated as the beginning of a paragraph
  (setq-local paragraph-start "^[^0-9]"))

(provide 'dashboard-table)
;;; dashboard-table.el ends here
