;;; dashboard-table.el --- Tabular data with section names -*- lexical-binding: t; -*-

;; Author: Thomas Hisch <t.hisch@gmail.com>
;; Maintainer: Thomas Hisch <t.hisch@gmail.com>
;; URL: https://github.com/thisch/dashboard-table.el
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
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

;; define macros that can be used for creating dashboards

;; TODO option for displaying header line for every section
;; TODO allow using different header-columns for different sections (
;;      very complicated using tabulated-list-mode)
;; TODO use different backend for dashboard-table instead of tabulated-list-mode.
;;       Maybe eww or xwidget-webkit-browse-url?
;; TODO support multiline rows (if the value doesn't fit within one row)

;;; Code:

(require 'outline) ;; only for a single face

(defvar-local dashboard-table-buffer-name "*dashboard-table*" nil)
(defvar-local dashboard-table-section-alist nil
  "Mapping from section names to user-defined section data.")

(defvar-local dashboard-table-columns nil
  "Column-names and column-sizes of the dashboard.
See the docstring of `tabulated-list-format'.")

(defface dashboard-table-section
  '((t (:inherit 'outline-1)))
  "Used for the section names in the dashboard."
  :group 'faces)

(defvar-local dashboard-table-get-section-data-function (lambda ())
  "Return a list with \"tabulated-list-entries\".")

(defun dashboard-table--section-name-row (section-name num-entries)
  `((section-row
     ,(propertize
       (format "%s (%d)" section-name num-entries)
       'face 'dashboard-table-section))))

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

(defun dashboard-table-print-entry (id cols)
  (if (eq id 'section-row)
      ;; cols is a section name (a string) for section rows
      (insert (concat cols "\n"))
    (tabulated-list-print-entry id cols)))

(defun dashboard-table--refresh ()
  "Refresh dashboard."
  (interactive)
  (setq tabulated-list-format dashboard-table-columns)
  (setq tabulated-list-entries (dashboard-table--get-list-entries))
  (setq tabulated-list-printer #'dashboard-table-print-entry)
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

  ;; TODO I know that it is possible to set paragraph-start also to
  ;; functions; do that
  ;; see https://sachachua.com/blog/2022/01/defining-generic-and-mode-specific-emacs-lisp-functions-with-cl-defmethod/
  ;; (setq-local paragraph-start "^[^0-9]")
  )

(provide 'dashboard-table)
;;; dashboard-table.el ends here
