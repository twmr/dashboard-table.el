[![Build Status](https://github.com/thisch/dashboard-table.el/workflows/CI/badge.svg)](https://github.com/thisch/dashboard-table.el/actions)

dashboard-table.el
==================

This emacs package is a tiny wrapper around `tabulated-list-mode`, which
allows adding multiple tables into a tabulated list buffer. All the tables
need to have the same columns (It is planned to get rid of this limitation
in the future).

## Installation

This package is not yet available via MELPA.

## Usage

Similar to the usage of `tabulated-list-mode`. A package that wants to use
`dashboard-table.el` needs to create a derived major mode (See documentation
of `tabulated-list-mode`).

Here is a small example that shows how the package can be used.

```el
(require 'dashboard-table)

(defun example-table-get-section-data (section)
  "Return a list with \"tabulated-list-entries\"."
  (seq-map (lambda (row)
             `(nil [,(format "%s-row%d-abc" section row)
                    "def"
                    "DD"]))
             '(2 4 6 9 10)))

(define-derived-mode example-table-mode dashboard-table-mode "example-table"
  "example-table mode"
  ;; you can use a local keymap here
  )

(defun example-table ()
  "Show a dashboard in a new buffer."
  (interactive)
  (switch-to-buffer "example-table")
  (example-table-mode)
  (setq dashboard-table-section-alist
        '(("Section1" . "pars for section1")
          ("Section2" . "pars for section2")))
  (setq dashboard-table-columns
        [("Key" 40)
         ("Name" 55)
         ("Status" 10)
         ])
  (setq dashboard-table-section-name-column 0)
  (setq dashboard-table-get-section-data-function
        #'example-table-get-section-data)
  (dashboard-table--refresh))
```

`dashboard-table.el` defines the following keybindings

* <kbd>g</kbd> Refresh the buffer and restore position of point.
