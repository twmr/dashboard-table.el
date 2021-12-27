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
  ;; here the major-mode is already `example-table-mode`

  (setq dashboard-table-section-alist
        '(("Section1" . "pars for section1")
          ("Section2" . "pars for section2")))
  (setq dashboard-table-columns
        [("Key" 40)
         ("Name" 55)
         ("Data1" 10)
         ])
  (setq dashboard-table-section-name-column 0)
  (setq dashboard-table-get-section-data-function
        #'example-table-get-section-data)
  (dashboard-table--refresh))

;;;###autoload
(defun example-table ()
  "Show a dashboard in a new buffer."
  (interactive)
  (switch-to-buffer "example-table")
  (example-table-mode))
