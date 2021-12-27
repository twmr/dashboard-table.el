(require 'dashboard-table)

(define-derived-mode example-table-mode dashboard-table-mode "example-table"
  "example-table mode"
  )

;; TODO this needs to be buffer local
(defun dashboard-table--get-section-data (section)
  "Return a list with \"tabulated-list-entries\"."
  (seq-map (lambda (row)
             `(nil [,(format "S%s-row%d-abc" section row), "def" "DD"]))
             '(2 4 6 9 10)))

;;;###autoload
(defun example-table ()
  "Show a dashboard in a new buffer."
  (interactive)
  (switch-to-buffer "example-table")
  (setq dashboard-table-query-alist
        '(("Section1" . "pars for section1")
          ("Section2" . "pars for section2")))

  (setq dashboard-table-columns
        [("Key" 20)
         ("Name" 55)
         ("Data1" 10)
         ])

  (example-table-mode))
