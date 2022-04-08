(provide 'ob-async-shell)

(defun org-babel-execute:async-shell (body params)
  (message "body: %s" body)
  (message "params: %s" params)
  (async-shell-command body))

;;;###autoload
(define-derived-mode async-shell-mode shell-script-mode "Async Shell Mode"
  "Major Mode for highlighting text in async-shell org babel blocks.")
