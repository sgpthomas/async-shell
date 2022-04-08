(provide 'ob-async-shell)

(defun org-babel-execute:async-shell (body params)
  (message "body: %s" body)
  (message "params: %s" params)
  (async-shell-command body))
