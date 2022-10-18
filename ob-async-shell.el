(provide 'ob-async-shell)

(defun org-babel-execute:async-shell (body params)
  (let* ((processed-params (org-babel-process-params params))
	 (default-dir (cdr (assq :dir processed-params))))

    (with-current-buffer (get-buffer-create "*ob-async-shell*")
      ;; save the command as a buffer local variable
      (setq-local ob-async-shell-command body)
      (setq-local default-directory default-dir)
)

    (ob-async-shell-run body)))

(defun ob-async-shell-run (command)
  (compilation-start command t (lambda (_) "*ob-async-shell*")))

(defun ob-async-filter ()
  (message "Hi"))

;;;###autoload
(define-derived-mode async-shell-mode shell-script-mode "Async Shell Mode"
  "Major Mode for highlighting text in async-shell org babel blocks.")

(define-derived-mode async-shell-process-mode compilation-mode "Async Shell"
  "Major mode for the Async Shell process buffer."
  (setq-local truncate-lines t)
  ;; add compilation filter
  (add-hook 'compilation-filter-hook
	    #'ob-async-filter))
