(provide 'ob-async-shell)

(defun org-babel-execute:async-shell (body params)
  (let* ((processed-params (org-babel-process-params params))
	 (default-directory (cdr (assq :dir processed-params))))
    (with-current-buffer "*ob-async-shell*"
      (erase-buffer)
      (insert "=== Command ===\n")
      (insert body)
      (insert "\n=== Output ===\n")
      (shell-mode)
      (start-file-process "ob-async-shell" "*ob-async-shell*" "/bin/bash" "-c" body))))

;;;###autoload
(define-derived-mode async-shell-mode shell-script-mode "Async Shell Mode"
  "Major Mode for highlighting text in async-shell org babel blocks.")
