(provide 'ob-async-shell)

(defun org-babel-execute:async-shell (body params)
  (let* ((processed-params (org-babel-process-params params))
	 (default-directory (cdr (assq :dir processed-params))))
    (with-current-buffer (get-buffer-create "*ob-async-shell*")
      (special-mode)

      ;; save the command as a buffer local variable
      (setq-local ob-async-shell-command body)

      ;; set the revert buffer function
      (setq-local revert-buffer-function
		  'ob-async-shell-rerun))

    (ob-async-shell-run body)))

(defun ob-async-shell-run (command)
  (with-current-buffer "*ob-async-shell*"
    (ob-async-shell-cleanup-process)

    (setq-local buffer-read-only 'nil)

    ;; clear buffer and then insert command body
    (erase-buffer)
    ;; (shell-mode)
    (insert "=== Command ===\n")
    (insert command)

    ;; run the command using `start-file-process'
    ;; so that it respects `default-directory'.
    (insert "\n=== Output ===\n")
    (start-file-process "ob-async-shell" "*ob-async-shell*" "/bin/bash" "-c" command)

    (setq-local buffer-read-only t)))

(defun ob-async-shell-cleanup-process ()
  (let* ((process (get-buffer-process "*ob-async-shell*"))
	 (alive? (process-live-p process)))
    (when alive?
      (if (y-or-n-p "There is a process alive. Really kill? ")
	  (kill-process process)
	(error "Can't run this command. A process is still alive.")))))

(defun ob-async-shell-rerun (&optional ignore-auto no-confirm)
  (with-current-buffer "*ob-async-shell*"

    (ob-async-shell-cleanup-process)

    ;; only do thing when the shell command is defined
    (when ob-async-shell-command
      (ob-async-shell-run ob-async-shell-command))))

;;;###autoload
(define-derived-mode async-shell-mode shell-script-mode "Async Shell Mode"
  "Major Mode for highlighting text in async-shell org babel blocks.")
