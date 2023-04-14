;; -*- lexical-binding: t -*-

(require 'ansi-color)
(require 's)
(require 'dash)

(provide 'ob-async-shell)

(defvar ob-async-shell-use-ansi t
  "Should ob async use ansi escape codes?")

(defun async-shell-launch (command &optional default-dir no-ansi-color vars name)
  (interactive "MCommand: ")
  (let* ((buffer-name (if name (format "*ob-async-shell:%s*" name)
                        (format "*ob-async-shell:%s*" (car (s-split-up-to " " command 1)))))
         (vars (if no-ansi-color
                   vars
                 (concat "export TERM=\"xterm-256color\"\n" vars)))
         (body (s-concat "#### begin body ####\n"
                         vars "\n"
                         command "\n"
                         "#### end body ####\n"))

         )
    (with-current-buffer (get-buffer-create buffer-name)
      ;; save the command as a buffer local variable
      (setq-local ob-async-shell-command body)
      (setq-local ob-async-shell-name name)
      (setq-local default-directory default-dir)
      (setq-local ob-async-shell-use-ansi (not no-ansi-color)))

    (defun ob-async-shell-run (command)
      (compilation-start command
                         'async-shell-process-mode
                         (lambda (_) buffer-name)))

    (ob-async-shell-run body)))

(defun org-babel-execute:async-shell (body params)
  (let* ((processed-params (org-babel-process-params params))
	 (default-dir (cdr (assq :dir processed-params)))
	 (use-ansi-color (cdr (assq :ansi processed-params)))
         (no-ansi-color (if (null use-ansi-color) 'nil
                          (s-equals? use-ansi-color "nil")))
         (vars (--> processed-params
                    (--filter (equal (car it) :var) it)
                    (--map (format "%s=\"%s\"" (cadr it) (cddr it)) it)
                    (s-join "\n" it)))
         (name (cdr (assq :name processed-params)))
         (name (if name name "none")))

    (async-shell-launch body default-dir no-ansi-color vars name)))

(defun ob-async-filter ()
  (when ob-async-shell-use-ansi
    (ansi-color-apply-on-region compilation-filter-start (point))))

;;;###autoload
(define-derived-mode async-shell-mode shell-script-mode "Async Shell Mode"
  "Major Mode for highlighting text in async-shell org babel blocks.")

(define-derived-mode async-shell-process-mode compilation-mode "Async Shell"
  "Major mode for the Async Shell process buffer."
  (font-lock-mode -1)
  (setq-local truncate-lines t)
  (setq-local ansi-color-apply-face-function
	      (lambda (beg end face)
		(when face
		  (put-text-property beg end 'face face))))
  (add-hook 'compilation-filter-hook #'ob-async-filter))
