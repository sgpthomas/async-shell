;; -*- lexical-binding: t -*-

(require 'ansi-color)
(require 's)
(require 'dash)

(provide 'ob-async-shell)

(defvar-local ob-async-shell-use-ansi t
  "Should ob async use ansi escape codes?")

(defvar-local ob-async-shell-command nil
  "The shell command that this buffer is bound to.")

(defvar-local ob-async-shell-name nil
  "The name of this shell buffer.")

(defvar ob-async-save-hooks '()
  "List of save hooks registered.")

(defun async-shell-launch (command &optional default-dir no-ansi-color vars name)
  (interactive "MCommand: ")
  (let* ((buffer-name (if name (format "*ob-async-shell:%s*" name)
                        (format "*ob-async-shell:%s*" (car (s-split-up-to " " command 1)))))
         (vars (if no-ansi-color
                   vars
                 (concat "export TERM=\"xterm-256color\"\n" vars)))
         (default-dir (if default-dir default-dir default-directory))
         ;; append cd to the beginning because `compilation-start' expects this
         (body (s-concat (format "cd %s\n" default-dir)
                         "#### begin body ####\n"
                         vars "\n"
                         command "\n"
                         "#### end body ####\n"))
         (comp-buf (compilation-start body 'async-shell-process-mode
                                      (lambda (_)
                                        (if ob-async-shell-name
                                            ob-async-shell-name
                                          buffer-name)))))

    ;; we have to set the variables after starting the compilation buffer
    ;; otherwise these bindings are removed.
    ;; TODO: rewrite to not use compilation buffer
    (with-current-buffer comp-buf
      ;; save the command as a buffer local variable
      (setq-local default-directory default-dir
                  ob-async-shell-command body
                  ob-async-shell-name buffer-name
                  ob-async-shell-use-ansi (not no-ansi-color)))))

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

(defun ob-async-update-buffer-fn (name)
  (lambda ()
    (with-current-buffer (get-buffer name)
      (recompile))))

(defun ob-async-apply-save-hook ()
  (when ob-async-save-hooks
    (--each ob-async-save-hooks (funcall (cdr it)))))

(defun ob-async-read (&optional filter-fn)
  "Uses the `completing-read' interface to prompt the user to select and ob-async buffer"

  (read-buffer "Buffer: " nil t
               (lambda (b)
                 (and (equal (buffer-local-value 'major-mode (cdr b))
                             #'async-shell-process-mode)
                      (if filter-fn
                          (funcall filter-fn (car b))
                        t)
                      ))))

(defun ob-async-this-buffer? ()
  "Returns `t' if the current buffer is an async-shell buffer"
  (equal major-mode #'async-shell-process-mode))

(defun ob-async-here-or-prompt (&optional filter)
  "Returns an ob-async buffer either by prompting or returning the current buffer"

  (if (ob-async-this-buffer?)
      (buffer-name)
    (ob-async-read filter)))

(defun ob-async-register ()
  (interactive)

  (let ((buffer (ob-async-here-or-prompt
                 (lambda (b) (not (assoc b ob-async-save-hooks))))))
    (add-to-list 'ob-async-save-hooks `(,buffer . ,(ob-async-update-buffer-fn buffer)))))

(defun ob-async-unregister ()
  (interactive)

  (when ob-async-save-hooks
    (setq ob-async-save-hooks
          (assoc-delete-all (ob-async-here-or-prompt
                             (lambda (b)
                               (assoc b ob-async-save-hooks)))
                            ob-async-save-hooks))))

(defun ob-async-rename ()
  (interactive)

  (let* ((buf (ob-async-here-or-prompt)))
    (with-current-buffer (get-buffer buf)
      (let ((new-name
             (format "*ob-async-shell:%s*" (read-string "New Name: "))))
        (rename-buffer new-name)
        (setq-local ob-async-shell-name new-name)))))

;;;###autoload
(define-derived-mode async-shell-mode shell-script-mode "Async Shell Mode"
  "Major Mode for highlighting text in async-shell org babel blocks."

  )

(define-derived-mode async-shell-process-mode compilation-mode "Async Shell"
  "Major mode for the Async Shell process buffer."
  (font-lock-mode -1)
  (setq-local truncate-lines t)
  (setq-local ansi-color-apply-face-function
	      (lambda (beg end face)
		(when face
		  (put-text-property beg end 'face face))))
  (add-hook 'compilation-filter-hook #'ob-async-filter)
  (add-hook 'after-save-hook #'ob-async-apply-save-hook))
