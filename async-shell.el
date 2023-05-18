;; -*- lexical-binding: t -*-

(require 'ansi-color)
(require 's)
(require 'dash)
(require 'transient)

(defvar-local async-shell-use-ansi t
  "Should ob async use ansi escape codes?")

(defvar-local async-shell-command nil
  "The shell command that this buffer is bound to.")

(defvar-local async-shell-name nil
  "The name of this shell buffer.")

(defvar async-shell-save-hooks '()
  "List of save hooks registered.")

(defvar-local async-shell-changes nil
  "Tracks any changes made from a transient menu.")

(defun make-insert-progn (input)
  "Converts `input' into a sequence of insert statements and cursor move commands."

  (let ((start 0)
        result)
    (while (setq end (string-match "\r" input start))
      (push `(insert ,(substring input start end)) result)
      (push '(delete-line) result)
      (setq start (match-end 0)))
    (push `(insert ,(substring input start end)) result)
    `(progn . ,(nreverse result))))

(defun async-shell-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (setq-local buffer-read-only 'nil)
      (let ((moving (= (point) (process-mark proc)))
            (ansi-str (ansi-color-apply string))
            )
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (eval (make-insert-progn ansi-str))
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc))))
      (setq-local buffer-read-only t))))

(defun propertize-key-value (key value)
  (concat (propertize key 
                      'face 'font-lock-function-call-face)
          (propertize (format "%s" value)
                      'face 'italic)))

(defun async-shell-start-process (buffer &optional override)
  (when (or override
            (not (get-buffer-process buffer))
            (y-or-n-p "Process is currently running. Kill?"))

    (when (get-buffer-process buffer)
      (kill-process (get-buffer-process buffer))
      (sit-for 1))

    (with-current-buffer buffer
      ;; prepare the buffer
      (setq-local buffer-read-only 'nil)
      (erase-buffer)
      (insert (propertize-key-value "default-directory: " default-directory) "\n")
      (insert (propertize-key-value "ansi-color: " async-shell-use-ansi) "\n")

      (insert (propertize-key-value (if (s-contains? "\n" async-shell-command)
                                        "command:\n"
                                      "command: ")
                                    async-shell-command)
              "\n\n")
      (setq-local buffer-read-only t)

      ;; start process
      (let* ((command (concat (if async-shell-use-ansi
                                  "export TERM=\"xterm-256color\"\n"
                                "")
                              async-shell-command))
             (proc (start-file-process-shell-command
                    async-shell-name buffer command)))
        (set-process-filter proc 'async-shell-filter)))))

(defun async-shell-update-buffer-fn (name)
  (lambda ()
    (with-current-buffer (get-buffer name)
      (revert-buffer))))

(defun async-shell-apply-save-hook ()
  (when async-shell-save-hooks
    (--each async-shell-save-hooks (funcall (cdr it)))))

(defun async-shell-read (&optional filter-fn)
  "Uses the `completing-read' interface to prompt the user to select and async-shell buffer"

  (read-buffer "Buffer: " nil t
               (lambda (b)
                 (and (equal (buffer-local-value 'major-mode (cdr b))
                             #'async-shell-process-mode)
                      (if filter-fn
                          (funcall filter-fn (car b))
                        t)))))

(defun async-shell-this-buffer? ()
  "Returns `t' if the current buffer is an async-shell buffer"
  (equal major-mode #'async-shell-process-mode))

(defun async-shell-here-or-prompt (&optional filter)
  "Returns an ob-async buffer either by prompting or returning the current buffer"

  (if (async-shell-this-buffer?)
      (buffer-name)
    (async-shell-read filter)))

(defun async-shell-register ()
  (interactive)

  (let ((buffer (async-shell-here-or-prompt
                 (lambda (b) (not (assoc b async-shell-save-hooks))))))
    (add-to-list 'async-shell-save-hooks `(,buffer . ,(async-shell-update-buffer-fn buffer)))))

(defun async-shell-unregister ()
  (interactive)

  (when async-shell-save-hooks
    (setq async-shell-save-hooks
          (assoc-delete-all (async-shell-here-or-prompt
                             (lambda (b)
                               (assoc b async-shell-save-hooks)))
                            async-shell-save-hooks))))

(defun async-shell-rename ()
  (interactive)

  (let* ((buf (async-shell-here-or-prompt)))
    (with-current-buffer (get-buffer buf)
      (let ((new-name
             (format "*async-shell:%s*" (read-string "New Name: "))))
        (rename-buffer new-name)
        (setq-local async-shell-name new-name)))))

(defun async-shell-change-command ()
  (interactive)

  (let* ((buf (async-shell-here-or-prompt)))
    (with-current-buffer (get-buffer buf)
      (let ((new-command (read-string "Command: ")))
        (setq-local async-shell-command new-command)
        (revert-buffer)))))

;;;###autoload
(define-derived-mode async-shell-mode shell-script-mode "Async Shell Mode"
  "Major Mode for highlighting text in async-shell org babel blocks.")

(transient-define-suffix async-shell:--register ()
  :transient t
  :key "r"
  :description (lambda ()
                 (format "Reload on save (%s)"
                         (propertize "active"
                                     'face
                                     (if (assoc (buffer-name) async-shell-save-hooks)
                                         'transient-argument
                                       'transient-inactive-argument))))
  (interactive)
  (setq async-shell-changes t)
  (if (assoc (buffer-name) async-shell-save-hooks)
      (async-shell-unregister)
    (async-shell-register)))

(transient-define-suffix async-shell:--ansi-color ()
  :transient t
  :key "a"
  :description (lambda ()
                 (format "Ansi-color (%s)"
                         (propertize "active"
                                     'face
                                     (if async-shell-use-ansi
                                         'transient-argument
                                       'transient-inactive-argument))))
  (interactive)
  (setq async-shell-changes t)
  (setq async-shell-use-ansi
        (not async-shell-use-ansi)))

(transient-define-suffix async-shell:--apply ()
  :transient nil
  :key "q"
  :description "Quit"

  (interactive)
  (when async-shell-changes
    (setq async-shell-changes nil)
    (revert-buffer)))

(transient-define-prefix async-shell-menu ()
  [["Actions"
    ("R" "Rename" async-shell-rename)
    ("c" "Command" async-shell-change-command)]
   ["Toggles"
    (async-shell:--register)
    (async-shell:--ansi-color)]
   ["Exit"
    (async-shell:--apply)]])

(defvar-keymap async-shell-process-mode-map
  "TAB" #'async-shell-menu
  "R" #'async-shell-rename)

(define-derived-mode async-shell-process-mode special-mode "Async Shell"
  "Major mode for the Async Shell process buffer."

  (buffer-disable-undo)
  (setq-local truncate-lines t)
  (add-hook 'after-save-hook #'async-shell-apply-save-hook))

(defun async-shell-launch (command &optional default-dir no-ansi-color vars name)
  (interactive "MCommand: ")
  (let* ((buffer-name (if name (format "*async-shell:%s*" name)
                        (format "*async-shell:%s*" (car (s-split-up-to " " command 1)))))
         (default-dir (if default-dir default-dir default-directory))
         (body (s-concat (if vars (format "%s\n" vars) "")
                         command))
         (shell-buf (get-buffer-create buffer-name)))

    (with-current-buffer shell-buf
      (async-shell-process-mode)
      (setq-local default-directory default-dir
                  async-shell-command body
                  async-shell-name buffer-name
                  async-shell-use-ansi (not no-ansi-color)
                  revert-buffer-function (lambda (&optional _ignore-auto _no-confirm)
                                           (async-shell-start-process shell-buf))))
    (async-shell-start-process shell-buf)
    (switch-to-buffer shell-buf)))

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

(provide 'async-shell)