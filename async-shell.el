;;; async-shell.el --- A tool for managing asynchronous shell commands. -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Samuel Thomas

;; Author: Samuel Thomas <sgt@cs.utexas.edu>
;; Package-Requires: (dash s transient)

;;; External packages:
(require 'ansi-color)
(require 'dash)
(require 's)
(require 'transient)

;;; Code:
(defvar-local async-shell-use-ansi t
  "Should ob async use ansi escape codes?")

(defvar-local async-shell-command nil
  "The shell command that this buffer is bound to.")

(defvar-local async-shell-name nil
  "The name of this shell buffer.")

(defvar async-shell-save-hooks '()
  "List of save hooks registered.")

(defvar-local async-shell-pin-lineno nil
  "Keeps track of a line number that can optionally be pinned so that when output is
updated, it maintains this location.")

(defface async-shell-line-run
  '((t :inherit warning))
  "Face for Async-shell mode's \"running\" mode line indicator.")

(defface async-shell-line-success
  '((t :inherit success))
  "Face for Async-shell mode's \"exit\" mode line indicator.")

(defface async-shell-line-fail
  '((t :inherit error))
  "Face for Async-shell mode's \"error\" mode line indicator.")

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

(defun async-shell-sync-window-point ()
  "the window point can be separate from the buffer point.
update that point to the buffer point"

  (--each (get-buffer-window-list (current-buffer) nil t)
    (set-window-point it (point))
    (when async-shell-pin-lineno
      (set-window-start it (point)))))

(defun async-shell-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((ansi-str (ansi-color-apply string))
            (proc-point-line (line-number-at-pos (process-mark proc)))
            (inhibit-read-only t))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (eval (make-insert-progn ansi-str))
          (set-marker (process-mark proc) (point)))

        ;; update the buffer point
        (if (and async-shell-pin-lineno (<= async-shell-pin-lineno proc-point-line))
            (goto-line async-shell-pin-lineno)
          (goto-char (process-mark proc)))

        (async-shell-sync-window-point)))))

(defun async-shell-sentinel (proc event)
  (with-current-buffer (process-buffer proc)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-max))
        (insert "\n")
        (insert (propertize (format "Process %s %s" (process-name proc) event)
                            'face 'font-lock-function-call-face)))

      (when (null async-shell-pin-lineno)
        (goto-char (point-max)))

      (async-shell-sync-window-point))

    ;; when the process is dead
    (let ((alive-p (process-live-p proc))
          (status (process-exit-status proc)))
      (when (not alive-p)
        (setq mode-line-process
              `((:propertize ,(format ":exit [%s] " status)
                             face ,(if (> status 0)
                                       'async-shell-line-fail
                                     'async-shell-line-success))))
        (force-mode-line-update)))))

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

      ;; add message to the modeline
      (setq mode-line-process
            '((:propertize ":running " face async-shell-line-run)))
      (force-mode-line-update)

      ;; start process
      (let* ((command (concat (if async-shell-use-ansi
                                  "export TERM=\"xterm-256color\"\n"
                                "")
                              async-shell-command))
             (proc (start-file-process-shell-command
                    async-shell-name buffer command)))
        (set-process-filter proc 'async-shell-filter)
        (set-process-sentinel proc 'async-shell-sentinel)))))

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
      (let ((new-command (read-string "Command: " async-shell-command)))
        (setq-local async-shell-command new-command)
        (revert-buffer)))))

(defun async-shell-kill ()
  (interactive)

  (let* ((buf (async-shell-here-or-prompt))
         (proc (get-buffer-process buf)))
    (kill-process proc)))

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
  (setq async-shell-use-ansi
        (not async-shell-use-ansi)))

(transient-define-suffix async-shell:--pin-lineno ()
  :transient t
  :key "p"
  :description (lambda ()
                 (format "Pin Line Number (%s)"
                         (propertize (format "active: %s" async-shell-pin-lineno)
                                     'face
                                     (if async-shell-pin-lineno
                                         'transient-argument
                                       'transient-inactive-argument))))
  (interactive)
  (setq async-shell-pin-lineno
        (if async-shell-pin-lineno
            nil
          (line-number-at-pos))))

(transient-define-prefix async-shell-menu ()
  [["Actions"
    ("R" "Rename" async-shell-rename)
    ("c" "Command" async-shell-change-command)
    ("k" "Kill" async-shell-kill
     :if (lambda () (process-live-p (get-buffer-process (current-buffer)))))]
   ["Toggles"
    (async-shell:--register)
    (async-shell:--ansi-color)
    (async-shell:--pin-lineno)
    ]
   ["Exit"
    ("RET" "Quit" transient-quit-one)
    ("q" "Quit" transient-quit-one)]])

(defvar-keymap async-shell-process-mode-map
  :parent special-mode-map
  "," #'async-shell-menu
  "R" #'async-shell-rename)

(define-derived-mode async-shell-process-mode special-mode "Async Shell"
  "Major mode for the Async Shell process buffer."

  (buffer-disable-undo)
  (setq-local truncate-lines t)
  (add-hook 'after-save-hook #'async-shell-apply-save-hook)
  (when (fboundp 'evil-make-overriding-map)
    (evil-make-overriding-map async-shell-process-mode-map 'normal)))

(defun async-shell-launch (command &optional default-dir no-ansi-color vars name dont-show-buffer)
  (interactive "MCommand: ")
  (let* ((buffer-name (if name (format "*async-shell:%s*" name)
                        (format "*async-shell:%s*" (car (s-split-up-to " " command 1)))))
         (default-dir (if default-dir default-dir default-directory))
         (body (s-concat (if (and vars (not (s-equals? vars ""))) (format "%s\n" vars) "")
                         command))
         ;; record if this is a new buffer
         (new (not (get-buffer buffer-name)))
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
    (when (or new (not dont-show-buffer))
      (switch-to-buffer-other-window shell-buf))))

(provide 'async-shell)

;;; async-shell.el ends here
