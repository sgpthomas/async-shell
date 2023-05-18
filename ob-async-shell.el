;;; ob-async-shell.el -*- lexical-binding: t -*-

;; Copyright (C) 2023 Samuel Thomas

;;; External packages:
(require 's)

;;; Code:
(require 'async-shell)

(defun org-babel-execute:async-shell (body params)
  (let* ((processed-params (org-babel-process-params params))
	 (default-dir (cdr (assq :dir processed-params)))
	 (use-ansi-color (cdr (assq :ansi processed-params)))
	 (switch-buf (cdr (assq :switch processed-params)))
         (dont-switch (if (null switch-buf) t
                        (s-equals? switch-buf "nil")))
         (no-ansi-color (if (null use-ansi-color) 'nil
                          (s-equals? use-ansi-color "nil")))
         (vars (--> processed-params
                    (--filter (equal (car it) :var) it)
                    (--map (format "%s=\"%s\"" (cadr it) (cddr it)) it)
                    (s-join "\n" it)))
         (name (cdr (assq :name processed-params)))
         (name (if name name "none")))

    (async-shell-launch body default-dir
                        no-ansi-color vars name
                        dont-switch)))

(provide 'ob-async-shell)

;;; ob-async-shell.el ends here
