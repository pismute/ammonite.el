;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with <open> and enter text in its buffer.

(require 'term)

(defconst ammonite--program-name "Ammonite")
(defconst ammonite--command "amm")
(defconst ammonite--predef-sc-filename "predef.sc")
(defconst ammonite--home (file-name-as-directory (getenv "HOME")))
(defconst ammonite--debug nil)

(defvar ammonite-prompt "@")
(defvar ammonite-predef-sc nil)
(defvar ammonite-process-filters '())

(defvar ammonite--buffer-name nil)

(defun ammonite--find-predef-sc (path predef-sc)
  (cond
   ((or (null path) (not (string-prefix-p ammonite--home path)))
    nil)
   ((file-exists-p (concat path predef-sc))
    (concat path predef-sc))
   (t
    (ammonite--find-predef-sc (file-name-directory (directory-file-name path)) predef-sc))))

(defun ammonite--predef-option (path)
  (let* ((candidates (list ammonite-predef-sc
                          (ammonite--find-predef-sc path ammonite--predef-sc-filename)))
         (predef-sc (seq-find 'stringp candidates)))
    (if predef-sc (list "--predef" predef-sc) nil)))

(defun ammonite-get-buffer-name (&optional session-name)
  (if session-name
    (concat "*" ammonite--program-name "-" session-name "*")
    (concat "*" ammonite--program-name "*")))

(defun ammonite--broadcast-to-process-filters (proc out)
  (with-current-buffer (process-buffer proc)
    (dolist (el ammonite-process-filters)
      (funcall el proc out))
    (term-emulate-terminal proc out)))

(defun ammonite-session-running-p (session)
  (get-buffer-process (get-buffer (ammonite-get-buffer-name session))))

(defun ammonite-assert-session-running (session)
  (unless (ammonite-session-running-p session)
    (message "`%s' is not running" (ammonite-get-buffer-name session))))

(defun ammonite--message-filter-debug (proc out)
  (when ammonite--debug
    (message "ammonite--message-filter-debug=%s" out)))

(defun ammonite (&optional session stay)
  "Start a ammonite in a new buffer."
  (interactive)

  (let* ((buffer-name (ammonite-get-buffer-name session))
         (switches (ammonite--predef-option (file-name-directory buffer-file-name)))
         (args (seq-concatenate 'list (list buffer-name ammonite--command nil) switches))
         (proc (get-buffer-process buffer-name)))
    (unless proc

      (message "Running ammonite %s" args)
      (setq buffer-name (apply 'term-ansi-make-term args))
      (setq proc (get-buffer-process buffer-name))

      (set-buffer buffer-name)
      (ammonite-mode)
      (term-char-mode)

      ;; See `ansi-term' in `term.el'
      (let (term-escape-char)
        (term-set-escape-char ?\C-x))

      (set-process-filter proc 'ammonite--broadcast-to-process-filters))

    (if stay buffer-name (switch-to-buffer buffer-name))))

(define-derived-mode ammonite-mode term-mode "Ammonite"
  "Major mode for `Ammonite'.

\\<ammonite-mode-map>"

  (set (make-local-variable 'ammonite-process-filters) '())

  (add-to-list 'ammonite-process-filters 'ammonite--message-filter-debug)

  )


(provide 'ammonite)
