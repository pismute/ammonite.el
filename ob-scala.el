;;; ob-scala.el --- org-babel functions for scala evaluation

;; Copyright (C) Changwoo Park

;; Author: Changwoo Park
;; Keywords: literate programming, scala
;; Homepage: http://orgmode.org
;; Version: 0.0.1

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


(require 'ob)
(require 'ammonite)
(require 'term)

(defconst ob-scala--debug nil)

;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("scala" . "scala"))

(defvar ob-scala-barrier "================================================")
(defun ob-scala-barrier-p (x)
  (string-equal ob-scala-barrier x))

(defvar ob-scala--ammonite-process nil)
(defvar ob-scala--buffer-on nil)
(defvar ob-scala--buffer '())
(defvar ob-scala--buffer-result nil)
(defvar ob-scala--buffer-done nil)

(defun ob-scala-extract-lines (open acc xs)
  (let ((head (car xs))
        (tail (cdr xs)))
    (cond
     ((null xs)
      (list open acc tail))
     ((and open (ob-scala-barrier-p head))
      (list nil acc tail))
     ((and open)
      (ob-scala-extract-lines open (cons head acc) tail))
     ((and (ob-scala-barrier-p head))
      (ob-scala-extract-lines t acc tail))
     (t
      (ob-scala-extract-lines open acc tail)))))

(defun ob-scala-find-result (open acc xs forward-result)
  (seq-let (next-open next-acc tail)
      (ob-scala-extract-lines open acc xs)
    (when ob-scala--debug
      (message "ob-scala-find-result: open=%s, acc=%s, length=%s, xs=%s, forward-result=%s"
               open acc (length xs) xs forward-result)
      (message "ob-scala-find-result: next-open=%s, next-acc=%s, tail=%s" next-open next-acc tail))
    (cond
     ((and next-open)
      (list next-open next-acc nil))
     ((and (null next-open) (null open) (null next-acc))
      (list nil nil nil))
     ((and (null next-open) (null open) next-acc)
      (funcall forward-result next-acc)
      (ob-scala-find-result next-open nil tail forward-result))
     ((and (null next-open) open)
      (funcall forward-result next-acc)
      (ob-scala-find-result next-open nil tail forward-result))
     (t
      (message "unexpected: next-open=%s, next-acc=%s, tail=%s, open=%s, acc=%s, xs=%s"
               next-open next-acc tail open acc xs)
      (list nil nil nil)))))

(defun ob-scala--buffer-set-result (out)
  (when ob-scala--debug (message "ob-scala--buffer-set-result: %s" out))
  (setq ob-scala--buffer-done t)
  (setq ob-scala--buffer-result out))

(defun ob-scala--buffer-process-filter (buffer-name proc out)
  (let* ((x (replace-regexp-in-string "\x1b\\[[0-9]+[a-zA-Z]" "" out))
         (y (replace-regexp-in-string "" "" x))
         (z (replace-regexp-in-string "\n$" "" y))
         (xs (split-string z "\n")))
    (when ob-scala--debug
      (message "ob-scala--buffer-process-filter: buffer-name=%s, proc=%s, out=%s, n=%s"
               buffer-name proc xs (length xs)))
    (with-current-buffer buffer-name
      (when (and (null ob-scala--ammonite-process)
                   (string-match-p (regexp-quote "@") out))
        (setq ob-scala--ammonite-process proc))

      (cond
       ;; detect error color
       ((string-match-p "\x1b\\[31m" out)
        (ob-scala--buffer-set-result nil)
        (switch-to-buffer (process-buffer proc))
        (goto-char (point-max)))
       (t
        (seq-let (open acc tail)
            (ob-scala-find-result ob-scala--buffer-on ob-scala--buffer xs 'ob-scala--buffer-set-result)
          (setq ob-scala--buffer-on open)
          (setq ob-scala--buffer acc)
          nil))))))

(defun ob-scala-wrap-body (result-type body)
  (format "{\nprintln(\"%s\")\n%s\nprintln(\"%s\")\n}\n"
          ob-scala-barrier
          (pcase result-type
            ('value (format "println{\n%s\n}" body))
            ('output body))
          ob-scala-barrier))

(defun ob-scala-await (millis step symbol)
  (when ob-scala--debug
    (message "ob-scala-await: millis=%s, step=%s, symbol=%s value=%s" millis step symbol (eval symbol)))
  (cond
   ((and (null (eval symbol)) (< millis 1))
    (error "ob-scala-await: timeout on %s" symbol))
   ((null (eval symbol))
    (sleep-for 0 step)
    (ob-scala-await (- millis step) step symbol))
   (t
    (eval symbol))))

;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is the main function which is called to evaluate a code
;; block.
(defun org-babel-execute:scala (body params)
  "Execute a block of Scalacode with org-babel.
This function is called by `org-babel-execute-src-block'"
  (let* ((current-buffer-name (current-buffer))
         (processed-params (org-babel-process-params params))
         ;; set the session
         (session (assoc-default :session processed-params))
         (ammonite-buffer-name (org-babel-scala-initiate-session session))
         (ammonite-proc (get-buffer-process ammonite-buffer-name))
         ;; variables assigned for use in the block
         (vars (assoc-default :vars processed-params))
         (result-params (assoc-default :result-params processed-params))
         ;; either OUTPUT or VALUE which should behave as described above
         (result-type (assoc-default :result-type processed-params))
         ;; expand the body with `org-babel-expand-body:scala'
         (full-body (ob-scala-wrap-body result-type body)))

    (with-current-buffer ammonite-buffer-name
      (add-to-list 'ammonite-process-filters (apply-partially 'ob-scala--buffer-process-filter current-buffer-name)))

    (with-current-buffer current-buffer-name
      (when ob-scala--debug
        (message "current-buffer-name=%s, session=%s, ammonite-buffer-name=%s"
                 current-buffer-name session ammonite-buffer-name)
        (message "ammonite-proc=%s, vars=%s, result-params=%s, result-type=%s"
                 ammonite-proc vars result-params result-type)
        (message "ob-scala--buffer-done=%s, ob-scala--buffer-result=%s"
                 ob-scala--buffer-done ob-scala--buffer-result)
        (message "full-body=%s" full-body))

      ;; Wait for started
      (ob-scala-await 60000 500 'ob-scala--ammonite-process)

      (term-simple-send ammonite-proc full-body)

      ;; Wait for result
      (ob-scala-await 6000000 300 'ob-scala--buffer-done)

      (org-babel-scala-table-or-string
       (pcase result-type
        ('value
         (car ob-scala--buffer-result))
        ('output
         (mapconcat 'identity (reverse ob-scala--buffer-result) "\n")))))))

;; This function should be used to assign any variables in params in
;; the context of the session environment.
(defun org-babel-prep-session:scala (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (when ob-scala--debug (message "session=%s, param=%s" session params)))

(defun org-babel-scala-var-to-scala (var)
  "Convert an elisp var into a string of scala source code
specifying a var of the same value."
  (when ob-scala--debug (format "%S" var)))

(defun org-babel-scala-table-or-string (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (when ob-scala--debug (message "org-babel-scala-table-or-string=%s" results))
  results)

(defun ob-scala--ammonite-live-p ()
   (and (process-live-p ob-scala--ammonite-process)
        (buffer-live-p (process-buffer ob-scala--ammonite-process))))

(defun org-babel-scala-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION then create.
Return the initialized session."
  (unless (ob-scala--ammonite-live-p)
    (set (make-local-variable 'ob-scala--ammonite-process) nil)
    (make-local-variable 'ob-scala--buffer-on)
    (make-local-variable 'ob-scala--buffer)
    (make-local-variable 'ob-scala--buffer-done)
    (make-local-variable 'ob-scala--buffer-result))
  (setq ob-scala--buffer-on nil)
  (setq ob-scala--buffer '())
  (setq ob-scala--buffer-done nil)
  (setq ob-scala--buffer-result nil)
  (let ((current-buffer-name (current-buffer))
        (ammonite-buffer-name (ammonite session t)))
    (set-buffer current-buffer-name)
    ammonite-buffer-name))

(provide 'ob-scala)
