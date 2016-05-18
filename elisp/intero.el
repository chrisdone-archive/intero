;;; intero.el --- Complete interaction mode

;; Copyright (c) 2016 Chris Done. All rights reserved.

;; Package-Requires: ((flycheck "26") (haskell-mode "13") (company "0.9.0"))

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:

;; * Redistributions of source code must retain the above copyright
;;   notice, this list of conditions and the following disclaimer.

;; * Redistributions in binary form must reproduce the above copyright
;;   notice, this list of conditions and the following disclaimer in
;;   the documentation and/or other materials provided with the
;;   distribution.

;; * Neither the name of intero nor the names of its contributors may
;;   be used to endorse or promote products derived from this software
;;   without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:
;;
;; Mode that enables:
;;
;; * Flycheck type checking ✓
;; * Company mode completion ✓
;; * Go to definition ✓
;; * Type of selection ✓
;; * Find uses
;; * List all types in all expressions

;;; Code:

(require 'flycheck)
(require 'haskell-mode)
(require 'haskell-completions)
(require 'haskell-interactive-mode)
(require 'cl-lib)
(require 'company)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes

(defvar intero-mode-map (make-sparse-keymap)
  "Intero minor mode's map.")

(define-minor-mode intero-mode "Minor mode for Intero"
  :lighter " Intero"
  :keymap intero-mode-map
  (when (buffer-file-name)
    (if intero-mode
        (progn (flycheck-select-checker 'intero)
               (flycheck-mode)
               (add-to-list (make-local-variable 'company-backends) 'company-intero)
               (company-mode))
      (message "Intero mode disabled."))))

(define-key intero-mode-map (kbd "C-c C-t") 'intero-type-at)
(define-key intero-mode-map (kbd "M-.") 'intero-goto-definition)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer-local variables/state

(defvar intero-callbacks (list)
  "List of callbacks waiting for output. FIFO.")
(make-variable-buffer-local 'intero-callbacks)

(defvar intero-arguments (list)
  "Arguments used to call the stack process.")
(make-variable-buffer-local 'intero-arguments)

(defvar intero-targets ""
  "Targets used for the stack process.")
(make-variable-buffer-local 'intero-targets)

(defvar intero-project-root nil
  "The project root of the current buffer.")
(make-variable-buffer-local 'intero-project-root)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive commands

(defun intero-type-at ()
  "Get the type of the thing or selection at point."
  (interactive)
  (message
   "%s"
   (let ((result (apply #'intero-get-type-at (intero-thing-at-point))))
     (with-temp-buffer
       (haskell-mode)
       (insert result)
       (font-lock-fontify-buffer)
       (buffer-string)))))

(defun intero-goto-definition ()
  "Jump to the definition of the thing at point."
  (interactive)
  (let ((result (apply #'intero-get-loc-at (intero-thing-at-point))))
    (when (string-match "\\(.*?\\):(\\([0-9]+\\),\\([0-9]+\\))-(\\([0-9]+\\),\\([0-9]+\\))$"
                        result)
      (let ((file (match-string 1 result))
            (line (string-to-number (match-string 2 result)))
            (col (string-to-number (match-string 3 result))))
        (find-file file)
        (pop-mark)
        (goto-char (point-min))
        (forward-line (1- line))
        (forward-char (1- col))))))

(defun intero-restart ()
  "Simply restart the process with the same configuration as before."
  (interactive)
  (let ((targets (with-current-buffer (intero-buffer)
                   intero-targets)))
    (intero-destroy)
    (intero-get-worker-create targets (current-buffer))))

(defun intero-targets ()
  "Set the targets to use for stack ghci."
  (interactive)
  (let ((targets (split-string (read-from-minibuffer "Targets: ")
                               " "
                               t)))
    (intero-destroy)
    (intero-get-worker-create targets (current-buffer))))

(defun intero-destroy ()
  "Stop the current worker process and kill its associated."
  (interactive)
  (with-current-buffer (intero-get-buffer-create)
    (when (get-buffer-process (current-buffer))
      (kill-process (get-buffer-process (current-buffer)))
      (delete-process (get-buffer-process (current-buffer))))
    (kill-buffer (current-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck integration

(defun intero-check (checker cont)
  "Run a check and pass the status onto CONT."
  (let ((file-buffer (current-buffer)))
    (write-region (point-min) (point-max) (buffer-file-name))
    (clear-visited-file-modtime)
    (intero-async-call
     (concat ":l " (buffer-file-name))
     (list :cont cont
           :file-buffer file-buffer
           :checker checker)
     (lambda (state string)
       (with-current-buffer (plist-get state :file-buffer)
         (funcall (plist-get state :cont)
                  'finished
                  (intero-parse-errors-warnings
                   (plist-get state :checker)
                   (current-buffer)
                   string))
         (when (string-match "OK, modules loaded: \\(.*\\)\\.$" string)
           (let ((modules (match-string 1 string)))
             (intero-async-call (concat ":m + "
                                        (replace-in-string modules "," ""))
                                nil
                                (lambda (_st _))))))))))

(flycheck-define-generic-checker 'intero
  "A syntax and type checker for Haskell using an Intero worker
process."
  :start 'intero-check
  :modes '(haskell-mode)
  :next-checkers '((warning . haskell-hlint)))

(add-to-list 'flycheck-checkers 'intero)

(defun intero-parse-errors-warnings (checker buffer string)
  "Parse from the given STRING a list of flycheck errors and
warnings, adding CHECKER and BUFFER to each one."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let ((messages (list)))
      (while (search-forward-regexp
              (concat "[\r\n]\\([A-Z]?:?[^ \r\n:][^:\n\r]+\\):\\([0-9()-:]+\\):"
                      "[ \n\r]+\\([[:unibyte:][:nonascii:]]+?\\)\n[^ ]")
              nil t 1)
        (let* ((file (match-string 1))
               (location-raw (match-string 2))
               (msg (match-string 3))
               (type (cond ((string-match "^Warning:" msg)
                            (setq msg (replace-regexp-in-string "^Warning: *" "" msg))
                            'warning)
                           ((string-match "^Splicing " msg) 'splice)
                           (t                               'error)))
               (location (haskell-process-parse-error
                          (concat file ":" location-raw ": x")))
               (line (plist-get location :line))
               (column (plist-get location :col)))
          (setq messages
                (cons (flycheck-error-new-at
                       line column type msg
                       :checker checker
                       :buffer (when (string= (buffer-file-name buffer)
                                              file)
                                 buffer)
                       :filename file)
                      messages))))
      messages)))

(defun intero-call-in-buffer (buffer func &rest args)
  "Utility function which calls FUNC in BUFFER with ARGS."
  (with-current-buffer buffer
    (apply func args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company integration (auto-completion)

(defun company-intero (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-intero))
    (prefix (let ((prefix-info (haskell-completions-grab-prefix)))
              (when prefix-info
                (cl-destructuring-bind
                    (_beg _end prefix _type) prefix-info
                  prefix))))
    (candidates (intero-get-completions arg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer operations

(defun intero-thing-at-point ()
  "Return (list START END) of something at the point."
  (if (region-active-p)
      (list (region-beginning)
            (region-end))
    (let ((pos (haskell-ident-pos-at-point)))
      (if pos
          (list (car pos) (cdr pos))
        (list (point) (point))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Query/commands

(defun intero-get-all-types ()
  "Get all types in all expressions in all modules."
  (intero-blocking-call ":all-types"))

(defun intero-get-type-at (beg end)
  "Get the type at the given region denoted by BEG and END."
  (replace-regexp-in-string
   "\n$" ""
   (intero-blocking-call
    (format ":type-at %S %d %d %d %d %S"
            (buffer-file-name)
            (save-excursion (goto-char beg)
                            (line-number-at-pos))
            (save-excursion (goto-char beg)
                            (1+ (current-column)))
            (save-excursion (goto-char end)
                            (line-number-at-pos))
            (save-excursion (goto-char end)
                            (1+ (current-column)))
            (buffer-substring-no-properties beg end)))))

(defun intero-get-loc-at (beg end)
  "Get the location of the identifier denoted by BEG and END."
  (replace-regexp-in-string
   "\n$" ""
   (intero-blocking-call
    (format ":loc-at %S %d %d %d %d %S"
            (buffer-file-name)
            (save-excursion (goto-char beg)
                            (line-number-at-pos))
            (save-excursion (goto-char beg)
                            (1+ (current-column)))
            (save-excursion (goto-char end)
                            (line-number-at-pos))
            (save-excursion (goto-char end)
                            (1+ (current-column)))
            (buffer-substring-no-properties beg end)))))

(defun intero-get-uses-at (beg end)
  "Get the uses of the identifier denoted by BEG and END."
  (replace-regexp-in-string
   "\n$" ""
   (intero-blocking-call
    (format ":uses %S %d %d %d %d %S"
            (buffer-file-name)
            (save-excursion (goto-char beg)
                            (line-number-at-pos))
            (save-excursion (goto-char beg)
                            (1+ (current-column)))
            (save-excursion (goto-char end)
                            (line-number-at-pos))
            (save-excursion (goto-char end)
                            (1+ (current-column)))
            (buffer-substring-no-properties beg end)))))

(defun intero-get-completions (prefix)
  "Get completions for a PREFIX."
  (mapcar #'read
          (cdr (split-string
                (intero-blocking-call
                 (format ":complete repl %S" prefix))
                "\n"
                t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process communication

(defun intero-async-call (cmd &optional state callback)
  "Make an asynchronous call of CMD (string) to the process,
calling CALLBACK as (CALLBACK STATE REPLY)."
  (with-current-buffer (intero-buffer)
    (setq intero-callbacks
          (append intero-callbacks
                  (list (list state
                              (or callback #'ignore)
                              cmd)))))
  (process-send-string (intero-process)
                       (concat cmd "\n")))

(defun intero-blocking-call (cmd)
  "Make a synchronous call of CMD to the process."
  (let ((result (list nil)))
    (intero-async-call
     cmd
     result
     (lambda (result reply)
       (setf (car result) reply)))
    (with-current-buffer (intero-buffer)
      (while (not (null intero-callbacks))
        (sleep-for 0.0001)))
    (car result)))

(defun intero-buffer ()
  "Get the worker buffer for the current directory."
  (let ((buffer (intero-get-buffer-create)))
    (if (get-buffer-process buffer)
        buffer
      (intero-get-worker-create))))

(defun intero-process ()
  "Get the worker process for the current directory."
  (get-buffer-process (intero-buffer)))

(defun intero-get-worker-create (&optional targets source-buffer)
  "Start an Intero worker."
  (let* ((buffer (intero-get-buffer-create)))
    (if (get-buffer-process buffer)
        buffer
      (let* ((options (list "--with-ghc"
                            "intero"
                            "--no-load"
                            "--no-build"))
             (main-is (list))
             (arguments (append options
                                targets
                                main-is))
             (process (with-current-buffer buffer
                        (message "Booting up intero ...")
                        (apply #'start-process "stack" buffer "stack" "ghci"
                               arguments))))
        (process-send-string process ":set -fobject-code\n")
        (process-send-string process ":set prompt \"\\4\"\n")
        (with-current-buffer buffer
          (setq intero-targets targets)
          (setq intero-arguments arguments)
          (setq intero-callbacks
                (list (list source-buffer
                            (lambda (source-buffer _msg)
                              (when source-buffer
                                (with-current-buffer source-buffer
                                  (when flycheck-mode
                                    (run-with-timer 0 nil
                                                    'intero-call-in-buffer
                                                    (current-buffer)
                                                    'flycheck-buffer))))
                              (message "Booted up intero!"))))))
        (set-process-filter process
                            (lambda (process string)
                              (when (buffer-live-p (process-buffer process))
                                (with-current-buffer (process-buffer process)
                                  (goto-char (point-max))
                                  (insert string)
                                  (intero-read-buffer)))))
        (set-process-sentinel process
                              (lambda (process change)
                                (when (buffer-live-p (process-buffer process))
                                  (when (not (process-live-p process))
                                    (switch-to-buffer (process-buffer process))
                                    (goto-char (point-max))
                                    (insert "\n---\n
This is the buffer where Emacs talks to intero. It's normally hidden,
but a problem occcured.\n")
                                    (insert "\nThe process ended. Here is the reason:\n"
                                            "  " change
                                            "\n")
                                    (insert "For troubleshooting purposes, here are the arguments used to launch intero:\n"
                                            (format "  stack ghci %s"
                                                    (mapconcat #'identity
                                                               intero-arguments
                                                               " "))
                                            "\n\n")
                                    (insert "You can kill this buffer when you're ready.\n")))))
        buffer))))

(defun intero-read-buffer ()
  "In the process buffer, we read what's in it."
  (let ((repeat t))
    (while repeat
      (setq repeat nil)
      (goto-char (point-min))
      (when (search-forward "\4" (point-max) t 1)
        (let* ((next-callback (pop intero-callbacks))
               (state (nth 0 next-callback))
               (func (nth 1 next-callback)))
          (let ((string (buffer-substring (point-min) (1- (point)))))
            (if next-callback
                (progn (with-temp-buffer
                         (funcall func state string))
                       (setq repeat t))
              (when debug-on-error
                (warn "Received output but no callback in `intero-callbacks': %S"
                      string)))))
        (delete-region (point-min) (point))))))

(defun intero-get-buffer-create ()
  "Get or create the stack buffer for this current directory and
the given targets."
  (let* ((root (intero-project-root))
         (default-directory root))
    (with-current-buffer
        (get-buffer-create (concat " intero:"
                                   (file-name-nondirectory root)
                                   " "
                                   root))
      (cd root)
      (current-buffer))))

(defun intero-project-root ()
  "Get the directory where the stack.yaml is placed for this
project, or the global one."
  (if intero-project-root
      intero-project-root
    (setq intero-project-root
          (with-temp-buffer
            (save-excursion
              (call-process "stack" nil
                            (current-buffer)
                            nil
                            "path"
                            "--project-root"
                            "--verbosity" "silent"))
            (buffer-substring (line-beginning-position) (line-end-position))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'intero)

;;; intero.el ends here
