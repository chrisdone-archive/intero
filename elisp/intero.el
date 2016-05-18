;;; intero.el --- Complete interaction mode

;; Copyright (c) 2016 Chris Done.
;; Copyright (c) 2015 Athur Fayzrakhmanov.

;; Package-Requires: ((flycheck "26") (haskell-mode "13") (company "0.9.0"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Mode that enables:
;;
;; * Flycheck type checking ✓
;; * Company mode completion ✓
;; * Go to definition ✓
;; * Type of selection ✓
;; * Info ✓
;; * REPL
;; * Find uses
;; * List all types in all expressions

;;; Code:

(require 'flycheck)
(require 'haskell-mode)
(require 'haskell-interactive-mode)
(require 'cl-lib)
(require 'company)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration

(defconst intero-package-version "intero-0.1.8"
  "Package version to auto-install.")

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
(define-key intero-mode-map (kbd "C-c C-i") 'intero-info)
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

(defvar intero-deleting nil
  "The process of the buffer is being deleted.")
(make-variable-buffer-local 'intero-deleting)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive commands

(defun intero-type-at (insert)
  "Get the type of the thing or selection at point."
  (interactive "P")
  (let ((ty (apply #'intero-get-type-at (intero-thing-at-point))))
    (if insert
        (save-excursion
          (goto-char (line-beginning-position))
          (insert
           (format "%s\n"
                   (with-temp-buffer
                     (haskell-mode)
                     (insert ty)
                     (font-lock-fontify-buffer)
                     (buffer-string)))))
      (message
       "%s"
       (with-temp-buffer
         (haskell-mode)
         (insert ty)
         (font-lock-fontify-buffer)
         (buffer-string))))))

(defun intero-info (insert)
  "Get the info of the thing at point."
  (interactive "P")
  (let ((info (intero-get-info-of (haskell-ident-at-point))))
    (message
     "%s"
     (with-temp-buffer
       (haskell-mode)
       (insert info)
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
  (when (intero-buffer-p 'backend)
    (let ((targets (with-current-buffer (intero-buffer 'backend)
                     intero-targets)))
      (intero-destroy 'backend)
      (intero-get-worker-create 'backend targets (current-buffer))))
  (when (intero-buffer-p 'repl)
    (let ((targets (with-current-buffer (intero-buffer 'repl)
                     intero-targets)))
      (intero-destroy 'repl)
      (intero-get-worker-create 'backend targets (current-buffer)))))

(defun intero-targets ()
  "Set the targets to use for stack ghci."
  (interactive)
  (let ((targets (split-string (read-from-minibuffer "Targets: ")
                               " "
                               t)))
    (intero-destroy)
    (intero-get-worker-create 'backend targets (current-buffer))))

(defun intero-destroy (&optional worker)
  "Stop the current worker process and kill its associated."
  (interactive)
  (if worker
      (intero-delete-worker worker)
    (intero-delete-worker 'backend)
    (intero-delete-worker 'repl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DevelMain integration

(defun intero-devel-reload ()
  "Reload the module `DevelMain' and then run `DevelMain.update'.

This is for doing live update of the code of servers or GUI
applications.  Put your development version of the program in
`DevelMain', and define `update' to auto-start the program on a
new thread, and use the `foreign-store' package to access the
running context across :load/:reloads in Intero."
  (interactive)
  (unwind-protect
      (with-current-buffer
          (or (get-buffer "DevelMain.hs")
              (if (y-or-n-p
                   "You need to open a buffer named DevelMain.hs. Find now?")
                  (ido-find-file)
                (error "No DevelMain.hs buffer.")))
        (message "Reloading ...")
        (intero-async-call
         'backend
         ":l DevelMain"
         (current-buffer)
         (lambda (buffer reply)
           (if (string-match "^OK, modules loaded" reply)
               (intero-async-call
                'backend
                "DevelMain.update"
                buffer
                (lambda (_buffer reply)
                  (message "DevelMain updated. Output was:\n%s"
                           reply)))
             (progn
               (message "DevelMain FAILED. Switch to DevelMain.hs and compile that.")
               (switch-to-buffer buffer)
               (flycheck-buffer)
               (flycheck-list-errors))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck integration

(defun intero-check (checker cont)
  "Run a check and pass the status onto CONT."
  (let ((file-buffer (current-buffer)))
    (write-region (point-min) (point-max) (buffer-file-name))
    (clear-visited-file-modtime)
    (intero-async-call
     'backend
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
             (intero-async-call 'backend
                                (concat ":m + "
                                        (replace-regexp-in-string modules "," ""))
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
    (prefix (let ((prefix-info (intero-completions-grab-prefix)))
              (when prefix-info
                (cl-destructuring-bind
                    (_beg _end prefix _type) prefix-info
                  prefix))))
    (candidates (intero-get-completions arg))))

(defun intero-completions-grab-prefix (&optional minlen)
   "Grab prefix at point for possible completion."
   (when (intero-completions-can-grab-prefix)
     (let ((prefix (cond
                    ((intero-completions-grab-identifier-prefix)))))
       (cond ((and minlen prefix)
              (when (>= (length (nth 2 prefix)) minlen)
                prefix))
             (prefix prefix)))))

(defun intero-completions-can-grab-prefix ()
  "Check if the case is appropriate for grabbing completion prefix."
  (when (not (region-active-p))
    (when (looking-at-p (rx (| space line-end punct)))
      (when (not (bobp))
        (save-excursion
          (backward-char)
          (not (looking-at-p (rx (| space line-end)))))))))

(defun intero-completions-grab-identifier-prefix ()
  "Grab identifier prefix."
  (let ((pos-at-point (haskell-ident-pos-at-point))
        (p (point)))
    (when pos-at-point
      (let* ((start (car pos-at-point))
             (end (cdr pos-at-point))
             (type 'haskell-completions-identifier-prefix)
             (case-fold-search nil)
             value)
        (when (<= p end)
          (setq end p)
          (setq value (buffer-substring-no-properties start end))
          (when (string-match-p (rx bos upper) value)
            (save-excursion
              (goto-char (line-beginning-position))
              (when (re-search-forward
                     (rx "import"
                         (? (1+ space) "qualified")
                         (1+ space)
                         upper
                         (1+ (| alnum ".")))
                     p    ;; bound
                     t)   ;; no-error
                (if (equal p (point))
                    (setq type 'haskell-completions-module-name-prefix)
                  (when (re-search-forward
                         (rx (| " as " "("))
                         start
                         t)
                    (setq type 'haskell-completions-identifier-prefix))))))
          (when (nth 8 (syntax-ppss))
            (setq type 'haskell-completions-general-prefix))
          (when value (list start end value type)))))))

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
  (intero-blocking-call 'backend ":all-types"))

(defun intero-get-type-at (beg end)
  "Get the type at the given region denoted by BEG and END."
  (replace-regexp-in-string
   "\n$" ""
   (intero-blocking-call
    'backend
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

(defun intero-get-info-of (thing)
  "Get info for the thing."
  (replace-regexp-in-string
   "\n$" ""
   (intero-blocking-call
    'backend
    (format ":i %s" thing))))

(defun intero-get-loc-at (beg end)
  "Get the location of the identifier denoted by BEG and END."
  (replace-regexp-in-string
   "\n$" ""
   (intero-blocking-call
    'backend
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
    'backend
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
                 'backend
                 (format ":complete repl %S" prefix))
                "\n"
                t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process communication

(defun intero-delete-worker (worker)
  "Delete the given worker."
  (when (intero-buffer-p worker)
    (with-current-buffer (intero-get-buffer-create worker)
      (when (get-buffer-process (current-buffer))
        (setq intero-deleting t)
        (kill-process (get-buffer-process (current-buffer)))
        (delete-process (get-buffer-process (current-buffer))))
      (kill-buffer (current-buffer)))))

(defun intero-blocking-call (worker cmd)
  "Make a synchronous call of CMD to the process."
  (let ((result (list nil)))
    (intero-async-call
     worker
     cmd
     result
     (lambda (result reply)
       (setf (car result) reply)))
    (with-current-buffer (intero-buffer worker)
      (while (not (null intero-callbacks))
        (sleep-for 0.0001)))
    (car result)))

(defun intero-async-call (worker cmd &optional state callback)
  "Make an asynchronous call of CMD (string) to the process,
calling CALLBACK as (CALLBACK STATE REPLY)."
  (with-current-buffer (intero-buffer worker)
    (setq intero-callbacks
          (append intero-callbacks
                  (list (list state
                              (or callback #'ignore)
                              cmd)))))
  (process-send-string (intero-process worker)
                       (concat cmd "\n")))

(defun intero-buffer (worker)
  "Get the worker buffer for the current directory."
  (let ((buffer (intero-get-buffer-create worker)))
    (if (get-buffer-process buffer)
        buffer
      (intero-get-worker-create worker))))

(defun intero-process (worker)
  "Get the worker process for the current directory."
  (get-buffer-process (intero-buffer worker)))

(defun intero-get-worker-create (worker &optional targets source-buffer)
  "Start an Intero worker."
  (let* ((buffer (intero-get-buffer-create worker)))
    (if (get-buffer-process buffer)
        buffer
      (if (intero-installed-p)
          (intero-start-process-in-buffer buffer targets source-buffer)
        (intero-auto-install buffer targets source-buffer)))))

(defun intero-auto-install (buffer &optional targets source-buffer)
  "Automatically install Intero."
  (switch-to-buffer buffer)
  (erase-buffer)
  (insert "Intero is not installed in the Stack environment.

Installing automatically ...

")
  (cl-case (call-process "stack" nil (current-buffer) t "build" intero-package-version)
    (0
     (insert "\nInstalled successfully! Starting Intero in a moment ...")
     (run-with-timer 2
                     nil
                     'intero-start-process-in-buffer
                     buffer targets source-buffer))
    (1 (insert (propertize "Could not install Intero!

We don't know why it failed. Please read the above output and try
installing manually. If that doesn't work, report this as a
problem.
"
                           'face 'compilation-error)))))

(defun intero-start-process-in-buffer (buffer &optional targets source-buffer)
  "Start an Intero worker in BUFFER for TARGETS, automatically
performing a initial actions in SOURCE-BUFFER, if specified."
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
      (erase-buffer)
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
    (set-process-sentinel process 'intero-sentinel)
    buffer))

(defun intero-sentinel (process change)
  "Handle a CHANGE to the PROCESS."
  (when (buffer-live-p (process-buffer process))
    (when (and (not (process-live-p process)))
      (if (with-current-buffer (process-buffer process)
            intero-deleting)
          (message "Intero process deleted.")
        (intero-show-process-problem process change)))))

(defun intero-installed-p ()
  "Is intero installed in the stack environment?"
  (= 0 (call-process "stack" nil nil nil "exec" "--" "intero" "--version")))

(defun intero-show-process-problem (process change)
  "Show the user that a CHANGE occurred on PROCESS, causing it to
end."
  (message "Problem with Intero!")
  (switch-to-buffer (process-buffer process))
  (goto-char (point-max))
  (insert "\n---\n\n")
  (insert
   (propertize
    (concat
     "This is the buffer where Emacs talks to intero. It's normally hidden,
but a problem occcured.

It may be obvious if there is some text above this message
indicating a problem.

The process ended. Here is the reason that Emacs gives us:

"
     "  " change
     "\n"
     "For troubleshooting purposes, here are the arguments used to launch intero:

"
     (format "  stack ghci %s"
             (mapconcat #'identity
                        intero-arguments
                        " "))
     "

After fixing this problem, you could switch back to your code and
run M-x intero-restart to try again.

You can kill this buffer when you're done reading it.\n")
    'face 'compilation-error)))

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

(defun intero-get-buffer-create (worker)
  "Get or create the stack buffer for this current directory and
the given targets."
  (let* ((root (intero-project-root))
         (default-directory root))
    (with-current-buffer
        (get-buffer-create (intero-buffer-name worker))
      (cd root)
      (current-buffer))))

(defun intero-buffer-p (worker)
  "Does a buffer exist for a given worker?"
  (get-buffer (intero-buffer-name worker)))

(defun intero-buffer-name (worker)
  "For a given WORKER, create a buffer name."
  (let ((root (intero-project-root)))
    (concat " intero:"
            (format "%s" worker)
            ":"
            (file-name-nondirectory root)
            " "
            root)))

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
