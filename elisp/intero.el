;;; intero.el --- Complete development mode for Haskell

;; Copyright (c) 2016 Chris Done
;; Copyright (c) 2015 Athur Fayzrakhmanov
;; Copyright (c) 2013 Herbert Valerio Riedel
;; Copyright (c) 2007 Stefan Monnier

;; Package-Requires: ((flycheck "26") (company "0.9.0"))

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
(require 'cl-lib)
(require 'company)
(require 'comint)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration

(defconst intero-package-version "0.1.13"
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
(define-key intero-mode-map (kbd "C-c C-l") 'intero-repl-load)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer-local variables/state

(defvar intero-callbacks (list)
  "List of callbacks waiting for output. FIFO.")
(make-variable-buffer-local 'intero-callbacks)

(defvar intero-arguments (list)
  "Arguments used to call the stack process.")
(make-variable-buffer-local 'intero-arguments)

(defvar intero-targets (list)
  "Targets used for the stack process.")
(make-variable-buffer-local 'intero-targets)

(defvar intero-project-root nil
  "The project root of the current buffer.")
(make-variable-buffer-local 'intero-project-root)

(defvar intero-package-name nil
  "The package name associated with the current buffer.")
(make-variable-buffer-local 'intero-package-name)

(defvar intero-deleting nil
  "The process of the buffer is being deleted.")
(make-variable-buffer-local 'intero-deleting)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive commands

(defun intero-list-buffers ()
  "List hidden process buffers created by intero.

You can use this to kill them or look inside."
  (interactive)
  (let ((buffers (cl-remove-if-not
                  (lambda (buffer)
                    (string-match " intero:" (buffer-name buffer)))
                  (buffer-list))))
    (if buffers
        (display-buffer
         (list-buffers-noselect
          nil
          buffers))
      (error "There are no Intero process buffers."))))

(defun intero-cd ()
  "Change directory in the backend process."
  (interactive)
  (intero-async-call
   'backend
   (concat ":cd "
           (read-directory-name "Change Intero directory: "))))

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
                     (when (fboundp 'haskell-mode)
                       (haskell-mode))
                     (insert ty)
                     (font-lock-fontify-buffer)
                     (buffer-string)))))
      (message
       "%s"
       (with-temp-buffer
         (when (fboundp 'haskell-mode)
           (haskell-mode))
         (insert ty)
         (font-lock-fontify-buffer)
         (buffer-string))))))

(defun intero-info (insert)
  "Get the info of the thing at point."
  (interactive "P")
  (let ((info (intero-get-info-of (intero-ident-at-point))))
    (message
     "%s"
     (with-temp-buffer
       (when (fboundp 'haskell-mode)
         (haskell-mode))
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
    (intero-delete-worker 'backend)))

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
               (location (intero-parse-error
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

(defconst intero-error-regexp-alist
  `((,(concat
       "^ *\\(?1:[^\t\r\n]+?\\):"
       "\\(?:"
       "\\(?2:[0-9]+\\):\\(?4:[0-9]+\\)\\(?:-\\(?5:[0-9]+\\)\\)?" ;; "121:1" & "12:3-5"
       "\\|"
       "(\\(?2:[0-9]+\\),\\(?4:[0-9]+\\))-(\\(?3:[0-9]+\\),\\(?5:[0-9]+\\))" ;; "(289,5)-(291,36)"
       "\\)"
       ":\\(?6: Warning:\\)?")
     1 (2 . 3) (4 . 5) (6 . nil)) ;; error/warning locus

    ;; multiple declarations
    ("^    \\(?:Declared at:\\|            \\) \\(?1:[^ \t\r\n]+\\):\\(?2:[0-9]+\\):\\(?4:[0-9]+\\)$"
     1 2 4 0) ;; info locus

    ;; this is the weakest pattern as it's subject to line wrapping et al.
    (" at \\(?1:[^ \t\r\n]+\\):\\(?2:[0-9]+\\):\\(?4:[0-9]+\\)\\(?:-\\(?5:[0-9]+\\)\\)?[)]?$"
     1 2 (4 . 5) 0)) ;; info locus
  "Regexps used for matching GHC compile messages.")

(defun intero-parse-error (string)
  "Parse the line number from the error."
  (let ((span nil))
    (cl-loop for regex
             in intero-error-regexp-alist
             do (when (string-match (car regex) string)
                  (setq span
                        (list :file (match-string 1 string)
                              :line (string-to-number (match-string 2 string))
                              :col (string-to-number (match-string 4 string))
                              :line2 (when (match-string 3 string)
                                       (string-to-number (match-string 3 string)))
                              :col2 (when (match-string 5 string)
                                      (string-to-number (match-string 5 string)))))))
    span))

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
                    (beg end prefix _type) prefix-info
                  prefix))))
    (candidates
     (let ((prefix-info (intero-completions-grab-prefix)))
       (when prefix-info
         (cl-destructuring-bind
             (beg end prefix _type) prefix-info
           (intero-get-completions beg end)))))))

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
  (let ((pos-at-point (intero-ident-pos-at-point))
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
;; REPL

(defconst intero-prompt-regexp "^\4 ")

(defun intero-repl-load ()
  "Load the current file in the REPL."
  (interactive)
  (let ((file (buffer-file-name))
        (repl-buffer (intero-repl-buffer)))
    (with-current-buffer repl-buffer
      (comint-send-string
       (get-buffer-process (current-buffer))
       (concat ":l " file "\n")))
    (pop-to-buffer repl-buffer)))

(defun intero-repl ()
  "Start up the REPL for this stack project."
  (interactive)
  (switch-to-buffer (intero-repl-buffer)))

(defun intero-repl-buffer ()
  (let* ((root (intero-project-root))
         (package-name (intero-package-name))
         (name (format "*intero:%s:%s:repl*"
                       (file-name-nondirectory root)
                       package-name))
         (backend-buffer (intero-buffer 'backend)))
    (if (get-buffer name)
        (get-buffer name)
      (with-current-buffer
          (get-buffer-create name)
        (cd root)
        (intero-repl-mode)
        (intero-repl-mode-start (buffer-local-value 'intero-targets backend-buffer))
        (current-buffer)))))

(define-derived-mode intero-repl-mode comint-mode "Intero-REPL"
  "Interactive prompt for Intero."
  (when (and (not (eq major-mode 'fundamental-mode))
             (eq this-command 'intero-repl-mode))
    (error "You probably meant to run: M-x intero-repl"))
  (set (make-local-variable 'comint-prompt-regexp) intero-prompt-regexp))

(defun intero-repl-mode-start (targets)
  "Start the process for the repl buffer."
  (setq intero-targets targets)
  (let ((arguments (intero-make-options-list intero-targets)))
    (insert (propertize
             (format "Starting:\n  stack ghci %s\n" (mapconcat #'identity arguments " "))
             'face 'font-lock-comment-face))
    (let ((script (with-current-buffer (find-file-noselect (make-temp-file "intero-script"))
                    (insert ":set prompt \"\"
:set -fobject-code
:set prompt \"\\4 \"
")
                    (basic-save-buffer)
                    (buffer-file-name))))
      (let ((process (apply #'start-process "intero" (current-buffer) "stack" "ghci"
                            (append arguments
                                    (list "--verbosity" "silent")
                                    (list "--ghci-options"
                                          (concat "-ghci-script=" script))))))
        (when (process-live-p process)
          (message "Started Intero process for REPL."))))))

;; For live migration, remove later
(font-lock-remove-keywords
 'intero-repl-mode
 '(("\\(\4\\)"
    (0 (prog1 ()
         (compose-region (match-beginning 1)
                         (match-end 1)
                         ?λ))))))

(font-lock-add-keywords
 'intero-repl-mode
 '(("\\(\4\\)"
    (0 (prog1 ()
         (compose-region (match-beginning 1)
                         (match-end 1)
                         ?‽))))))

(define-key intero-repl-mode-map [remap move-beginning-of-line] 'intero-repl-beginning-of-line)
(define-key intero-repl-mode-map [remap delete-backward-char] 'intero-repl-delete-backward-char)

(defun intero-repl-delete-backward-char ()
  "Delete backwards, excluding the prompt."
  (interactive)
  (unless (looking-back intero-prompt-regexp)
    (call-interactively 'delete-backward-char)))

(defun intero-repl-beginning-of-line ()
  "Go to the beginning of the line, excluding the prompt."
  (interactive)
  (if (search-backward-regexp intero-prompt-regexp (line-beginning-position) t 1)
      (goto-char (+ 2 (line-beginning-position)))
    (call-interactively 'move-beginning-of-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer operations

(defun intero-thing-at-point ()
  "Return (list START END) of something at the point."
  (if (region-active-p)
      (list (region-beginning)
            (region-end))
    (let ((pos (intero-ident-pos-at-point)))
      (if pos
          (list (car pos) (cdr pos))
        (list (point) (point))))))

(defun intero-ident-at-point ()
  "Return the identifier under point, or nil if none found.
May return a qualified name."
  (let ((reg (intero-ident-pos-at-point)))
    (when reg
      (buffer-substring-no-properties (car reg) (cdr reg)))))

(defun intero-ident-pos-at-point ()
  "Return the span of the identifier under point, or nil if none found.
May return a qualified name."
  (save-excursion
    ;; Skip whitespace if we're on it.  That way, if we're at "map ", we'll
    ;; see the word "map".
    (if (and (not (eobp))
             (eq ?  (char-syntax (char-after))))
        (skip-chars-backward " \t"))

    (let ((case-fold-search nil))
      (cl-multiple-value-bind (start end)
          (list
           (progn (skip-syntax-backward "w_") (point))
           (progn (skip-syntax-forward "w_") (point)))
        ;; If we're looking at a module ID that qualifies further IDs, add
        ;; those IDs.
        (goto-char start)
        (while (and (looking-at "[[:upper:]]") (eq (char-after end) ?.)
                    ;; It's a module ID that qualifies further IDs.
                    (goto-char (1+ end))
                    (save-excursion
                      (when (not (zerop (skip-syntax-forward
                                         (if (looking-at "\\s_") "_" "w'"))))
                        (setq end (point))))))
        ;; If we're looking at an ID that's itself qualified by previous
        ;; module IDs, add those too.
        (goto-char start)
        (if (eq (char-after) ?.) (forward-char 1)) ;Special case for "."
        (while (and (eq (char-before) ?.)
                    (progn (forward-char -1)
                           (not (zerop (skip-syntax-backward "w'"))))
                    (skip-syntax-forward "'")
                    (looking-at "[[:upper:]]"))
          (setq start (point)))
        ;; This is it.
        (unless (= start end)
          (cons start end))))))

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

(defun intero-get-completions (beg end)
  "Get completions for a PREFIX."
  (split-string
   (intero-blocking-call
    'backend
    (format ":complete-at %S %d %d %d %d %S"
            (buffer-file-name)
            (save-excursion (goto-char beg)
                            (line-number-at-pos))
            (save-excursion (goto-char beg)
                            (1+ (current-column)))
            (save-excursion (goto-char end)
                            (line-number-at-pos))
            (save-excursion (goto-char end)
                            (1+ (current-column)))
            (buffer-substring-no-properties beg end)))
   "\n"
   t))

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
      (intero-get-worker-create worker nil (current-buffer)))))

(defun intero-process (worker)
  "Get the worker process for the current directory."
  (get-buffer-process (intero-buffer worker)))

(defun intero-get-worker-create (worker &optional targets source-buffer)
  "Start an Intero worker."
  (let* ((buffer (intero-get-buffer-create worker)))
    (if (get-buffer-process buffer)
        buffer
      (let ((install-status (intero-installed-p)))
        (if (eq install-status 'installed)
            (intero-start-process-in-buffer buffer targets source-buffer)
          (intero-auto-install buffer install-status targets source-buffer))))))

(defun intero-auto-install (buffer install-status &optional targets source-buffer)
  "Automatically install Intero."
  (let ((source-buffer (or source-buffer (current-buffer))))
    (switch-to-buffer buffer)
    (erase-buffer)
    (insert (cl-case install-status
              (not-installed "Intero is not installed in the Stack environment.")
              (wrong-version "The wrong version of Intero is installed for this Emacs package."))
            (format "

Installing intero-%s automatically ...

" intero-package-version))
    (redisplay)
    (cl-case (call-process "stack" nil (current-buffer) t "build"
                           (with-current-buffer buffer
                             (let* ((cabal-file (intero-cabal-find-file))
                                    (package-name (intero-package-name cabal-file)))
                               ;; For local development. Most users'll
                               ;; never hit this behaviour.
                               (if (string= package-name "intero")
                                   "intero"
                                 (concat "intero-" intero-package-version)))))
      (0
       (message "Installed successfully! Starting Intero in a moment ...")
       (bury-buffer buffer)
       (switch-to-buffer source-buffer)
       (intero-start-process-in-buffer buffer targets source-buffer))
      (1 (insert (propertize "Could not install Intero!

We don't know why it failed. Please read the above output and try
installing manually. If that doesn't work, report this as a
problem.
"
                             'face 'compilation-error))))))

(defun intero-start-process-in-buffer (buffer &optional targets source-buffer)
  "Start an Intero worker in BUFFER for TARGETS, automatically
performing a initial actions in SOURCE-BUFFER, if specified."
  (let* ((options
          (intero-make-options-list
           (or targets
               (list (buffer-local-value 'intero-package-name buffer)))))
         (arguments options)
         (process (with-current-buffer buffer
                    (when debug-on-error
                      (message "Intero arguments: %S" arguments))
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

(defun intero-make-options-list (targets)
  "Make the stack ghci options list."
  (append (list "--with-ghc"
                "intero"
                "--docker-run-args=--interactive=true --tty=false"
                "--no-load"
                "--no-build")
          (let ((dir (make-temp-file "intero" t)))
            (list "--ghci-options"
                  (format "%S" (concat "-odir=" dir))
                  "--ghci-options"
                  (format "%S" (concat "-hidir=" dir))))
          targets))

(defun intero-sentinel (process change)
  "Handle a CHANGE to the PROCESS."
  (when (buffer-live-p (process-buffer process))
    (when (and (not (process-live-p process)))
      (if (with-current-buffer (process-buffer process)
            intero-deleting)
          (message "Intero process deleted.")
        (intero-show-process-problem process change)))))

(defun intero-installed-p ()
  "Is intero (of the right version) installed in the stack environment?"
  (redisplay)
  (with-temp-buffer
    (if (= 0 (call-process "stack" nil t nil "exec" "--" "intero" "--version"))
        (progn
          (goto-char (point-min))
          (if (string= (buffer-substring (point-min) (line-end-position))
                       intero-package-version)
              'installed
            'wrong-version))
      'not-installed)))

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
  (let* ((cabal-file (intero-cabal-find-file))
         (package-name (intero-package-name cabal-file))
         (buffer-name (intero-buffer-name worker))
         (default-directory (file-name-directory cabal-file)))
    (with-current-buffer
        (get-buffer-create buffer-name)
      (setq intero-package-name package-name)
      (cd default-directory)
      (current-buffer))))

(defun intero-buffer-p (worker)
  "Does a buffer exist for a given worker?"
  (get-buffer (intero-buffer-name worker)))

(defun intero-buffer-name (worker)
  "For a given WORKER, create a buffer name."
  (let* ((root (intero-project-root))
         (package-name (intero-package-name)))
    (concat " intero:"
            (format "%s" worker)
            ":"
            package-name
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

(defun intero-package-name (&optional cabal-file)
  "Get the current package name from a nearby .cabal file. If
there is none, return empty string."
  (or intero-package-name
      (setq intero-package-name
            (let ((cabal-file (or cabal-file
                                  (intero-cabal-find-file))))
              (if cabal-file
                  (replace-regexp-in-string
                   ".cabal$" ""
                   (file-name-nondirectory cabal-file))
                "")))))

(defun intero-cabal-find-file (&optional dir)
  "Search for package description file upwards starting from DIR.
If DIR is nil, `default-directory' is used as starting point for
directory traversal.  Upward traversal is aborted if file owner
changes.  Uses `intero-cabal-find-pkg-desc' internally."
  (let ((use-dir (or dir default-directory)))
    (while (and use-dir (not (file-directory-p use-dir)))
      (setq use-dir (file-name-directory (directory-file-name use-dir))))
    (when use-dir
      (catch 'found
        (let ((user (nth 2 (file-attributes use-dir)))
              ;; Abbreviate, so as to stop when we cross ~/.
              (root (abbreviate-file-name use-dir)))
          ;; traverse current dir up to root as long as file owner doesn't change
          (while (and root (equal user (nth 2 (file-attributes root))))
            (let ((cabal-file (intero-cabal-find-pkg-desc root)))
              (when cabal-file
                (throw 'found cabal-file)))

            (let ((proot (file-name-directory (directory-file-name root))))
              (if (equal proot root) ;; fix-point reached?
                  (throw 'found nil)
                (setq root proot))))
          nil)))))

(defun intero-cabal-find-pkg-desc (dir &optional allow-multiple)
  "Find a package description file in the directory DIR.
Returns nil if none or multiple \".cabal\" files were found.  If
ALLOW-MULTIPLE is non nil, in case of multiple \".cabal\" files,
a list is returned instead of failing with a nil result."
  ;; This is basically a port of Cabal's
  ;; Distribution.Simple.Utils.findPackageDesc function
  ;;  http://hackage.haskell.org/packages/archive/Cabal/1.16.0.3/doc/html/Distribution-Simple-Utils.html
  ;; but without the exception throwing.
  (let* ((cabal-files
          (cl-remove-if 'file-directory-p
                        (cl-remove-if-not 'file-exists-p
                                          (directory-files dir t ".\\.cabal\\'")))))
    (cond
     ((= (length cabal-files) 1) (car cabal-files)) ;; exactly one candidate found
     (allow-multiple cabal-files) ;; pass-thru multiple candidates
     (t nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'intero)

;;; intero.el ends here
