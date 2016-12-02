;;; intero.el --- Complete development mode for Haskell

;; Copyright (c) 2016 Chris Done
;; Copyright (C) 2016 Артур Файзрахманов
;; Copyright (c) 2015 Athur Fayzrakhmanov
;; Copyright (C) 2015 Gracjan Polak
;; Copyright (c) 2013 Herbert Valerio Riedel
;; Copyright (c) 2007 Stefan Monnier

;; Author: Chris Done <chrisdone@fpcomplete.com>
;; Maintainer: Chris Done <chrisdone@fpcomplete.com>
;; URL: https://github.com/commercialhaskell/intero
;; Created: 3rd June 2016
;; Version: 0.1.13
;; Keywords: haskell, tools
;; Package-Requires: ((flycheck "0.25") (company "0.8") (emacs "24.3") (haskell-mode "13.0"))

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
;; * REPL ✓
;; * Apply suggestions (extensions, imports, etc.) ✓
;; * Find uses
;; * Completion of stack targets ✓
;; * List all types in all expressions
;; * Import management
;; * Dependency management

;;; Code:

(require 'flycheck)
(require 'json)
(require 'warnings)
(require 'cl-lib)
(require 'company)
(require 'comint)
(require 'widget)
(require 'eldoc)
(eval-when-compile
  (require 'wid-edit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration

(defgroup intero nil
  "Complete development mode for Haskell"
  :group 'haskell)

(defcustom intero-package-version
  "0.1.20"
  "Package version to auto-install.

This version does not necessarily have to be the latest version
of intero published on Hackage.  Sometimes there are changes to
Intero which have no use for the Emacs mode.  It is only bumped
when the Emacs mode actually requires newer features from the
intero executable, otherwise we force our users to upgrade
pointlessly."
  :group 'intero
  :type 'string)

(defcustom intero-repl-no-load
  t
  "Pass --no-load when starting the repl.
This causes it to skip loading the files from the selected target."
  :group 'intero
  :type 'boolean)
(make-variable-buffer-local 'intero-repl-no-load)

(defcustom intero-repl-no-build
  t
  "Pass --no-build when starting the repl.
This causes it to skip building the target."
  :group 'intero
  :type 'boolean)
(make-variable-buffer-local 'intero-repl-no-build)

(defcustom intero-debug nil
  "Show debug output."
  :group 'intero
  :type 'boolean)

(defcustom intero-whitelist
  nil
  "Projects to whitelist.

It should be a list of directories.

To use this, use the following mode hook:
  (add-hook 'haskell-mode-hook 'intero-mode-whitelist)"
  :group 'intero
  :type 'string)

(defcustom intero-blacklist
  nil
  "Projects to blacklist.

It should be a list of directories.

To use this, use the following mode hook:
  (add-hook 'haskell-mode-hook 'intero-mode-blacklist)"
  :group 'intero
  :type 'string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes

(defvar intero-mode-map (make-sparse-keymap)
  "Intero minor mode's map.")

(defvar-local intero-lighter " Intero"
  "Lighter for the intero minor mode.")

;;;###autoload
(define-minor-mode intero-mode
  "Minor mode for Intero

\\{intero-mode-map}"
  :lighter intero-lighter
  :keymap intero-mode-map
  (when (bound-and-true-p interactive-haskell-mode)
    (when (fboundp 'interactive-haskell-mode)
      (message "Disabling interactive-haskell-mode ...")
      (interactive-haskell-mode -1)))
  (if intero-mode
      (progn (flycheck-select-checker 'intero)
             (flycheck-mode)
             (add-to-list (make-local-variable 'company-backends) 'company-intero)
             (company-mode)
             (setq-local eldoc-documentation-function 'eldoc-intero))
    (message "Intero mode disabled.")))

(defun intero-mode-whitelist ()
  "Run intero-mode when the current project is in `intero-whitelist'."
  (interactive)
  (let ((file (buffer-file-name)))
    (when (cl-remove-if-not (lambda (directory)
                              (file-in-directory-p file directory))
                            intero-whitelist)
      (intero-mode))))

(defun intero-mode-blacklist ()
  "Run intero-mode unless the current project is in `intero-blacklist'."
  (interactive)
  (let ((file (buffer-file-name)))
    (unless (cl-remove-if-not (lambda (directory)
                                (file-in-directory-p file directory))
                              intero-blacklist)
      (intero-mode))))

(define-key intero-mode-map (kbd "C-c C-t") 'intero-type-at)
(define-key intero-mode-map (kbd "C-c C-i") 'intero-info)
(define-key intero-mode-map (kbd "M-.") 'intero-goto-definition)
(define-key intero-mode-map (kbd "C-c C-l") 'intero-repl-load)
(define-key intero-mode-map (kbd "C-c C-z") 'intero-repl)
(define-key intero-mode-map (kbd "C-c C-r") 'intero-apply-suggestions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global variables/state

(defvar intero-global-mode nil
  "Global mode is enabled?")

(defun global-intero-mode ()
  "Enable Intero on all Haskell mode buffers."
  (interactive)
  (setq intero-global-mode (not intero-global-mode))
  (if intero-global-mode
      (add-hook 'haskell-mode-hook 'intero-mode)
    (remove-hook 'haskell-mode-hook 'intero-mode))
  (when (eq this-command 'global-intero-mode)
    (message "Intero mode is now %s on all future Haskell buffers."
             (if intero-global-mode
                 "enabled" "disabled"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer-local variables/state

(defvar-local intero-callbacks (list)
  "List of callbacks waiting for output.
LIST is a FIFO.")

(defvar-local intero-arguments (list)
  "Arguments used to call the stack process.")

(defvar-local intero-targets (list)
  "Targets used for the stack process.")

(defvar-local intero-source-buffer (list)
  "Buffer from which Intero was first requested to start.")

(defvar-local intero-project-root nil
  "The project root of the current buffer.")

(defvar-local intero-package-name nil
  "The package name associated with the current buffer.")

(defvar-local intero-deleting nil
  "The process of the buffer is being deleted.")

(defvar-local intero-give-up nil
  "When non-nil, give up trying to start the backend.
A true value indicates that the backend could not start, or could
not be installed.  The user will have to manually run
`intero-restart' or `intero-targets' to destroy the buffer and
create a fresh one without this variable enabled.")

(defvar-local intero-try-with-build nil
  "Try starting intero without --no-build.
This is slower, but will build required dependencies.")

(defvar-local intero-starting nil
  "When non-nil, indicates that the intero process starting up.")

(defvar-local intero-hoogle-port nil
  "Port that hoogle server is listening on.")

(defvar-local intero-suggestions nil
  "Auto actions for the buffer.")

(defvar-local intero-extensions nil
  "Extensions supported by the compiler.")

(defvar intero-ghc-version nil
  "GHC version used by the project.")

(defvar-local intero-repl-last-loaded
  nil
  "The last loaded thing with `intero-repl-load`.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive commands

(defun intero-add-package (package)
  "Add a package dependency to the currently running project backend."
  (interactive "sPackage: ")
  (intero-blocking-call 'backend (concat ":set -package " package))
  (flycheck-buffer))

(defun intero-toggle-debug ()
  "Toggle debugging mode on/off."
  (interactive)
  (setq intero-debug (not intero-debug))
  (message "Intero debugging is: %s" (if intero-debug "ON" "OFF")))

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
      (error "There are no Intero process buffers"))))

(defun intero-cd ()
  "Change directory in the backend process."
  (interactive)
  (intero-async-call
   'backend
   (concat ":cd "
           (read-directory-name "Change Intero directory: "))))

(defun intero-fontify-expression (expression)
  "Return a haskell-fontified version of EXPRESSION."
  (with-temp-buffer
    (when (fboundp 'haskell-mode)
      (let ((flycheck-checkers nil)
            (haskell-mode-hook nil))
        (haskell-mode)))
    (insert expression)
    (if (fboundp 'font-lock-ensure)
        (font-lock-ensure)
      (font-lock-fontify-buffer))
    (buffer-string)))

(defun intero-type-at (insert)
  "Get the type of the thing or selection at point.

With prefix argument INSERT, inserts the type above the current
line as a type signature."
  (interactive "P")
  (let ((ty (apply #'intero-get-type-at (intero-thing-at-point))))
    (if insert
        (save-excursion
          (goto-char (line-beginning-position))
          (insert (intero-fontify-expression ty) "\n"))
      (message
       "%s" (intero-fontify-expression ty)))))

(defun intero-info (ident)
  "Get the info of the thing with IDENT at point."
  (interactive (list (intero-ident-at-point)))
  (let ((origin-buffer (current-buffer))
        (package (intero-package-name))
        (info (intero-get-info-of ident))
        (help-xref-following nil)
        (origin (buffer-name)))
    (help-setup-xref (list #'intero-call-in-buffer origin-buffer 'intero-info ident)
                     (called-interactively-p 'interactive))
    (save-excursion
      (let ((help-xref-following nil))
        (with-help-window (help-buffer)
          (with-current-buffer (help-buffer)
            (insert
             (intero-fontify-expression ident)
             " in `"
             origin
             "'"
             " (" package ")"
             "\n\n"
             (intero-fontify-expression info))
            (goto-char (point-min))))))))

(defun intero-goto-definition ()
  "Jump to the definition of the thing at point.
Returns nil when unable to find definition."
  (interactive)
  (let ((result (apply #'intero-get-loc-at (intero-thing-at-point))))
    (when (string-match "\\(.*?\\):(\\([0-9]+\\),\\([0-9]+\\))-(\\([0-9]+\\),\\([0-9]+\\))$"
                        result)
      (if (fboundp 'xref-push-marker-stack) ;; Emacs 25
          (xref-push-marker-stack)
        (with-no-warnings
          (ring-insert find-tag-marker-ring (point-marker))))
      (let ((file (match-string 1 result))
            (line (string-to-number (match-string 2 result)))
            (col (string-to-number (match-string 3 result))))
        (unless (string= file (intero-temp-file-name))
          (find-file file))
        (pop-mark)
        (goto-char (point-min))
        (forward-line (1- line))
        (forward-char (1- col))
        t))))

(defun intero-restart ()
  "Simply restart the process with the same configuration as before."
  (interactive)
  (when (intero-buffer-p 'backend)
    (let ((targets (with-current-buffer (intero-buffer 'backend)
                     intero-targets)))
      (intero-destroy 'backend)
      (intero-get-worker-create 'backend targets (current-buffer))
      (intero-repl-restart))))

(defun intero-targets ()
  "Set the targets to use for stack ghci."
  (interactive)
  (let* ((old-targets
          (with-current-buffer (intero-buffer 'backend)
            intero-targets))
         (available-targets (intero-get-targets))
         (targets (if available-targets
                      (intero-multiswitch
                       "Targets:"
                       (mapcar (lambda (target)
                                 (list :key target
                                       :title target
                                       :default (member target old-targets)))
                               available-targets))
                    (split-string (read-from-minibuffer "Targets: " nil nil nil nil old-targets)
                                  " "
                                  t))))
    (intero-destroy)
    (intero-get-worker-create 'backend targets (current-buffer))
    (intero-repl-restart)))

(defun intero-destroy (&optional worker)
  "Stop WORKER and kill its associated process buffer.
If not provided, WORKER defaults to the current worker process."
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
                   "You need to open a buffer named DevelMain.hs.  Find now? ")
                  (ido-find-file)
                (error "No DevelMain.hs buffer")))
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
  "Run a check with CHECKER and pass the status onto CONT."
  (if (intero-gave-up 'backend)
      (run-with-timer 0
                      nil
                      cont
                      'interrupted)
    (let ((file-buffer (current-buffer)))
      (intero-async-call
       'backend
       (concat ":l " (intero-temp-file-name))
       (list :cont cont
             :file-buffer file-buffer
             :checker checker)
       (lambda (state string)
         (let ((compile-ok (string-match "OK, modules loaded: \\(.*\\)\\.$" string)))
           (with-current-buffer (plist-get state :file-buffer)
             (let ((modules (match-string 1 string))
                   (msgs (intero-parse-errors-warnings-splices
                          (plist-get state :checker)
                          (current-buffer)
                          string)))
               (intero-collect-compiler-messages msgs)
               (funcall (plist-get state :cont)
                        'finished
                        (cl-remove-if (lambda (msg)
                                        (eq 'splice (flycheck-error-level msg)))
                                      msgs))
               (when compile-ok
                 (intero-async-call 'backend
                                    (concat ":m + "
                                            (replace-regexp-in-string modules "," ""))
                                    nil
                                    (lambda (_st _))))))))))))


(flycheck-define-generic-checker 'intero
  "A syntax and type checker for Haskell using an Intero worker
process."
  :start 'intero-check
  :modes '(haskell-mode literate-haskell-mode))

(add-to-list 'flycheck-checkers 'intero)

(defun intero-parse-errors-warnings-splices (checker buffer string)
  "Parse flycheck errors and warnings.
CHECKER and BUFFER are added to each item parsed from STRING."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let ((messages (list))
          (temp-file (intero-temp-file-name buffer)))
      (while (search-forward-regexp
              (concat "[\r\n]\\([A-Z]?:?[^ \r\n:][^:\n\r]+\\):\\([0-9()-:]+\\):"
                      "[ \n\r]+\\([[:unibyte:][:nonascii:]]+?\\)\n[^ ]")
              nil t 1)
        (let* ((file (intero-canonicalize-path (match-string 1)))
               (location-raw (match-string 2))
               (msg (match-string 3)) ;; Replace gross bullet points.
               (type (cond ((string-match "^Warning:" msg)
                            (setq msg (replace-regexp-in-string "^Warning: *" "" msg))
                            (if (string-match "^\\[-Wdeferred-type-errors\\]" msg)
                                'error
                              'warning))
                           ((string-match "^Splicing " msg) 'splice)
                           (t                               'error)))
               (location (intero-parse-error
                          (concat file ":" location-raw ": x")))
               (line (plist-get location :line))
               (column (plist-get location :col)))
          (setq messages
                (cons (flycheck-error-new-at
                       line column type
                       msg
                       :checker checker
                       :buffer (when (string= temp-file file)
                                 buffer)
                       :filename (intero-buffer-file-name buffer))
                      messages)))
        (forward-line -1))
      (delete-dups messages))))

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
  "Parse the line number from the error in STRING."
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
  "In BUFFER, call FUNC with ARGS."
  (with-current-buffer buffer
    (apply func args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company integration (auto-completion)

(defconst intero-pragmas
  '("CONLIKE" "SCC" "DEPRECATED" "INCLUDE" "INCOHERENT" "INLINABLE" "INLINE"
    "LANGUAGE" "LINE" "MINIMAL" "NOINLINE" "NOUNPACK" "OPTIONS" "OPTIONS_GHC"
    "OVERLAPPABLE" "OVERLAPPING" "OVERLAPS" "RULES" "SOURCE" "SPECIALIZE"
    "UNPACK" "WARNING")
  "Pragmas that GHC supports.")

(defun company-intero (command &optional arg &rest ignored)
  "Company source for intero, with the standard COMMAND and ARG args.
Other arguments are IGNORED."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-intero))
    (prefix
     (unless (intero-gave-up 'backend)
       (let ((prefix-info (intero-completions-grab-prefix)))
         (when prefix-info
           (cl-destructuring-bind
               (beg end prefix _type) prefix-info
             prefix)))))
    (candidates
     (unless (intero-gave-up 'backend)
       (let ((prefix-info (intero-completions-grab-prefix)))
         (when prefix-info
           (cons :async
                 (-partial 'intero-company-callback
                           (current-buffer)
                           prefix-info))))))))

(defun intero-company-callback (source-buffer prefix-info cont)
  "Generate completions for SOURCE-BUFFER based on PREFIX-INFO and call CONT on the results."
  (cl-destructuring-bind
      (beg end prefix type) prefix-info
    (or (and (bound-and-true-p intero-mode)
             (cl-case type
               (haskell-completions-module-name-prefix
                (intero-get-repl-completions source-buffer (concat "import " prefix) cont)
                t)
               (haskell-completions-identifier-prefix
                (intero-get-completions source-buffer beg end cont)
                t)
               (haskell-completions-language-extension-prefix
                (intero-get-repl-completions
                 source-buffer
                 (concat ":set -X" prefix)
                 (-partial (lambda (cont results)
                             (funcall cont
                                      (mapcar (lambda (x)
                                                (replace-regexp-in-string "^-X" "" x))
                                              results)))
                           cont))
                t)
               (haskell-completions-pragma-name-prefix
                (funcall cont
                         (cl-remove-if-not
                          (lambda (candidate)
                            (string-match (concat "^" prefix) candidate))
                          intero-pragmas))
                t)))
        (intero-get-repl-completions source-buffer prefix cont))))

(defun intero-completions-grab-prefix (&optional minlen)
  "Grab prefix at point for possible completion.
If specified, MINLEN is the shortest completion which will be
considered."
  (when (intero-completions-can-grab-prefix)
    (let ((prefix (cond
                   ((intero-completions-grab-pragma-prefix))
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

(defun intero-completions-grab-pragma-prefix ()
  "Grab completion prefix for pragma completions.
Returns a list of form '(prefix-start-position
prefix-end-position prefix-value prefix-type) for pramga names
such as WARNING, DEPRECATED, LANGUAGE etc.  Also returns
completion prefixes for options in case OPTIONS_GHC pragma, or
language extensions in case of LANGUAGE pragma.  Obsolete OPTIONS
pragma is supported also."
  (when (nth 4 (syntax-ppss))
    ;; We're inside comment
    (let ((p (point))
          (comment-start (nth 8 (syntax-ppss)))
          (case-fold-search nil)
          prefix-start
          prefix-end
          prefix-type
          prefix-value)
      (save-excursion
        (goto-char comment-start)
        (when (looking-at (rx "{-#" (1+ (| space "\n"))))
          (let ((pragma-start (match-end 0)))
            (when (> p pragma-start)
              ;; point stands after `{-#`
              (goto-char pragma-start)
              (when (looking-at (rx (1+ (| upper "_"))))
                ;; found suitable sequence for pragma name
                (let ((pragma-end (match-end 0))
                      (pragma-value (match-string-no-properties 0)))
                  (if (eq p pragma-end)
                      ;; point is at the end of (in)complete pragma name
                      ;; prepare resulting values
                      (progn
                        (setq prefix-start pragma-start)
                        (setq prefix-end pragma-end)
                        (setq prefix-value pragma-value)
                        (setq prefix-type
                              'haskell-completions-pragma-name-prefix))
                    (when (and (> p pragma-end)
                               (or (equal "OPTIONS_GHC" pragma-value)
                                   (equal "OPTIONS" pragma-value)
                                   (equal "LANGUAGE" pragma-value)))
                      ;; point is after pragma name, so we need to check
                      ;; special cases of `OPTIONS_GHC` and `LANGUAGE` pragmas
                      ;; and provide a completion prefix for possible ghc
                      ;; option or language extension.
                      (goto-char pragma-end)
                      (when (re-search-forward
                             (rx (* anything)
                                 (1+ (regexp "\\S-")))
                             p
                             t)
                        (let* ((str (match-string-no-properties 0))
                               (split (split-string str (rx (| space "\n")) t))
                               (val (car (last split)))
                               (end (point)))
                          (when (and (equal p end)
                                     (not (string-match-p "#" val)))
                            (setq prefix-value val)
                            (backward-char (length val))
                            (setq prefix-start (point))
                            (setq prefix-end end)
                            (setq
                             prefix-type
                             (if (not (equal "LANGUAGE" pragma-value))
                                 'haskell-completions-ghc-option-prefix
                               'haskell-completions-language-extension-prefix
                               )))))))))))))
      (when prefix-value
        (list prefix-start prefix-end prefix-value prefix-type)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ELDoc integration

(defvar-local eldoc-intero-cache (make-hash-table :test 'equal)
  "Cache for types of regions, used by `eldoc-intero'.
This is not for saving on requests (we make a request even if
something is in cache, overwriting the old entry), but rather for
making types show immediately when we do have them cached.")

(defun eldoc-intero-maybe-print (msg)
  "Print MSG with eldoc if eldoc would display a message now.
Like `eldoc-print-current-symbol-info', but just printing MSG
instead of using `eldoc-documentation-function'."
  (with-demoted-errors "eldoc error: %s"
    (and (or (eldoc-display-message-p)
             ;; Erase the last message if we won't display a new one.
             (when eldoc-last-message
               (eldoc-message nil)
               nil))
         (eldoc-message msg))))

(defun eldoc-intero ()
  "ELDoc backend for intero."
  (let ((buffer (intero-buffer 'backend)))
    (when (and buffer (process-live-p (get-buffer-process buffer)))
      (apply #'intero-get-type-at-async
             (lambda (beg end ty)
               (let ((response-status (intero-haskell-utils-repl-response-error-status ty)))
                 (if (eq 'no-error response-status)
                     (let ((msg (intero-fontify-expression
                                 (replace-regexp-in-string "[ \n]+" " " ty))))
                       ;; Got an updated type-at-point, cache and print now:
                       (puthash (list beg end)
                                msg
                                eldoc-intero-cache)
                       (eldoc-intero-maybe-print msg))
                   ;; But if we're seeing errors, invalidate cache-at-point:
                   (remhash (list beg end) eldoc-intero-cache))))
             (intero-thing-at-point))))
  ;; If we have something cached at point, print that first:
  (gethash (intero-thing-at-point) eldoc-intero-cache))

(defun intero-haskell-utils-repl-response-error-status (response)
  "Parse response REPL's RESPONSE for errors.
Returns one of the following symbols:

+ unknown-command
+ option-missing
+ interactive-error
+ no-error

*Warning*: this funciton covers only three kind of responses:

* \"unknown command …\"
  REPL missing requested command
* \"<interactive>:3:5: …\"
  interactive REPL error
* \"Couldn't guess that module name. Does it exist?\"
  (:type-at and maybe some other commands error)
* *all other reposnses* are treated as success reposneses and
  'no-error is returned."
  (let ((first-line (car (split-string response "\n" t))))
    (cond
     ((null first-line) 'no-error)
     ((string-match-p "^unknown command" first-line)
      'unknown-command)
     ((string-match-p
       "^Couldn't guess that module name. Does it exist?"
       first-line)
      'option-missing)
     ((string-match-p "^<interactive>:" first-line)
      'interactive-error)
     ((string-match-p "^<no location info>:" first-line)
      'inspection-error)
     (t 'no-error))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPL

(defconst intero-prompt-regexp "^\4 ")

(defvar-local intero-repl-previous-buffer nil
  "Records the buffer to which `intero-repl-switch-back' should jump.
This is set by `intero-repl-buffer', and should otherwise be nil.")

(defun intero-repl-clear-buffer ()
  "Clear the current REPL buffer."
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(defun intero-repl-load (&optional prompt-options)
  "Load the current file in the REPL.
If PROMPT-OPTIONS is non-nil, prompt with an options list."
  (interactive "P")
  (save-buffer)
  (let ((file (intero-temp-file-name))
        (repl-buffer (intero-repl-buffer prompt-options t)))
    (with-current-buffer repl-buffer
      (comint-simple-send
       (get-buffer-process (current-buffer))
       (if (string= intero-repl-last-loaded file)
           ":r"
         (concat ":l " file)))
      (setq intero-repl-last-loaded file))
    (pop-to-buffer repl-buffer)))

(defun intero-repl (&optional prompt-options)
  "Start up the REPL for this stack project.
If PROMPT-OPTIONS is non-nil, prompt with an options list."
  (interactive "P")
  (switch-to-buffer-other-window (intero-repl-buffer prompt-options t)))

(defun intero-repl-restart ()
  "Restart the REPL."
  (interactive)
  (let* ((root (intero-project-root))
         (package-name (intero-package-name))
         (backend-buffer (intero-buffer 'backend))
         (name (format "*intero:%s:%s:repl*"
                       (file-name-nondirectory root)
                       package-name)))
    (when (get-buffer name)
      (with-current-buffer (get-buffer name)
        (goto-char (point-max))
        (let ((process (get-buffer-process (current-buffer))))
          (when process (kill-process process)))
        (intero-repl-mode-start backend-buffer
                                (buffer-local-value 'intero-targets backend-buffer)
                                nil)))))

(defun intero-repl-buffer (prompt-options &optional store-previous)
  "Start the REPL buffer.
If PROMPT-OPTIONS is non-nil, prompt with an options list.  When
STORE-PREVIOUS is non-nil, note the caller's buffer in
`intero-repl-previous-buffer'."
  (let* ((root (intero-project-root))
         (package-name (intero-package-name))
         (name (format "*intero:%s:%s:repl*"
                       (file-name-nondirectory root)
                       package-name))
         (initial-buffer (current-buffer))
         (backend-buffer (intero-buffer 'backend)))
    (with-current-buffer
        (if (get-buffer name)
            (get-buffer name)
          (with-current-buffer
              (get-buffer-create name)
            (cd root)
            (intero-repl-mode)
            (intero-repl-mode-start backend-buffer
                                    (buffer-local-value 'intero-targets backend-buffer)
                                    prompt-options)
            (current-buffer)))
      (progn
        (when store-previous
          (setq intero-repl-previous-buffer initial-buffer))
        (current-buffer)))))

(defvar intero-hyperlink-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1]  'intero-find-file-with-line:char)
    (define-key map [C-return] 'intero-find-file-with-line:char)
    map)
  "Keymap for clicking on links in REPL.")

(defun intero-find-file-with-line:char ()
  "Jump to the file and location indicated by text properties at point."
  (interactive)
  (let ((file (get-text-property (point) 'file))
        (line (get-text-property (point) 'line))
        (char (get-text-property (point) 'char)))
    (with-no-warnings (find-file-other-window file))
    (goto-char (point-min))
    (forward-line (1- line))
    (forward-char (1- char))))

(defun intero-linkify-file-line-char (begin end)
  "Linkify all occurences of <file>:<line>:<char>: betwen BEGIN and END."
  (when (> end begin)
    (let ((end-marker (copy-marker end))
          ;; match - /path/to/file.ext:<line>:<char>:
          ;;       - /path/to/file.ext:<line>:<char>-
          ;;       - /path/to/file.ext:(<line>:<char>)
          (file:line:char-regexp "\\([A-Z]?:?[^ \r\n:][^:\n\r]+\\)[:](?\\([0-9]+\\)[:,]\\([0-9]+\\)[:)-]"))
      (save-excursion
        (goto-char begin)
        ;; Delete unrecognized escape sequences.
        (while (re-search-forward file:line:char-regexp end-marker t)
          (let ((file (match-string-no-properties 1))
                (line (match-string-no-properties 2))
                (char (match-string-no-properties 3))
                (link-start (1+ (match-beginning 1)))
                (link-end   (1+ (match-end 2))))
            (add-text-properties
             link-start link-end
             (list 'keymap intero-hyperlink-map
                   'file   file
                   'line   (string-to-number line)
                   'char   (string-to-number char)
                   'help-echo "mouse-2: visit this file"))))))))

(defvar intero-last-output-newline-marker nil)

(defun intero-linkify-process-output (_)
  "Comint filter function to make <file>:<line>:<char>: into clickable links.

Note that this function uses the `intero-last-output-newline-marker',
to keep track of line breaks.  The `intero-linkify-file-line-char'
function is subsequently applied to each line, once."
  (unless intero-last-output-newline-marker
    (setq-local intero-last-output-newline-marker (make-marker))
    (set-marker intero-last-output-newline-marker (marker-position comint-last-output-start)))
  (let ((start-marker (if (and (markerp intero-last-output-newline-marker)
                               (eq (marker-buffer intero-last-output-newline-marker)
                                   (current-buffer))
                               (marker-position intero-last-output-newline-marker))
                          comint-last-output-start
                        (point-min-marker)))
        (end-marker (process-mark (get-buffer-process (current-buffer)))))
    (save-excursion
      (goto-char start-marker)
      (while (re-search-forward "[\n\r]" end-marker t)
        (intero-linkify-file-line-char intero-last-output-newline-marker (match-beginning 0))
        (set-marker intero-last-output-newline-marker (match-end 0))))))

(define-derived-mode intero-repl-mode comint-mode "Intero-REPL"
  "Interactive prompt for Intero."
  (when (and (not (eq major-mode 'fundamental-mode))
             (eq this-command 'intero-repl-mode))
    (error "You probably meant to run: M-x intero-repl"))
  (setq-local comint-prompt-regexp intero-prompt-regexp)
  (setq-local warning-suppress-types (cons '(undo discard-info) warning-suppress-types))
  (add-hook 'comint-output-filter-functions
            'intero-linkify-process-output
            t)
  (setq-local comint-prompt-read-only t)
  (add-to-list (make-local-variable 'company-backends) 'company-intero)
  (company-mode))

(defun intero-repl-mode-start (backend-buffer targets prompt-options)
  "Start the process for the repl in the current buffer.
BACKEND-BUFFER is used for options.
TARGETS is the targets to load.
If PROMPT-OPTIONS is non-nil, prompt with an options list."
  (setq intero-targets targets)
  (when prompt-options
    (intero-repl-options backend-buffer))
  (let ((arguments (intero-make-options-list
                    (or targets
                        (let ((package-name (buffer-local-value 'intero-package-name
                                                                backend-buffer)))
                          (unless (equal "" package-name)
                            (list package-name))))
                    (buffer-local-value 'intero-repl-no-build backend-buffer)
                    (buffer-local-value 'intero-repl-no-load backend-buffer))))
    (insert (propertize
             (format "Starting:\n  stack ghci %s\n" (combine-and-quote-strings arguments))
             'face 'font-lock-comment-face))
    (let* ((script-buffer
            (with-current-buffer (find-file-noselect (intero-make-temp-file "intero-script"))
              (insert ":set prompt \"\"
:set -fbyte-code
:set prompt \"\\4 \"
")
              (basic-save-buffer)
              (current-buffer)))
           (script
            (with-current-buffer script-buffer
              (intero-buffer-file-name))))
      (let ((process
             (get-buffer-process
              (apply #'make-comint-in-buffer "intero" (current-buffer) "stack" nil "ghci"
                     (append arguments
                             (list "--verbosity" "silent")
                             (list "--ghci-options"
                                   (concat "-ghci-script=" script)))))))
        (when (process-live-p process)
          (set-process-query-on-exit-flag process nil)
          (message "Started Intero process for REPL.")
          (kill-buffer script-buffer))))))

(defun intero-repl-options (backend-buffer)
  "Open an option menu to set options used when starting the REPL.
Default options come from user customization and any temporary
changes in the BACKEND-BUFFER."
  (interactive)
  (let* ((old-options
          (list
           (list :key "load-all"
                 :title "Load all modules"
                 :default (not (buffer-local-value 'intero-repl-no-load backend-buffer)))
           (list :key "build-first"
                 :title "Build project first"
                 :default (not (buffer-local-value 'intero-repl-no-build backend-buffer)))))
         (new-options (intero-multiswitch "Start REPL with options:" old-options)))
    (with-current-buffer backend-buffer
      (setq intero-repl-no-load (not (member "load-all" new-options)))
      (setq intero-repl-no-build (not (member "build-first" new-options))))))

(font-lock-add-keywords
 'intero-repl-mode
 '(("\\(\4\\)"
    (0 (prog1 ()
         (compose-region (match-beginning 1)
                         (match-end 1)
                         ?λ))))))

(define-key intero-repl-mode-map [remap move-beginning-of-line] 'intero-repl-beginning-of-line)
(define-key intero-repl-mode-map [remap delete-backward-char] 'intero-repl-delete-backward-char)
(define-key intero-repl-mode-map (kbd "C-c C-k") 'intero-repl-clear-buffer)
(define-key intero-repl-mode-map (kbd "C-c C-z") 'intero-repl-switch-back)

(defun intero-repl-delete-backward-char ()
  "Delete backwards, excluding the prompt."
  (interactive)
  (unless (looking-back intero-prompt-regexp (line-beginning-position))
    (call-interactively 'delete-backward-char)))

(defun intero-repl-beginning-of-line ()
  "Go to the beginning of the line, excluding the prompt."
  (interactive)
  (if (search-backward-regexp intero-prompt-regexp (line-beginning-position) t 1)
      (goto-char (+ 2 (line-beginning-position)))
    (call-interactively 'move-beginning-of-line)))

(defun intero-repl-switch-back ()
  "Switch back to the buffer from which this REPL buffer was reached."
  (interactive)
  (if intero-repl-previous-buffer
      (switch-to-buffer-other-window intero-repl-previous-buffer)
    (message "No previous buffer.")))

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
  "Return the span of the identifier near point going backward.
Returns nil if no identifier found or point is inside string or
comment.  May return a qualified name."
  (when (not (nth 8 (syntax-ppss)))
    ;; Do not handle comments and strings
    (let (start end)
      ;; Initial point position is non-deterministic, it may occur anywhere
      ;; inside identifier span, so the approach is:
      ;; - first try go left and find left boundary
      ;; - then try go right and find right boundary
      ;;
      ;; In both cases assume the longest path, e.g. when going left take into
      ;; account than point may occur at the end of identifier, when going right
      ;; take into account that point may occur at the beginning of identifier.
      ;;
      ;; We should handle `.` character very careful because it is heavily
      ;; overloaded.  Examples of possible cases:
      ;; Control.Monad.>>=  -- delimiter
      ;; Control.Monad.when -- delimiter
      ;; Data.Aeson..:      -- delimiter and operator symbol
      ;; concat.map         -- composition function
      ;; .?                 -- operator symbol
      (save-excursion
        ;; First, skip whitespace if we're on it, moving point to last
        ;; identifier char.  That way, if we're at "map ", we'll see the word
        ;; "map".
        (when (and (looking-at (rx eol))
                   (not (bolp)))
          (backward-char))
        (when (and (not (eobp))
                   (eq (char-syntax (char-after)) ? ))
          (skip-chars-backward " \t")
          (backward-char))
        ;; Now let's try to go left.
        (save-excursion
          (if (not (intero-mode--looking-at-varsym))
              ;; Looking at non-operator char, this is quite simple
              (progn
                (skip-syntax-backward "w_")
                ;; Remember position
                (setq start (point)))
            ;; Looking at operator char.
            (while (and (not (bobp))
                        (intero-mode--looking-at-varsym))
              ;; skip all operator chars backward
              (setq start (point))
              (backward-char))
            ;; Extra check for case when reached beginning of the buffer.
            (when (intero-mode--looking-at-varsym)
              (setq start (point))))
          ;; Slurp qualification part if present.  If identifier is qualified in
          ;; case of non-operator point will stop before `.` dot, but in case of
          ;; operator it will stand at `.` delimiting dot.  So if we're looking
          ;; at `.` let's step one char forward and try to get qualification
          ;; part.
          (goto-char start)
          (when (looking-at-p (rx "."))
            (forward-char))
          (let ((pos (intero-mode--skip-qualification-backward)))
            (when pos
              (setq start pos))))
        ;; Finally, let's try to go right.
        (save-excursion
          ;; Try to slurp qualification part first.
          (skip-syntax-forward "w_")
          (setq end (point))
          (while (and (looking-at (rx "." upper))
                      (not (zerop (progn (forward-char)
                                         (skip-syntax-forward "w_")))))
            (setq end (point)))
          ;; If point was at non-operator we already done, otherwise we need an
          ;; extra check.
          (while (intero-mode--looking-at-varsym)
            (forward-char)
            (setq end (point))))
        (when (not (= start end))
          (cons start end))))))

(defun intero-mode--looking-at-varsym ()
  "Return t when point stands at operator symbol."
  (when (not (eobp))
    (let ((lex (intero-lexeme-classify-by-first-char (char-after))))
      (or (eq lex 'varsym)
          (eq lex 'consym)))))

(defun intero-mode--skip-qualification-backward ()
  "Skip qualified part of identifier backward.
Expects point stands *after* delimiting dot.
Returns beginning position of qualified part or nil if no qualified part found."
  (when (not (and (bobp)
                  (looking-at (rx bol))))
    (let ((case-fold-search nil)
          pos)
      (while (and (eq (char-before) ?.)
                  (progn (backward-char)
                         (not (zerop (skip-syntax-backward "w'"))))
                  (skip-syntax-forward "'")
                  (looking-at "[[:upper:]]"))
        (setq pos (point)))
      pos)))

(defun intero-lexeme-classify-by-first-char (char)
  "Classify token by CHAR.
CHAR is a chararacter that is assumed to be the first character
of a token."
  (let ((category (get-char-code-property char 'general-category)))

    (cond
     ((or (member char '(?! ?# ?$ ?% ?& ?* ?+ ?. ?/ ?< ?= ?> ?? ?@ ?^ ?| ?~ ?\\ ?-))
          (and (> char 127)
               (member category '(Pc Pd Po Sm Sc Sk So))))
      'varsym)
     ((equal char ?:)
      'consym)
     ((equal char ?\')
      'char)
     ((equal char ?\")
      'string)
     ((member category '(Lu Lt))
      'conid)
     ((or (equal char ?_)
          (member category '(Ll Lo)))
      'varid)
     ((and (>= char ?0) (<= char ?9))
      'number)
     ((member char '(?\] ?\[ ?\( ?\) ?\{ ?\} ?\` ?\, ?\;))
      'special))))

(defun intero-buffer-file-name (&optional buffer)
  "Call function `buffer-file-name' for BUFFER and clean its result.
The path returned is canonicalized and stripped of any text properties."
  (let ((name (buffer-file-name buffer)))
    (when name
      (intero-canonicalize-path (substring-no-properties name)))))

(defvar-local intero-temp-file-name nil
  "The name of a temporary file to which the current buffer's content is copied.")

(defun intero-make-temp-file (prefix &optional dir-flag suffix)
  "Like `make-temp-file', but using a different temp directory.
PREFIX, DIR-FLAG and SUFFIX are all passed to `make-temp-file'
unmodified.  A different directory is applied so that if docker
is used with stack, the commands run inside docker can find the
path."
  (let ((temporary-file-directory
         (expand-file-name ".stack-work/intero/"
                           (intero-project-root))))
    (make-directory temporary-file-directory t)
    (make-temp-file prefix dir-flag suffix)))

(defun intero-temp-file-name (&optional buffer)
  "Return the name of a temp file containing an up-to-date copy of BUFFER's contents."
  (with-current-buffer (or buffer (current-buffer))
    (prog1
        (or intero-temp-file-name
            (setq intero-temp-file-name
                  (intero-canonicalize-path
                   (intero-make-temp-file
                    "intero" nil
                    (concat "." (if (buffer-file-name)
                                    (file-name-extension (buffer-file-name))
                                  "hs"))))))
      (let ((contents (buffer-string)))
        (with-temp-file intero-temp-file-name
          (insert contents))))))

(defun intero-canonicalize-path (path)
  "Return a standardized version of PATH.
Path names are standardised and drive names are
capitalized (relevant on Windows)."
  (intero-capitalize-drive-letter (convert-standard-filename path)))

(defun intero-capitalize-drive-letter (path)
  "Ensures the drive letter is capitalized in PATH.
This applies to paths of the form
x:\\foo\\bar (i.e., Windows)."
  (save-match-data
    (let ((drive-path (split-string path ":\\\\")))
      (if (or (null (car drive-path)) (null (cdr drive-path)))
          path
        (concat (upcase (car drive-path)) ":\\" (cadr drive-path))))))

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
    (intero-format-get-type-at beg end))))

(defun intero-get-type-at-async (cont beg end)
  "Call CONT with type of the region denoted by BEG and END.
CONT is called within the current buffer, with BEG, END and the
type as arguments."
  (intero-async-call
   'backend
   (intero-format-get-type-at beg end)
   (list :cont cont
         :source-buffer (current-buffer)
         :beg beg
         :end end)
   (lambda (state reply)
     (with-current-buffer (plist-get state :source-buffer)
       (funcall (plist-get state :cont)
                (plist-get state :beg)
                (plist-get state :end)
                (replace-regexp-in-string "\n$" "" reply))))))

(defun intero-format-get-type-at (beg end)
  "Compose a request for getting types in region from BEG to END."
  (format ":type-at %S %d %d %d %d %S"
          (intero-temp-file-name)
          (save-excursion (goto-char beg)
                          (line-number-at-pos))
          (save-excursion (goto-char beg)
                          (1+ (current-column)))
          (save-excursion (goto-char end)
                          (line-number-at-pos))
          (save-excursion (goto-char end)
                          (1+ (current-column)))
          (buffer-substring-no-properties beg end)))

(defun intero-get-info-of (thing)
  "Get info for THING."
  (let ((optimistic-result
         (replace-regexp-in-string
          "\n$" ""
          (intero-blocking-call
           'backend
           (format ":i %s" thing)))))
    (if (string-match "^<interactive>" optimistic-result)
        ;; Load the module Interpreted so that we get information,
        ;; then restore bytecode.
        (progn (intero-async-call
                'backend
                ":set -fbyte-code")
               (set-buffer-modified-p t)
               (save-buffer)
               (unless (member 'save flycheck-check-syntax-automatically)
                 (intero-async-call
                  'backend
                  (concat ":l " (intero-temp-file-name))))
               (intero-async-call
                'backend
                ":set -fobject-code")
               (replace-regexp-in-string
                "\n$" ""
                (intero-blocking-call
                 'backend
                 (format ":i %s" thing))))
      optimistic-result)))

(defun intero-get-loc-at (beg end)
  "Get the location of the identifier denoted by BEG and END."
  (replace-regexp-in-string
   "\n$" ""
   (intero-blocking-call
    'backend
    (format ":loc-at %S %d %d %d %d %S"
            (intero-temp-file-name)
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
  "Return usage list for identifier denoted by BEG and END."
  (replace-regexp-in-string
   "\n$" ""
   (intero-blocking-call
    'backend
    (format ":uses %S %d %d %d %d %S"
            (intero-temp-file-name)
            (save-excursion (goto-char beg)
                            (line-number-at-pos))
            (save-excursion (goto-char beg)
                            (1+ (current-column)))
            (save-excursion (goto-char end)
                            (line-number-at-pos))
            (save-excursion (goto-char end)
                            (1+ (current-column)))
            (buffer-substring-no-properties beg end)))))

(defun intero-get-completions (source-buffer beg end cont)
  "Get completions and send to SOURCE-BUFFER.
Prefix is marked by positions BEG and END.  Completions are
passed to CONT in SOURCE-BUFFER."
  (intero-async-call
   'backend
   (format ":complete-at %S %d %d %d %d %S"
           (intero-temp-file-name)
           (save-excursion (goto-char beg)
                           (line-number-at-pos))
           (save-excursion (goto-char beg)
                           (1+ (current-column)))
           (save-excursion (goto-char end)
                           (line-number-at-pos))
           (save-excursion (goto-char end)
                           (1+ (current-column)))
           (buffer-substring-no-properties beg end))
   (list :cont cont :source-buffer source-buffer)
   (lambda (state reply)
     (with-current-buffer
         (plist-get state :source-buffer)
       (funcall
        (plist-get state :cont)
        (if (string-match "^*** Exception" reply)
            (list)
          (mapcar
           (lambda (x)
             (replace-regexp-in-string "\\\"" "" x))
           (split-string reply "\n" t))))))))

(defun intero-get-repl-completions (source-buffer prefix cont)
  "Get REPL completions and send to SOURCE-BUFFER.
Completions for PREFIX are passed to CONT in SOURCE-BUFFER."
  (intero-async-call
   'backend
   (format ":complete repl %S" prefix)
   (list :cont cont :source-buffer source-buffer)
   (lambda (state reply)
     (with-current-buffer
         (plist-get state :source-buffer)
       (funcall
        (plist-get state :cont)
        (mapcar
         (lambda (x)
           (replace-regexp-in-string "\\\"" "" x))
         (cdr (split-string reply "\n" t))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process communication

(defun intero-delete-worker (worker)
  "Delete the given WORKER."
  (when (intero-buffer-p worker)
    (with-current-buffer (intero-get-buffer-create worker)
      (when (get-buffer-process (current-buffer))
        (setq intero-deleting t)
        (kill-process (get-buffer-process (current-buffer)))
        (delete-process (get-buffer-process (current-buffer))))
      (kill-buffer (current-buffer)))))

(defun intero-blocking-call (worker cmd)
  "Send WORKER the command string CMD and block pending its result."
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
  "Send WORKER the command string CMD.
The result, along with the given STATE, is passed to CALLBACK
as (CALLBACK STATE REPLY)."
  (let ((buffer (intero-buffer worker)))
    (if (and buffer (process-live-p (get-buffer-process buffer)))
        (progn (with-current-buffer buffer
                 (setq intero-callbacks
                       (append intero-callbacks
                               (list (list state
                                           (or callback #'ignore)
                                           cmd)))))
               (when intero-debug
                 (message "[Intero] -> %s" cmd))
               (comint-simple-send (intero-process worker) cmd))
      (error "Intero process is not running: run M-x intero-restart to start it"))))

(defun intero-buffer (worker)
  "Get the WORKER buffer for the current directory."
  (let ((buffer (intero-get-buffer-create worker)))
    (if (get-buffer-process buffer)
        buffer
      (intero-get-worker-create worker nil (current-buffer)))))

(defun intero-process (worker)
  "Get the WORKER process for the current directory."
  (get-buffer-process (intero-buffer worker)))

(defun intero-get-worker-create (worker &optional targets source-buffer)
  "Start the given WORKER.
If provided, use the specified TARGETS and SOURCE-BUFFER."
  (let* ((buffer (intero-get-buffer-create worker)))
    (if (get-buffer-process buffer)
        buffer
      (let ((install-status (intero-installed-p)))
        (if (eq install-status 'installed)
            (intero-start-process-in-buffer buffer targets source-buffer)
          (intero-auto-install buffer install-status targets source-buffer))))))

(defun intero-auto-install (buffer install-status &optional targets source-buffer)
  "Automatically install Intero appropriately for BUFFER.
INSTALL-STATUS indicates the current installation status.
If supplied, use the given TARGETS and SOURCE-BUFFER."
  (if (buffer-local-value 'intero-give-up buffer)
      buffer
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
                                   (concat "intero-" intero-package-version))))
                             "ghc-paths" "syb")
        (0
         (message "Installed successfully! Starting Intero in a moment ...")
         (bury-buffer buffer)
         (switch-to-buffer source-buffer)
         (intero-start-process-in-buffer buffer targets source-buffer))
        (1
         (with-current-buffer buffer (setq-local intero-give-up t))
         (insert (propertize "Could not install Intero!

We don't know why it failed. Please read the above output and try
installing manually. If that doesn't work, report this as a
problem.

WHAT TO DO NEXT

If you don't want to Intero to try installing itself again for
this project, just keep this buffer around in your Emacs.

If you'd like to try again next time you try use an Intero
feature, kill this buffer.
"
                             'face 'compilation-error))
         nil)))))

(defun intero-start-process-in-buffer (buffer &optional targets source-buffer)
  "Start an Intero worker in BUFFER, for the default or specified TARGETS.
Automatically performs initial actions in SOURCE-BUFFER, if specified."
  (if (buffer-local-value 'intero-give-up buffer)
      buffer
    (let* ((options
            (intero-make-options-list
             (or targets
                 (let ((package-name (buffer-local-value 'intero-package-name buffer)))
                   (unless (equal "" package-name)
                     (list package-name))))
             (not (buffer-local-value 'intero-try-with-build buffer))
             t ;; pass --no-load
             ))
           (arguments options)
           (process (with-current-buffer buffer
                      (when intero-debug
                        (message "Intero arguments: %s" (combine-and-quote-strings arguments)))
                      (message "Booting up intero ...")
                      (apply #'start-process "stack" buffer "stack" "ghci"
                             arguments))))
      (set-process-query-on-exit-flag process nil)
      (process-send-string process ":set -fobject-code\n")
      (process-send-string process ":set prompt \"\\4\"\n")
      (with-current-buffer buffer
        (erase-buffer)
        (setq intero-targets targets)
        (setq intero-source-buffer source-buffer)
        (setq intero-arguments arguments)
        (setq intero-starting t)
        (setq intero-callbacks
              (list (list (cons source-buffer
                                buffer)
                          (lambda (buffers _msg)
                            (let ((source-buffer (car buffers))
                                  (process-buffer (cdr buffers)))
                              (with-current-buffer process-buffer
                                (setq-local intero-starting nil))
                              (when source-buffer
                                (with-current-buffer source-buffer
                                  (when flycheck-mode
                                    (run-with-timer 0 nil
                                                    'intero-call-in-buffer
                                                    (current-buffer)
                                                    'intero-flycheck-buffer)))))
                            (message "Booted up intero!"))))))
      (set-process-filter
       process
       (lambda (process string)
         (when intero-debug
           (message "[Intero] <- %s" string))
         (when (buffer-live-p (process-buffer process))
           (with-current-buffer (process-buffer process)
             (goto-char (point-max))
             (insert string)
             (when (and intero-try-with-build
                        intero-starting)
               (let ((last-line (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position))))
                 (if (string-match "^Progress" last-line)
                     (message "Booting up intero (building dependencies: %s)"
                              (downcase
                               (or (car (split-string (replace-regexp-in-string
                                                       "\u0008+" "\n"
                                                       last-line)
                                                      "\n" t))
                                   "...")))
                   (message "Booting up intero ..."))))
             (intero-read-buffer)))))
      (set-process-sentinel process 'intero-sentinel)
      buffer)))

(defun intero-flycheck-buffer ()
  "Run flycheck in the buffer.
Restarts flycheck in case there was a problem and flycheck is stuck."
  (flycheck-mode -1)
  (flycheck-mode)
  (flycheck-buffer))

(defun intero-make-options-list (targets no-build no-load)
  "Make the stack ghci options list.
TARGETS are the build targets.  When non-nil, NO-BUILD and
NO-LOAD enable the correspondingly-named stack options."
  (append (list "--with-ghc"
                "intero"
                "--docker-run-args=--interactive=true --tty=false"
                )
          (when no-build
            (list "--no-build"))
          (when no-load
            (list "--no-load"))
          (let ((dir (intero-make-temp-file "intero" t)))
            (list "--ghci-options"
                  (concat "-odir=" dir)
                  "--ghci-options"
                  (concat "-hidir=" dir)))
          targets))

(defun intero-sentinel (process change)
  "Handle when PROCESS reports a CHANGE.
This is a standard process sentinel function."
  (when (buffer-live-p (process-buffer process))
    (when (and (not (process-live-p process)))
      (let ((buffer (process-buffer process)))
        (if (with-current-buffer buffer intero-deleting)
            (message "Intero process deleted.")
          (if (and (intero-unsatisfied-package-p buffer)
                   (not (buffer-local-value 'intero-try-with-build buffer)))
              (progn (with-current-buffer buffer (setq-local intero-try-with-build t))
                     (intero-start-process-in-buffer
                      buffer
                      (buffer-local-value 'intero-targets buffer)
                      (buffer-local-value 'intero-source-buffer buffer)))
            (progn (with-current-buffer buffer (setq-local intero-give-up t))
                   (intero-show-process-problem process change))))))))

(defun intero-unsatisfied-package-p (buffer)
  "Return non-nil if BUFFER contain GHCi's unsatisfied package complaint."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (search-forward-regexp "cannot satisfy -package" nil t 1))))

(defun intero-installed-p ()
  "Return non-nil if intero (of the right version) is installed in the stack environment."
  (redisplay)
  (with-temp-buffer
    (if (= 0 (call-process "stack" nil t nil "exec"
                           "--verbosity" "silent"
                           "--" "intero" "--version"))
        (progn
          (goto-char (point-min))
          ;; This skipping comes due to https://github.com/commercialhaskell/intero/pull/216/files
          (when (looking-at "Intero ")
            (goto-char (match-end 0)))
          ;;
          (if (string= (buffer-substring (point) (line-end-position))
                       intero-package-version)
              'installed
            'wrong-version))
      'not-installed)))

(defun intero-show-process-problem (process change)
  "Report to the user that PROCESS reported CHANGE, causing it to end."
  (message "Problem with Intero!")
  (switch-to-buffer (process-buffer process))
  (goto-char (point-max))
  (insert "\n---\n\n")
  (insert
   (propertize
    (concat
     "This is the buffer where Emacs talks to intero. It's normally hidden,
but a problem occcured.

TROUBLESHOOTING

It may be obvious if there is some text above this message
indicating a problem.

If you do not wish to use Intero for some projects, see
https://github.com/commercialhaskell/intero#whitelistingblacklisting-projects

The process ended. Here is the reason that Emacs gives us:

"
     "  " change
     "\n"
     "For troubleshooting purposes, here are the arguments used to launch intero:

"
     (format "  stack ghci %s"
             (combine-and-quote-strings intero-arguments))
     "

WHAT TO DO NEXT

If you fixed the problem, just kill this buffer, Intero will make
a fresh one and attempt to start the process automatically as
soon as you start editing code again.

If you are unable to fix the problem, just leave this buffer
around in Emacs and Intero will not attempt to start the process
anymore.

You can always run M-x intero-restart to make it try again.

")
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
          (let ((string (strip-carriage-returns (buffer-substring (point-min) (1- (point))))))
            (if next-callback
                (progn (with-temp-buffer
                         (funcall func state string))
                       (setq repeat t))
              (when intero-debug
                (intero--warn "Received output but no callback in `intero-callbacks': %S"
                              string)))))
        (delete-region (point-min) (point))))))

(defun strip-carriage-returns (string)
  "Strip the \\r from Windows \\r\\n line endings in STRING."
  (replace-regexp-in-string "\r" "" string))

(defun intero-get-buffer-create (worker)
  "Get or create the stack buffer for WORKER.
Uses the directory of the current buffer for context."
  (let* ((root (intero-project-root))
         (cabal-file (intero-cabal-find-file))
         (package-name (if cabal-file
                           (intero-package-name cabal-file)
                         ""))
         (buffer-name (intero-buffer-name worker))
         (default-directory (if cabal-file
                                (file-name-directory cabal-file)
                              root)))
    (with-current-buffer
        (get-buffer-create buffer-name)
      (setq intero-package-name package-name)
      (cd default-directory)
      (current-buffer))))

(defun intero-gave-up (worker)
  "Return non-nil if starting WORKER or installing intero failed."
  (and (intero-buffer-p worker)
       (let ((buffer (get-buffer (intero-buffer-name worker))))
         (buffer-local-value 'intero-give-up buffer))))

(defun intero-buffer-p (worker)
  "Return non-nil if a buffer exists for WORKER."
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
  "Get the current stack config directory.
This is either the directory where the stack.yaml is placed for
this project, or the global one if no such project-specific
config exists."
  (if intero-project-root
      intero-project-root
    (setq intero-project-root
          (with-temp-buffer
            (cl-case (save-excursion
                       (call-process "stack" nil
                                     (current-buffer)
                                     nil
                                     "path"
                                     "--project-root"
                                     "--verbosity" "silent"))
              (0 (buffer-substring (line-beginning-position) (line-end-position)))
              (t (intero--warn "Couldn't get the Stack project root.

This can be caused by a syntax error in your stack.yaml file. Check that out.

If you do not wish to use Intero for some projects, see
https://github.com/commercialhaskell/intero#whitelistingblacklisting-projects

Otherwise, please report this as a bug!

For debugging purposes, try running the following in your terminal:

stack path --project-root")
                 nil))))))

(defun intero-ghc-version ()
  "Get the GHC version used by the project."
  (with-current-buffer (intero-buffer 'backend)
    (or intero-ghc-version
        (setq intero-ghc-version
              (with-temp-buffer
                (cl-case (save-excursion
                           (call-process "stack" nil (current-buffer) t "ghc" "--" "--numeric-version"))
                  (0
                   (buffer-substring (line-beginning-position) (line-end-position)))
                  (1 nil)))))))

(defun intero-get-targets ()
  "Get all available targets."
  (with-temp-buffer
    (cl-case (call-process "stack" nil (current-buffer) t "ide" "targets")
      (0
       (cl-remove-if-not
        (lambda (line)
          (string-match "^[A-Za-z0-9-:]+$" line))
        (split-string (buffer-string) "[\r\n]" t)))
      (1 nil))))

(defun intero-package-name (&optional cabal-file)
  "Get the current package name from a nearby .cabal file.
If there is none, return an empty string.  If specified, use
CABAL-FILE rather than trying to locate one."
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
;; Multiselection

(defun intero-multiswitch (title options)
  "Displaying TITLE, read multiple flags from a list of OPTIONS.
Each option is a plist of (:key :default :title) wherein:

  :key should be something comparable with EQUAL
  :title should be a string
  :default (boolean) specifies the default checkedness"
  (let ((available-width (window-total-width)))
    (save-window-excursion
      (with-temp-buffer
        (rename-buffer (generate-new-buffer-name "multiswitch"))
        (widget-insert (concat title "\n\n"))
        (widget-insert (propertize "Hit " 'face 'font-lock-comment-face))
        (widget-create 'push-button :notify
                       (lambda (&rest ignore)
                         (exit-recursive-edit))
                       "C-c C-c")
        (widget-insert (propertize " to apply these choices.\n\n" 'face 'font-lock-comment-face))
        (let* ((me (current-buffer))
               (choices (mapcar (lambda (option)
                                  (append option (list :value (plist-get option :default))))
                                options)))
          (cl-loop for option in choices
                   do (widget-create
                       'toggle
                       :notify (lambda (widget &rest ignore)
                                 (setq choices
                                       (mapcar (lambda (choice)
                                                 (if (equal (plist-get choice :key)
                                                            (plist-get (cdr widget) :key))
                                                     (plist-put choice :value (plist-get (cdr widget) :value))
                                                   choice))
                                               choices)))
                       :on (concat "[x] " (plist-get option :title))
                       :off (concat "[ ] " (plist-get option :title))
                       :value (plist-get option :default)
                       :key (plist-get option :key)))
          (let ((lines (line-number-at-pos)))
            (select-window (split-window-below))
            (switch-to-buffer me)
            (goto-char (point-min)))
          (use-local-map
           (let ((map (copy-keymap widget-keymap)))
             (define-key map (kbd "C-c C-c") 'exit-recursive-edit)
             (define-key map (kbd "C-g") 'abort-recursive-edit)
             map))
          (widget-setup)
          (recursive-edit)
          (kill-buffer me)
          (mapcar (lambda (choice)
                    (plist-get choice :key))
                  (cl-remove-if-not (lambda (choice)
                                      (plist-get choice :value))
                                    choices)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hoogle

(defun intero-hoogle-blocking-query (query)
  "Make a request of QUERY using the local hoogle server.
If running, otherwise returns nil.

It is the responsibility of the caller to make sure the server is
running; the user might not want to start the server
automatically."
  (let ((buffer (intero-hoogle-get-buffer)))
    (when buffer
      (let ((url (intero-hoogle-url buffer query)))
        (with-current-buffer (url-retrieve-synchronously url t)
          (search-forward "\n\n" nil t 1)
          (json-read-from-string
           (buffer-substring (line-beginning-position)
                             (line-end-position))))))))

(defun intero-hoogle-url (buffer query)
  "Via hoogle server BUFFER make the HTTP URL for QUERY."
  (format "http://127.0.0.1:%d/?hoogle=%s&mode=json"
          (buffer-local-value 'intero-hoogle-port buffer)
          (url-encode-url query)))

(defun intero-hoogle-get-worker-create ()
  "Get or create the hoogle worker."
  (let* ((buffer (intero-hoogle-get-buffer-create)))
    (if (get-buffer-process buffer)
        buffer
      (intero-start-hoogle-process-in-buffer buffer))))

(defun intero-start-hoogle-process-in-buffer (buffer)
  "Start the process in BUFFER, returning BUFFER."
  (let* ((port (intero-free-port))
         (process (with-current-buffer buffer
                    (message "Booting up hoogle ...")
                    (setq intero-hoogle-port port)
                    (start-process "hoogle"
                                   buffer
                                   "stack"
                                   "hoogle"
                                   "server"
                                   "--no-setup"
                                   "--"
                                   "--local"
                                   "--port"
                                   (number-to-string port)))))
    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel process 'intero-hoogle-sentinel)
    buffer))

(defun intero-free-port ()
  "Get the next free port to use."
  (let ((proc (make-network-process
               :name "port-check"
               :family 'ipv4
               :host "127.0.0.1"
               :service t
               :server t)))
    (delete-process proc)
    (process-contact proc :service)))

(defun intero-hoogle-sentinel (process change)
  "For the hoogle PROCESS there is a CHANGE to handle."
  (message "Hoogle sentinel: %S %S" process change))

(defun intero-hoogle-get-buffer-create ()
  "Get or create the Hoogle buffer for the current stack project."
  (let* ((root (intero-project-root))
         (buffer-name (intero-hoogle-buffer-name root))
         (buf (get-buffer buffer-name))
         (default-directory root))
    (if buf
        buf
      (with-current-buffer (get-buffer-create buffer-name)
        (cd default-directory)
        (current-buffer)))))

(defun intero-hoogle-get-buffer ()
  "Get the Hoogle buffer for the current stack project."
  (let* ((root (intero-project-root))
         (buffer-name (intero-hoogle-buffer-name root)))
    (get-buffer buffer-name)))

(defun intero-hoogle-buffer-name (root)
  "For a given worker, create a buffer name using ROOT."
  (concat "*Hoogle:" root "*"))

(defun intero-hoogle-ready-p ()
  "Is hoogle ready to be started?"
  (with-temp-buffer
    (cl-case (call-process "stack" nil (current-buffer) t
                           "hoogle" "--no-setup" "--verbosity" "silent")
      (0 t))))

(defun intero-hoogle-supported-p ()
  "Is the stack hoogle command supported?"
  (with-temp-buffer
    (cl-case (call-process "stack" nil (current-buffer) t
                           "hoogle" "--help")
      (0 t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collecting information from compiler messages

(defun intero-collect-compiler-messages (msgs)
  "Collect information from compiler MSGS.

This may update in-place the MSGS objects to hint that
suggestions are available."
  (setq intero-suggestions nil)
  (let ((extension-regex (regexp-opt (intero-extensions)))
        (quoted-symbol-regex "[‘`‛]\\([^ ]+\\)['’]"))
    (cl-loop
     for msg in msgs
     do (let ((text (flycheck-error-message msg))
              (note nil))
          ;; Messages of this format:
          ;;
          ;; Can't make a derived instance of ‘Functor X’:
          ;;       You need DeriveFunctor to derive an instance for this class
          ;;       Try GeneralizedNewtypeDeriving for GHC's newtype-deriving extension
          ;;       In the newtype declaration for ‘X’
          (let ((start 0))
            (while (string-match extension-regex text start)
              (setq note t)
              (add-to-list 'intero-suggestions
                           (list :type 'add-extension
                                 :extension (match-string 0 text)))
              (setq start (min (length text) (1+ (match-end 0))))))
          ;; Messages of this format:
          ;;
          ;; Could not find module ‘Language.Haskell.TH’
          ;;     It is a member of the hidden package ‘template-haskell’.
          ;;     Use -v to see a list of the files searched for....
          (let ((start 0))
            (while (string-match "It is a member of the hidden package [‘`‛]\\([^ ]+\\)['’]" text start)
              (setq note t)
              (add-to-list 'intero-suggestions
                           (list :type 'add-package
                                 :package (match-string 1 text)))
              (setq start (min (length text) (1+ (match-end 0))))))
          ;; Messages of this format:
          ;;
          ;; Defaulting the following constraint(s) to type ‘Integer’
          ;;   (Num a0) arising from the literal ‘1’
          ;; In the expression: 2
          ;; In an equation for ‘x'’: x' = 2
          (let ((start 0))
            (while (string-match
                    " Defaulting the following constraint" text start)
              (setq note t)
              (add-to-list 'intero-suggestions
                           (list :type 'add-ghc-option
                                 :option "-fno-warn-type-defaults"))
              (setq start (min (length text) (1+ (match-end 0))))))
          ;; Messages of this format:
          ;;
          ;;     This binding for ‘x’ shadows the existing binding
          (let ((start 0))
            (while (string-match
                    " This binding for ‘\\(.*\\)’ shadows the existing binding" text start)
              (setq note t)
              (add-to-list 'intero-suggestions
                           (list :type 'add-ghc-option
                                 :option "-fno-warn-name-shadowing"))
              (setq start (min (length text) (1+ (match-end 0))))))
          ;; Messages of this format:
          ;;
          ;; The import of ‘Control.Monad’ is redundant
          ;;   except perhaps to import instances from ‘Control.Monad’
          ;; To import instances alone, use: import Control.Monad()... (intero)
          (when (string-match
                 " The \\(qualified \\)?import of[ ][‘`‛]\\([^ ]+\\)['’] is redundant"
                 text)
            (setq note t)
            (add-to-list 'intero-suggestions
                         (list :type 'remove-import
                               :module (match-string 2 text)
                               :line (flycheck-error-line msg))))
          ;; Messages of this format:
          ;;
          ;; Not in scope: ‘putStrn’
          ;; Perhaps you meant one of these:
          ;;   ‘putStr’ (imported from Prelude),
          ;;   ‘putStrLn’ (imported from Prelude)
          ;;
          ;; Or this format:
          ;;
          ;; error:
          ;;    • Variable not in scope: lopSetup :: [Statement Exp']
          ;;    • Perhaps you meant ‘loopSetup’ (line 437)
          (when (string-match
                 "[Nn]ot in scope: \\(data constructor \\|type constructor or class \\)?[‘`‛]?\\([^'’ ]+\\).*\n.*Perhaps you meant"
                 text)
            (let ((typo (match-string 2 text))
                  (start (min (length text) (1+ (match-end 0)))))
              (while (string-match quoted-symbol-regex text start)
                (setq note t)
                (add-to-list 'intero-suggestions
                             (list :type 'fix-typo
                                   :typo typo
                                   :replacement (match-string 1 text)
                                   :column (flycheck-error-column msg)
                                   :line (flycheck-error-line msg)))
                (setq start (min (length text) (1+ (match-end 0)))))))
          ;; Messages of this format:
          ;;
          ;;     Top-level binding with no type signature: main :: IO ()
          (when (string-match
                 "Top-level binding with no type signature:"
                 text)
            (let ((start (min (length text) (match-end 0))))
              (setq note t)
              (add-to-list 'intero-suggestions
                           (list :type 'add-signature
                                 :signature (mapconcat #'identity (split-string (substring text start)) " ")
                                 :line (flycheck-error-line msg)))))
          ;; Messages of this format:
          ;;
          ;;     Redundant constraints: (Arith var, Bitwise var)
          ;; Or
          ;;     Redundant constraint: Arith var
          ;; Or
          ;;     Redundant constraints: (Arith var,
          ;;                             Bitwise var,
          ;;                             Functor var,
          ;;                             Applicative var,
          ;;                             Monad var)
          (when (string-match "Redundant constraints?: " text)
            (let* ((redundant-start (match-end 0))
                   (parts (with-temp-buffer
                            (insert (substring text redundant-start))
                            (goto-char (point-min))
                            ;; A lone unparenthesized constraint might
                            ;; be multiple sexps.
                            (while (not (eq (point) (point-at-eol)))
                              (forward-sexp))
                            (let ((redundant-end (point)))
                              (search-forward-regexp ".*\n.*In the ")
                              (cons (buffer-substring (point-min) redundant-end)
                                    (buffer-substring (match-end 0) (point-max)))))))
              (setq note t)
              (add-to-list
               'intero-suggestions
               (let ((rest (cdr parts))
                     (redundant (let ((raw (car parts)))
                                  (if (eq (string-to-char raw) ?\()
                                      (substring raw 1 (1- (length raw)))
                                    raw))))
                 (list :type 'redundant-constraint
                       :redundancies (mapcar #'string-trim
                                             (intero-parse-comma-list redundant))
                       :signature (mapconcat #'identity (split-string rest) " ")
                       :line (flycheck-error-line msg))))))
          ;; Add a note if we found a suggestion to make
          (when note
            (setf (flycheck-error-message msg)
                  (concat text
                          "\n\n"
                          (propertize "(Hit `C-c C-r' in the Haskell buffer to apply suggestions)"
                                      'face 'font-lock-warning-face)))))))
  (setq intero-lighter
        (if (null intero-suggestions)
            " Intero"
          (format " Intero:%d" (length intero-suggestions)))))

(defun intero-extensions ()
  "Get extensions for the current project's GHC."
  (with-current-buffer (intero-buffer 'backend)
    (or intero-extensions
        (setq intero-extensions
              (split-string
               (shell-command-to-string
                "stack exec --verbosity silent -- ghc --supported-extensions"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto actions

(defun intero-parse-comma-list (text)
  "Parse a list of comma-separated expressions in TEXT."
  (cl-loop for tok in (split-string text "[[:space:]\n]*,[[:space:]\n]*")
           with acc = nil
           append (let* ((clist (string-to-list tok))
                         (num-open (-count (lambda (c) (or (eq c ?\() (eq c ?\[)))
                                           clist))
                         (num-close (-count (lambda (c) (or (eq c ?\)) (eq c ?\])))
                                            clist)))
                    (cond
                     ((> num-open num-close) (progn (add-to-list 'acc tok) nil))
                     ((> num-close num-open) (let ((tmp (reverse (cons tok acc))))
                                               (setq acc nil)
                                               (list (string-join tmp ", "))))
                     (t (list tok))))))

(defun intero-apply-suggestions ()
  "Prompt and apply the suggestions."
  (interactive)
  (if (null intero-suggestions)
      (message "No suggestions to apply")
    (let ((to-apply
           (intero-multiswitch
            (format "There are %d suggestions to apply:" (length intero-suggestions))
            (cl-remove-if-not
             #'identity
             (mapcar
              (lambda (suggestion)
                (cl-case (plist-get suggestion :type)
                  (add-extension
                   (list :key suggestion
                         :title (concat "Add {-# LANGUAGE "
                                        (plist-get suggestion :extension)
                                        " #-}")
                         :default t))
                  (add-ghc-option
                   (list :key suggestion
                         :title (concat "Add {-# OPTIONS_GHC "
                                        (plist-get suggestion :option)
                                        " #-}")
                         :default t))
                  (add-package
                   (list :key suggestion
                         :title (concat "Enable package: " (plist-get suggestion :package))
                         :default t))
                  (remove-import
                   (list :key suggestion
                         :title (concat "Remove: import "
                                        (plist-get suggestion :module))
                         :default t))
                  (fix-typo
                   (list :key suggestion
                         :title (concat "Replace ‘"
                                        (plist-get suggestion :typo)
                                        "’ with ‘"
                                        (plist-get suggestion :replacement)
                                        "’")
                         :default (null (cdr intero-suggestions))))
                  (add-signature
                   (list :key suggestion
                         :title (concat "Add signature: "
                                        (plist-get suggestion :signature))
                         :default t))
                  (redundant-constraint
                   (list :key suggestion
                         :title (concat
                                 "Remove redundant constraints: "
                                 (string-join (plist-get suggestion :redundancies)
                                              ", ")
                                 "\n    from the "
                                 (plist-get suggestion :signature))
                         :default nil))))
              intero-suggestions)))))
      (if (null to-apply)
          (message "No changes selected to apply.")
        (let ((sorted (sort to-apply
                            (lambda (lt gt)
                              (let ((lt-line   (or (plist-get lt :line)   0))
                                    (lt-column (or (plist-get lt :column) 0))
                                    (gt-line   (or (plist-get gt :line)   0))
                                    (gt-column (or (plist-get gt :column) 0)))
                                (or (> lt-line gt-line)
                                    (and (= lt-line gt-line)
                                         (> lt-column gt-column))))))))
          ;; # Changes unrelated to the buffer
          (cl-loop
           for suggestion in sorted
           do (cl-case (plist-get suggestion :type)
                (add-package
                 (intero-add-package (plist-get suggestion :package)))))
          ;; # Changes that do not increase/decrease line numbers
          ;;
          ;; Update in-place suggestions
          (cl-loop
           for suggestion in sorted
           do (cl-case (plist-get suggestion :type)
                (fix-typo
                 (save-excursion
                   (goto-char (point-min))
                   (forward-line (1- (plist-get suggestion :line)))
                   (move-to-column (- (plist-get suggestion :column) 1))
                   (delete-char (length (plist-get suggestion :typo)))
                   (insert (plist-get suggestion :replacement))))))
          ;; # Changes that do increase/decrease line numbers
          ;;
          ;; Remove redundant constraints
          (cl-loop
           for suggestion in sorted
           do (cl-case (plist-get suggestion :type)
                (redundant-constraint
                 (save-excursion
                   (goto-char (point-min))
                   (forward-line (1- (plist-get suggestion :line)))
                   (search-forward-regexp "[[:alnum:][:space:]\n]*=>")
                   (backward-sexp 2)
                   (let ((start (1+ (point))))
                     (forward-sexp)
                     (let* ((end (1- (point)))
                            (constraints (intero-parse-comma-list
                                          (buffer-substring start end)))
                            (nonredundant
                             (cl-loop for r in (plist-get suggestion :redundancies)
                                      with nonredundant = constraints
                                      do (setq nonredundant (delete r nonredundant))
                                      finally return nonredundant)))
                       (goto-char start)
                       (delete-char (- end start))
                       (insert (string-join nonredundant ", "))))))))

          ;; Add a type signature to a top-level binding.
          (cl-loop
           for suggestion in sorted
           do (cl-case (plist-get suggestion :type)
                (add-signature
                 (save-excursion
                   (goto-char (point-min))
                   (forward-line (1- (plist-get suggestion :line)))
                   (insert (plist-get suggestion :signature))
                   (insert "\n")))))

          ;; Remove import lines from the file. May remove more than one
          ;; line per import.
          (cl-loop
           for suggestion in sorted
           do (cl-case (plist-get suggestion :type)
                (remove-import
                 (save-excursion
                   (goto-char (point-min))
                   (forward-line (1- (plist-get suggestion :line)))
                   (delete-region (line-beginning-position)
                                  (or (when (search-forward-regexp "\n[^ \t]" nil t 1)
                                        (1- (point)))
                                      (line-end-position)))))))
          ;; Add extensions to the top of the file
          (cl-loop
           for suggestion in sorted
           do (cl-case (plist-get suggestion :type)
                (add-extension
                 (save-excursion
                   (goto-char (point-min))
                   (insert "{-# LANGUAGE "
                           (plist-get suggestion :extension)
                           " #-}\n")))
                (add-ghc-option
                 (save-excursion
                   (goto-char (point-min))
                   (insert "{-# OPTIONS_GHC "
                           (plist-get suggestion :option)
                           " #-}\n"))))))))))

(defun intero--warn (message &rest args)
  "Display a warning message made from (format MESSAGE ARGS...).
Equivalent to 'warn', but label the warning as coming from intero."
  (display-warning 'intero (apply 'format message args) :warning))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'intero)

;;; intero.el ends here
