;;; intero-devel.el --- DevelMain support for Intero

;; Copyright (c) 2016 Chris Done. All rights reserved.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'intero)

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
        (intero-async-call
         ":l DevelMain"
         nil
         (lambda (_load reply)
           (if (string-match "^OK, modules loaded" reply)
               (intero-async-call
                "DevelMain.update"
                nil
                (lambda (_update reply)
                  (message "DevelMain updated. Output was:\n%s"
                           reply)))
             (message "DevelMain failed. Output was:\n%s"
                      reply)))))))

(provide 'intero-devel)
