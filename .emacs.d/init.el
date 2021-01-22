;; init.el --- initialization file

;; Copyright (C) 2020-2021 Finn Sauer <finn@finnsauer.xyz>
;;
;; This file is part of my `config.org' ecosystem.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code :

(eval-and-compile
  ;; This is for deferring the check for modifications mechanism to
  ;; `check-on-save' instead of `find-at-startup'-- which as the name
  ;; implies runs a command at startup.  The command is quite slow.
  (setq straight-check-for-modifications '(check-on-save))

  ;; Bootstrap code for `straight.el'.
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  ;; Install `leaf' and `leaf-keywords.'
  (unless (and (ignore-errors (require 'leaf))
               (ignore-errors (require 'leaf-keywords))
               (ignore-errors (require 'org)))
    (straight-use-package 'leaf)
    (straight-use-package 'leaf-keywords)
    (straight-use-package 'org))

  ;; Uses a different file for `custom' in order to not clutter the
  ;; `init.el' file with automatic settings made by custom.
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file 'noerror 'nomessage)

  ;; This is the actual configuration file.  This will load `config.el'
  ;; when it is available (I rebuild this file each time I quit Emacs).
  ;; If it is not available, then use `org-babel-tangle-file'.
  (let ((f (expand-file-name "config.elc" user-emacs-directory))
        (o (expand-file-name "config.org" user-emacs-directory)))
    (leaf-keywords-init)
    (if (file-readable-p f)
        (load-file f)
      (org-babel-load-file o))))

;;; init.el ends here
