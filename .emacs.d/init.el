;; init.el --- initialization file

;; Copyright (C) 2020 Finn Sauer <finn@finnsauer.xyz>
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
  (require 'package)

  ;; Specify the package archives-- where to download from.
  (setq package-archives '(("MELPA" . "https://melpa.org/packages/")
                           ("GNU"   . "https://elpa.gnu.org/packages/")
                           ("ORG" . "https://orgmode.org/elpa/")))

  ;; Initialize the packages, avoiding a re-initialization.
  (unless (bound-and-true-p package--initialized)
    (package-initialize))

  (unless (package-installed-p 'leaf)
    (package-install 'leaf)))

;; Uses a different file for `custom' in order to not clutter the
;; `init.el' file with automatic settings made by custom.
(setq custom-file
      (expand-file-name
       (concat user-emacs-directory "custom.el")))
(load custom-file t t)

;; This is the actual configuration file.  This will load `config.el'
;; when it is available (I rebuild this file each time I quit Emacs).
;; If it is not available, then use `org-babel-tangle-file'
(let ((f (expand-file-name "config.el" user-emacs-directory))
      (o (expand-file-name "config.org" user-emacs-directory)))
  (require 'leaf)
  (if (file-readable-p f)
      (load-file f)
    (org-babel-load-file o f)))

;;; init.el ends here
