;; early-init.el --- init.el just earlier

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

;;; Code:

;; Slow down the Garbage Collection, effectively stopping it.  This
;; setting will be overwritten by the function `index/gc-reset' inside
;; the `config.org' file.
(setq gc-cons-threshold most-positive-fixnum)

;; Disable GUI Elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq use-file-dialog nil)
(setq use-dialog-box t)
(setq inhibit-splash-screen t)

;; Found on `https://github.com/miklos1/dotemacs/blob/master/early-init.el'.
;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

;; This will prevent resizing with fonts.
(setq frame-inhibit-implied-resize t)

;;; early-init.el ends here
