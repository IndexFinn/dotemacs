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

(setq frame-inhibit-implied-resize t)

;;; early-init.el ends here
