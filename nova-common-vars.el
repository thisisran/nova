;;; nova-common-vars.el --- common variables shared between each nova module -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Free Software Foundation, Inc.
;; Author: blueranger1981 <blueranger1981@outlook.com>
;; Maintainer: blueranger1981 <blueranger1981@outlook.com>
;; URL: https://github.com/thisisran/nova-emacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (posframe "1.4.0"))
;; Keywords: convenience, posframe, nova

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defvar nova--frame-list nil "Keep track of all the nova (and associated posframes) frames.")

(defvar-local nova--frame nil "The frame representing nova")
(defvar-local nova--wrapped-posframe nil "The frame of the wrapped posframe")
(defvar-local nova--name nil "Name that represents the nova frame.")
(defvar-local nova--title nil "Title for the nova frame.")
(defvar-local nova--style nil "The frame drawing style.")
(defvar-local nova--last-size-position nil "Save last size and position of the nova frame.")

(defvar-local nova-background-color (face-attribute 'default :background) "Background color for the nova frame.")
(defvar-local nova-border-color "#3d5a80" "Border color for the nova frame.")
(defvar-local nova-border-size 1 "Border size for the frame.")
(defvar-local nova-title-color "#161c28" "Color of the title in the frame.") ; #e3b7bb
(defvar-local nova-title-background-color "#6DB9EF" "Background color of sidebar titles.")
(defvar-local nova-radius-x 18 "X radius for the rounds corners of the frame.")
(defvar-local nova-radius-y 18 "Y radius for the rounds corners of the frame.")
(defvar-local nova-min-height 10 "Minimum height (in lines)")
(defvar-local nova-left-padding 15 "Left padding to use when wrapping a posframe.")
(defvar-local nova-top-padding 0 "Top padding to use when wrapping a posframe.")
(defvar-local nova-extra-height 2 "Extra height to add to the frame.")

(provide 'nova-common-vars)
;;; nova-common-vars.el ends here
