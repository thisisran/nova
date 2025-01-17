;;; nova-top-center.el --- A nova style with round borders and top center title -*- lexical-binding: t; -*-
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

;; A nova style that has a round border, with the (optional)
;; title at the top center of the frame

;;; Code:

(require 'nova-common-vars)
(require 'nova-utils)

(defun nova--render-top-center (name svg-object)
  "Draw the nova title at the top center of the frame.
SVG-OBJECT: main SVG object
NAME: name representing the frame
WIDTH: width of the frame
HEIGHT: height of the frame"

  (let* ((size-position (nova--get-local nova--last-size-position name))
	 (width (aref size-position 0))
	 (height (aref size-position 1))
	 (width (* width (frame-char-width)))
	 (height (* height (frame-char-height)))

	 (title-length (length (nova--get-local nova--title name)))
	 (border-size (nova--get-local nova-border-size name))
	 (half-border-size (nova--div-and-round border-size 2))
	 (double-border-size (* border-size 2))
	 (default-font (face-attribute 'default :font))
	 (font-height (font-get default-font :size))
	 (title-top-padding (nova--div-and-round font-height 2))
	 (top-padding (if (> title-length 0) title-top-padding half-border-size))
	 (char-width-in-pixels (frame-char-width))
	 (title-width-in-pixels (* char-width-in-pixels title-length))
	 (x (nova--div-and-round width 2))
	 (y font-height)
	 (top-center-side-padding (nova--get-local nova-top-center-side-padding name))
	 (title-container-x (- x (nova--div-and-round title-width-in-pixels 2) (nova--div-and-round top-center-side-padding 2))) ; 
	 (title-container-width (+ title-width-in-pixels top-center-side-padding))
	 )

    ;; Main border
    (svg-rectangle svg-object
		   half-border-size
		   top-padding
		   (- width border-size)
		   (- height top-padding half-border-size)
		   :rx (nova--get-local nova-radius-x name)
		   :ry (nova--get-local nova-radius-y name)
		   :stroke (nova--get-local nova-border-color name)
		   :stroke-width border-size
		   :fill-color (nova--get-local nova-background-color name))

    ;; Title background
    (when (> title-length 0)
      (svg-rectangle svg-object
		     title-container-x
		     0
		     title-container-width
		     (+ title-top-padding double-border-size)
		     :fill-color (nova--get-local nova-background-color name)
		     )

      ;; Title text
      (svg-text svg-object (nova--get-local nova--title name)
		:x x
		:y y
		:fill (nova--get-local nova-title-color name)
		:text-anchor "middle"
		:dominant-baseline "central"))))

(defun nova--show-top-center (name)
  "Set up a top-center frame for NAME."
  
  (let* ((pos-frame (nova--get-local nova--wrapped-posframe name))
	 (parameters (frame-parameters pos-frame))
	 (x (cdr (assoc 'left parameters)))
	 (y (cdr (assoc 'top parameters)))
	 (width (frame-width pos-frame))
	 (height (max (frame-height pos-frame) (nova--get-local nova-min-height name)))
	 (default-font (face-attribute 'default :font))
	 (font-height (font-get default-font :size))
	 (title-length (length (nova--get-local nova--title name)))
	 (title-exists (> title-length 0))
	 (border-size (nova--get-local nova-border-size name))
	 (half-border-size (nova--div-and-round border-size 2))
	 (title-top-padding (nova--div-and-round font-height 2))
	 (top-padding (if title-exists title-top-padding half-border-size))
	 (extra-y (if title-exists border-size 0))
	 (radius-x (nova--get-local nova-radius-x name))
	 (radius-y (nova--get-local nova-radius-y name))
	 (nova-position (cons
			     (- x (nova--div-and-round radius-x 2))
			     (- y top-padding (nova--div-and-round radius-y 2) extra-y (nova--get-local nova-top-padding name)))) ; The 2 at the end is just a constant space from the title
	 (extra-width (nova--pixels-to-char-width radius-x))
	 (extra-height (nova--div-and-round (if title-exists radius-y radius-y) font-height))
	 (nova-width (+ width extra-width))
	 (nova-height (+ height extra-height (if title-exists 1 0)))
	 )

    (nova--set-local nova--last-size-position `[,nova-width ,nova-height ,(car nova-position) ,(cdr nova-position)] name)
     
    (posframe-show (get-buffer-create name)
		   :width nova-width
		   :height nova-height
		   :position nova-position
		   :nova-frame t)))

(provide 'nova-top-center)
;;; nova-top-center.el ends here
