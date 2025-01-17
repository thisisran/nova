;;; nova-side-left.el --- A nova style with round borders and a left rotated title -*- lexical-binding: t; -*-
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

;; A nova style that has a round border, with a title
;; showing on the left side of the frame

;;; Code:

(require 'nova-common-vars)
(require 'nova-utils)

(defconst nova-sidebar-shape-width 15 "Give the sidebar some width") ; The 15 here is just to give the sidebar shape a little width
(defconst nova-sidebar-width-factor 1.333 "a factor to calculate the sidebar width based on the radius") 

(defun nova--render-side-left (name svg-object)
  "Draw the nova title on the left of the frame, as a sidebar.
SVG-OBJECT: main SVG object
WIDTH: width of the frame
HEIGHT: height of the frame"
  (let* ((width (aref (nova--get-local nova--last-size-position name) 0))
	 (height (aref (nova--get-local nova--last-size-position name) 1))
	 (width (* width (frame-char-width)))
	 (height (* height (frame-char-height)))

	 (char-width-in-pixels (frame-char-width))
	 (radius-x (nova--get-local nova-radius-x name))
	 (radius-y (nova--get-local nova-radius-y name))
	 (background-color (nova--get-local nova-background-color name))
	 (border-size (nova--get-local nova-border-size name))
	 (double-border-size (* border-size 2))
	 (quad-border-size (* double-border-size 2))
	 (outer-height (- height quad-border-size))
	 (sidebar-top (* border-size 3))
	 (sidebar-width (* char-width-in-pixels nova-sidebar-shape-width))
	 (sidebar-height (- outer-height double-border-size))
	 (title (nova--get-local nova--title name))
	 (title-length (length title))
	 (title-width-in-pixels (* char-width-in-pixels title-length))
	 (title-y-pos (nova--div-and-round (+ outer-height title-width-in-pixels) 2))
	 (background-x (round (* radius-y nova-sidebar-width-factor))) ; give the sidebar a width of 33% more than the radius to give it some width
	 )

    ;; Main border
    (svg-rectangle svg-object
		   border-size
		   double-border-size
		   (- width double-border-size)
		   outer-height
		   :rx radius-x
		   :ry radius-y
		   :stroke (nova--get-local nova-border-color name)
		   :stroke-width border-size
		   :fill-color background-color)

    ;; Sidebar
    (svg-rectangle svg-object
		   double-border-size
		   sidebar-top
		   sidebar-width
		   sidebar-height
		   :rx radius-x
		   :ry radius-y
		   :fill-color (nova--get-local nova-title-background-color name))

    ;; Sidebar title
    (svg-text svg-object
	      title
	      :x 0
	      :y 0
	      :font-weight "bold"
	      :transform (format "translate (%s, %s) rotate(270)"
				 radius-y
				 title-y-pos)
	      :fill-color (nova--get-local nova-title-color name))

    ;; Sidebar background
    (svg-rectangle svg-object
		   background-x
		   sidebar-top
		   sidebar-width
		   sidebar-height
		   :fill-color background-color)))


(defun nova--show-side-left (name)
  "Set up a side-left frame for NAME."

  (let* ((pos-frame (nova--get-local nova--wrapped-posframe name))
	 (width (frame-width pos-frame))
	 (height (max (frame-height pos-frame) (nova--get-local nova-min-height name)))
	 
	 (parameters (frame-parameters pos-frame))
	 (x (cdr (assoc 'left parameters)))
         (y (cdr (assoc 'top parameters)))
	 (left-padding (nova--get-local nova-left-padding name))
	 (x-radius (nova--get-local nova-radius-x name))
	 (sidebar-width (+ (round (* x-radius nova-sidebar-width-factor)) left-padding))
	 (nova-height (+ height (nova--get-local nova-extra-height name)))
	 (half-radius (nova--div-and-round x-radius 2))
	 (nova-position (cons
			     (- x sidebar-width)
			     (- y half-radius (nova--get-local nova-border-size name))))
	 (extra-width (nova--pixels-to-char-width sidebar-width))
	 (nova-width (+ width extra-width (nova--pixels-to-char-width left-padding)))
	 )

    (nova--set-local nova--last-size-position `[,nova-width ,nova-height ,(car nova-position) ,(cdr nova-position)] name)

    (posframe-show (get-buffer-create name)
		   :width nova-width
		   :height nova-height
		   :position nova-position)))

(provide 'nova-side-left)
;;; nova-side-left.el ends here
