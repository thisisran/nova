;;; nova.el --- A library to show SVG frames with posframe as a backend -*- lexical-binding: t; -*-
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

;; nova is a wrapper around `posframe` that makes
;; creating and customizing posframes easy and visually appealing.

;; The package provides two preconfigured styles out of the box for
;; immediate use. These styles are designed to look modern and polished,
;; suitable for various contexts such as completion UIs or transient
;; menus.
;;
;; Developers can also use nova to build their own
;; custom styles. To create a new design, you implement 2 functions:
;; nova--render-style-name, and nova--show-style-name,
;; where render paints the frame, and show calculates the
;; correct size and position of the frame.
;; Look at the current implementations for an example.

;; NOTE: nova requires Emacs 27.1

;; Getting started
;; ---------------

;; Call nova-show-with-posframe (if you already have a posframe
;; you want to wrap), or nova-show to create a posframe
;; wrapped in a nova frame

;; nova-update: recreate the SVG frame (in case the underlying
;; posframe has changed)

;; Customization
;; -------------

;; Set nova-background-color, nova-border-color,
;; nova-border-size (recommended to use 1 as a starting point),
;; nova-title-color, and nova-title-background-color to your
;; preferred colors.

;; If something doesn't look right, consider tweaking
;; (all located at nova-common-vars.el) nova-radius-x,
;; nova-radius-y, nova-min-height, nova-left-padding,
;; nova-top-padding, nova-extra-height,
;; and nova-top-center-side-padding to make it look
;; right on your screen.

;;; Code:

(require 'svg)
(require 'posframe)
(require 'nova-utils)
(require 'nova-common-vars)

;; Supported Styles
(require 'nova-side-left)
(require 'nova-top-center)

(defconst nova--show-func-prefix "nova--show")
(defconst nova--render-func-prefix "nova--render")


(defun nova--call-func-by-style (name func-prefix &rest args)
  (let* ((style-func (intern (format "%s-%s" func-prefix (nova--get-local nova--style name))))
	 (func-args (if args (cons name args) (list name))))
	 (apply style-func func-args)))


(defun nova--posframe-to-svg (name)
  "When called from a posframe, turn it into an svg frame.
NAME: see nova-show-with-posframe"
  
  (let ((posframe-window (get-buffer-window name t))
	w
	h)
    
    (when (window-live-p posframe-window)
      (with-selected-window posframe-window
	(setq w (* (frame-char-width) (nova--get-size name :horizontal t)))
	(setq h (* (frame-char-height) (nova--get-size name))))

      (let ((main-svg (svg-create w h)))

	(nova--call-func-by-style name nova--render-func-prefix main-svg)

	;; Finally, draw the svg to the screen
	(with-current-buffer (nova--get-local nova--name name)
	  (svg-insert-image main-svg)
	  (goto-char (point-min)))))))


(defun nova-show (name title style pos-name &rest posframe-args)
  "Creates a posframe with POS-NAME buffer name and POSFRAME-ARGS,
and passes that to nova-show-with-posframe with NAME TITLE and STYLE."

  (nova-show-with-posframe name
			   title
			   style
			   (apply #'posframe-show pos-name posframe-args)))


(defun nova-show-with-posframe (name title style pos-frame)
  "Open a nova frame, and returns it.

NAME: A unique name for the nova buffer.

buffer name can prefix with space, for example \" *mybuffer*\", so
the buffer name will hide for ibuffer and `list-buffers'

TITLE: title of the frame
STYLE: which style to use (side-left or top-center) to draw the title
POS-FRAME: a posframe to build the nova UI around."

  (when (display-graphic-p)

    (with-current-buffer (get-buffer-create name)
      (setq-local nova--name name)
      (setq-local nova--title (or title ""))
      (setq-local nova--style style)
      (setq-local nova--wrapped-posframe pos-frame)
      (erase-buffer))

    (modify-frame-parameters pos-frame '((z-group . above)))

    (let* ((save-nova-frame (nova--call-func-by-style name nova--show-func-prefix)))

      (push `(,save-nova-frame . ,pos-frame) nova--frame-list)
      
      (with-current-buffer name
	(setq-local nova--frame save-nova-frame))

      (nova--posframe-to-svg name)
      save-nova-frame)))


(defun nova-update (name)
  "Regenerates NAME's frame's SVG."
  (with-current-buffer (get-buffer name)
    (erase-buffer))

  (nova--call-func-by-style name nova--show-func-prefix)
  (nova--posframe-to-svg name))


(defun nova--is-frame-visible (name)
  "Check if a nova frame (associated with NAME) exists and is visible."
  (let ((buffer (get-buffer name)))
    (and buffer
         (buffer-live-p buffer)
         (posframe-workable-p)
         (frame-live-p (buffer-local-value 'posframe--frame buffer))
         (frame-visible-p (buffer-local-value 'posframe--frame buffer)))))


(defun nova-delete-frame (name)
  "Delete the nova frame associated with NAME."
  (interactive)
  (when (nova--is-frame-visible name)
    (posframe-delete name)))


;; TODO: must be a better way to do it
;; probably using some frame parameter
(defun nova-delete-all ()
  "Delete all nova frames (and the their contained posframes)."
  (interactive)
  (dolist (nova-frame-value nova--frame-list)
    (let ((nova-frame (car nova-frame-value))
	  (nova-posframe (cdr nova-frame-value)))
      (delete-frame nova-frame)
      (delete-frame nova-posframe)))
  (setq nova--frame-list nil))

(provide 'nova)
;;; nova.el ends here
