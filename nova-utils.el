;;; nova-utils.el --- Utility functions for the nova library -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Free Software Foundation, Inc.
;; Author: blueranger1981 <blueranger1981@outlook.com>
;; Maintainer: blueranger1981 <blueranger1981@outlook.com>
;; URL: https://github.com/thisisran/nova-emacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, posframe, nova

;; This file is part of GNU Emacs.

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
;; utility functions for the nova package

;;; Code:

(defmacro nova--get-local (var name)
  "Get a buffer-local value for VAR in the NAME nova frame."
  `(buffer-local-value (quote ,var) (get-buffer ,name)))

(defmacro nova--set-local (var value name)
  "Set a buffer-local VALUE for VAR in the NAME nova frame."
  `(with-current-buffer (get-buffer ,name)
     (setq-local ,var ,value)))

(defun nova--pixels-to-char-width (pixels)
  "Convert PIXELS width to a char width."
  (nova--div-and-round pixels (frame-char-width)))

(defun nova--frame-params-relative-update (frame args)
  "Update a frame's parameters by a relative amount specificed in ARGS"
  (let ((parameters (frame-parameters frame)))
    (while args
      (let* ((curr-arg (pop args))
	     (param-name (car curr-arg))
	     (param-new-value (cdr curr-arg))
	     (param-curr-value (cdr (assoc param-name parameters)))
	     (param-combined (+ param-curr-value param-new-value)))
	(modify-frame-parameters frame `((,param-name . ,param-combined)))))))

(defun nova--div-and-round (num denom)
  "Round and divide NUM by DENOM."
  (round (/ (float num) denom)))

(cl-defun nova--get-size (name &key horizontal)
  "Get the width (HORIZONTAL is t) or height (HORIZONTAL is nil) of the frame associated with NAME."
  (let ((posframe-window (get-buffer-window name t)))
    (when (window-live-p posframe-window)
      (with-selected-window posframe-window
	(window-size posframe-window horizontal)))))

(defun nova--update-params (plist &rest args)
  "Update PLIST with multiple key-value pairs from ARGS and return the new plist."
  (while args
    (setq plist (plist-put plist (pop args) (pop args))))
  plist)

(provide 'nova-utils)
;;; nova-utils.el ends here
