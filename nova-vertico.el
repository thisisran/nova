;;; nova-vertico.el --- Using nova and posframe to show Vertico -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Free Software Foundation, Inc.
;; Author: blueranger1981 <blueranger1981@outlook.com>
;; Maintainer: blueranger1981 <blueranger1981@outlook.com>
;; URL: https://github.com/thisisran/nova-emacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (posframe "1.4.0") (vertico "1.1"))
;; Keywords: convenience, vertico, posframe, nova

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
;; nova-vertico uses the nova and vertico-posframe libraries to show
;; vertico in a nova posframe.

;; NOTE: nova-vertico requires Emacs 27.1

;; How to enable nova-vertico
;; --------------------------
;; (require 'nova-vertico)
;; (nova-vertico-mode 1)

;;; Code:

(require 'nova)

(unless (require 'vertico-posframe nil t)
  (message "vertico-posframe must be installed in order to use nova-vertico"))

(defconst nova-vertico--dedicated-buffer " *nova-vertico--dedicated-buffer*")
(defconst nova-vertico--save-vertico-posframe vertico-posframe-poshandler "Save the original poshandler")

(defvar nova-vertico--main-posframe nil "nova-vertico's posframe")

(defvar nova-vertico-depth-2-max-width 100 "frame's max width when depth is <= 2")
(defvar nova-vertico-deep-depth-max-width 150 "frame's max width when depth is > 2")


;;;###autoload
(define-minor-mode nova-vertico-mode
  "Display Vertico in nova instead of the minibuffer."
  
  :global t
  (when (display-graphic-p)
    (if nova-vertico-mode
	(progn
	  (nova-vertico--enable-theme)
	  (setq vertico-posframe-poshandler #'posframe-poshandler-frame-center)
	  (vertico-posframe-mode 1)
	  (advice-add 'vertico-posframe--show :around #'nova-vertico--enable)
	  (advice-add 'vertico-posframe--multiform-function :around #'nova-vertico--hide)
	  (advice-add 'vertico-posframe--minibuffer-exit-hook :around #'nova-vertico--hide)
	  (advice-add 'vertico-posframe-cleanup :around #'nova-vertico--delete)
	  (advice-add 'vertico-posframe-hidehandler :around #'nova-vertico--invisible))

      (setq vertico-posframe-poshandler nova-vertico--save-vertico-posframe)
      (advice-remove 'vertico-posframe-hidehandler #'nova-vertico--invisible)
      (advice-remove 'vertico-posframe--show #'nova-vertico--enable)
      (advice-remove 'vertico-posframe--multiform-function #'nova-vertico--hide)
      (advice-remove 'vertico-posframe--minibuffer-exit-hook #'nova-vertico--hide)
      (advice-remove 'vertico-posframe-cleanup #'nova-vertico--delete)
      (vertico-posframe-mode 0))))

(defun nova-vertico--enable-theme ()
  "Set the colors for the buffer on (re)open."
  (with-current-buffer (get-buffer-create nova-vertico--dedicated-buffer)
    (setq-local nova-background-color (face-attribute 'default :background))))

(defun nova-vertico--invisible (orig-fun &rest args)
  (advice-add 'posframe--make-frame-invisible :around #'nova-vertico--invisible-advice)
  (apply orig-fun args))

(defun nova-vertico--enable (orig-fun &rest args)
  "Capture posframe-show before calling ORIG-FUN using ARGS."
  (advice-add 'posframe-show :around #'nova-vertico--show-advice)
  (apply orig-fun args))

(defun nova-vertico--hide (orig-fun &rest args)
  "Capture posframe-hide before calling ORIG-FUN using ARGS."
  (advice-add 'posframe-hide :around #'nova-vertico--hide-advice)
  (apply orig-fun args))

(defun nova-vertico--delete (orig-fun &rest args)
  "Capture posframe-delete-frame before calling ORIG-FUN using ARGS."
  (advice-add 'posframe-delete-frame :around #'nova-vertico--delete-advice)
  (apply orig-fun args))

(defun nova-vertico--format-title (title)
  "Formats the title, truncating if necessary."

  (let* ((no-colon (if (string-suffix-p ":" title) (substring title 0 -1) title))
	 (no-paren (replace-regexp-in-string "(\\([^()]*\\))" "" no-colon))
	 (trimmed (string-trim no-paren)))
    trimmed))

(defun nova-vertico--show-advice (orig-fun &rest args)
  "Advice to create a background posframe before the original `posframe-show`."
  
  (advice-remove 'posframe-show #'nova-vertico--show-advice)
  (nova-vertico--enable-theme)

  ;; if minibuffer-depth < 2, then save the position and width of the original posframe
  ;; else, set the apply below to set the left and width according to the original posframe
  (let* ((title-raw (or (and (minibuffer-prompt) (string-trim (minibuffer-prompt))) ""))
	 (title-text (nova-vertico--format-title title-raw)))
    (if (< (minibuffer-depth) 2)
	(progn 
	  (setq nova-vertico--main-posframe
		(apply orig-fun (cons (car args) (nova--update-params (cdr args) :border-width 0 :max-width nova-vertico-depth-2-max-width))))
	  (nova-show-with-posframe nova-vertico--dedicated-buffer
				   title-text
				   'side-left
				   nova-vertico--main-posframe))
      (let* ((main-posframe nova-vertico--main-posframe)
	     (parameters (frame-parameters main-posframe))
	     (x (cdr (assoc 'left parameters)))
	     (new-width (cdr (assoc 'width parameters)))
	     (recursive-minibuffer-frame (apply orig-fun (cons (car args) (nova--update-params (cdr args)
											       :border-width 0
											       :max-width nova-vertico-depth-2-max-width)))))
	(nova-show-with-posframe nova-vertico--dedicated-buffer
				 (concat title-text (format " (depth %s)" (minibuffer-depth)))
				 'side-left
				 nova-vertico--main-posframe)
	(modify-frame-parameters recursive-minibuffer-frame `((z-group . above) (left . ,x) (width . ,new-width)))))))

(defun nova-vertico--hide-advice (orig-fun &rest args)
  (advice-remove 'posframe-hide #'nova-vertico--hide-advice)
  (when (= (minibuffer-depth) 1)
    (posframe-hide nova-vertico--dedicated-buffer))
  (apply orig-fun args))

(defun nova-vertico--invisible-advice (orig-fun &rest args)
  (advice-remove 'posframe--make-frame-invisible #'nova-vertico--invisible-advice)
  (apply orig-fun args))

(defun nova-vertico--delete-advice (orig-fun &rest args)
  (advice-remove 'posframe-delete-frame #'nova-vertico--delete-advice)
  (nova-delete-frame nova-vertico--dedicated-buffer)
  (apply orig-fun args))

(provide 'nova-vertico)
;;; nova-vertico.el ends here
