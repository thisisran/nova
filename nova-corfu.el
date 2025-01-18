;;; nova-corfu.el --- Wrap the corfu frame inside of nova  -*- lexical-binding: t -*-
;; Copyright (C) 2025 Free Software Foundation, Inc.
;; Author: blueranger1981 <blueranger1981@outlook.com>
;; Maintainer: blueranger1981 <blueranger1981@outlook.com>
;; URL: https://github.com/thisisran/nova-emacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (posframe "1.4.0") (corfu))
;; Keywords: convenience, corfu, posframe, nova

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
;; nova-vertico uses the nova and vertico-posframe libraries to show
;; vertico in a nova posframe.

;; NOTE: nova-vertico requires Emacs 27.1

;; How to enable nova-vertico
;; --------------------------
;; (require 'nova-corfu)
;; (nova-corfu-mode 1)

;;; Code:

(require 'nova)
(require 'nova-utils)
(require 'posframe)
(unless (require 'corfu nil t)
  (message "corfu must be installed in order to use nova-corfu"))

(defvar nova-corfu--original-corfu-border (face-attribute 'corfu-border :background nil t))
(defvar nova-corfu--original-corfu-default (face-attribute 'corfu-default :background nil t))
(defvar nova-corfu--original-corfu-current (face-attribute 'corfu-current :background nil t))
(defvar nova-corfu--original-orderless-match-background (face-attribute 'orderless-match-face-0 :background nil t))
(defvar nova-corfu--original-orderless-match-foreground (face-attribute 'orderless-match-face-0 :foreground nil t))

(defvar nova-corfu-top 10 "Adjust the top of the corfu frame by this amount")
(defvar nova-corfu-border-color "#292E41" "Color for the corfu border")
(defconst nova-corfu--buffer " *nova-corfu--buffer*")

(defun nova-corfu-mode-workable-p ()
  "Indicate whether the mode is on or off."
  nova-corfu-mode)

(defun nova-corfu--enable-theme ()
  "Set the colors for the buffer on (re)open."
  
  (with-current-buffer (get-buffer-create nova-corfu--buffer)

    (setq-local nova-left-padding 8)
    (setq-local nova-background-color "#292E41") ; (face-attrkbute 'corfu-default :background nkl t))
    (setq-local nova-border-size 1)
    (setq-local nova-border-color "#292E41")
    (setq-local nova-title-color "#A999DB")
    (setq-local nova-title-background-color "#383852")
    
    (set-face-attribute 'corfu-border nil :background "#292E41") ; (face-attribute 'default :background nil t))
    (set-face-attribute 'corfu-current nil :background "#212534" :weight 'bold) ; (face-attribute 'default :background nil t) :weight 'bold)
    (set-face-attribute 'corfu-default nil :background "#292E41") ; (face-attribute 'default :background nil t))
    (set-face-attribute 'orderless-match-face-0 nil :background (face-attribute 'default :background nil t) :foreground "#B29CE9")))

(defun nova-corfu--disable-theme ()
  "Unset the colors for the buffer before close."
  
  (set-face-attribute 'corfu-border nil :background nova-corfu--original-corfu-border)
  (set-face-attribute 'corfu-default nil :background nova-corfu--original-corfu-default)
  (set-face-attribute 'corfu-current nil :background nova-corfu--original-corfu-current)
  (set-face-attribute 'orderless-match-face-0 nil
		      :background nova-corfu--original-orderless-match-background
		      :foreground nova-corfu--original-orderless-match-foreground))

;;;###autoload
(define-minor-mode nova-corfu-mode
  "Display corfu completions in a nova frame."
  :global t
  (if nova-corfu-mode
      (progn
	(corfu-mode 1)
	(nova-corfu--enable-theme))
    (nova-corfu--disable-theme)))

(cl-defmethod corfu--popup-show :after (_pos _off _width _lines &context ((nova-corfu-mode-workable-p) (eql t)) &optional _curr _lo _bar)
  "Called after corfu pops up a menu to display it with a nova frame."

  (nova-corfu--enable-theme)

  (nova--frame-params-relative-update corfu--frame `((top . ,nova-corfu-top)))
  (nova-show-with-posframe nova-corfu--buffer
			   ""
			   'top-center
			   corfu--frame))

(cl-defmethod corfu--popup-hide :before (&context ((nova-corfu-mode-workable-p) (eql t)))
  "Called after corfu hides its popup to hide the nova frame as well."

  (when (get-buffer nova-corfu--buffer)
    (posframe-hide nova-corfu--buffer)
    (kill-buffer nova-corfu--buffer)))

(provide 'nova-corfu)
;;; nova-corfu.el ends here
