;;; nova-corfu-popupinfo.el --- Display corfu-popupinfo in a nova frame -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Free Software Foundation, Inc.
;; Author: blueranger1981 <blueranger1981@outlook.com>
;; Maintainer: blueranger1981 <blueranger1981@outlook.com>
;; URL: https://github.com/thisisran/nova-emacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (posframe "1.4.0"))
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
;; Show a corfu-popupinfo in a nova frame

;; How to use
;; ----------
;; (nova-corfu-popupinfo-mode 1)

;;; Code:

(require 'nova)

(unless (require 'corfu-popupinfo nil t)
  (message "corfu must be installed in order to use nova-vertico"))


(defconst nova-corfu-popupinfo--buffer " *corfu-popupinfo-nova--buffer*")
(defvar nova-corfu-popupinfo--save-x-pos nil)

(defun nova-corfu-popupinfo--enable-theme ()
  "Set the colors for the buffer on (re)open."
  (with-current-buffer (get-buffer-create nova-corfu-popupinfo--buffer)
    (setq nova-background-color (face-attribute 'corfu-popupinfo :background nil t))
    (setq nova-border-color (face-attribute 'corfu-popupinfo :background nil t))))

;;;###autoload
(define-minor-mode nova-corfu-popupinfo-mode
  "A minor mode to show eldoc-box in a nova popup."
  :global t
  (if nova-corfu-popupinfo-mode
      (progn
	(corfu-popupinfo-mode 1)
	(advice-add 'corfu-popupinfo--show :around #'nova-corfu-popupinfo--show)
	(advice-add 'corfu-popupinfo--hide :around #'nova-corfu-popupinfo--hide)
	(nova-corfu-popupinfo--enable-theme))
    (advice-remove 'corfu-popupinfo--show #'nova-corfu-popupinfo--show)
    (advice-remove 'corfu-popupinfo--hide #'nova-corfu-popupinfo--hide)))

(defun nova-corfu-popupinfo--show (orig-fun &rest args)
  "Main function to show a nova frame when corfu-popupinfo is shown."
  
  (when  (and (corfu-popupinfo--visible-p corfu--frame))
    (nova-corfu-popupinfo--enable-theme)
    (apply orig-fun args)
    (let* ((parameters (frame-parameters corfu-popupinfo--frame))
	   (x (cdr (assoc 'left parameters))))
      (setq nova-corfu-popupinfo--save-x-pos x)

      ;; top-center
      (modify-frame-parameters corfu-popupinfo--frame `((left . ,(+ nova-corfu-popupinfo--save-x-pos 15)))))

      ;; side-left
      ;; (modify-frame-parameters corfu-popupinfo--frame `((left . ,(+ nova-corfu-popupinfo--save-x-pos 20)))))

    (nova-show-with-posframe nova-corfu-popupinfo--buffer
				 ""
				 'top-center
				 corfu-popupinfo--frame)))

(defun nova-corfu-popupinfo--hide (orig-fun &rest args)
  (setq nova-corfu-popupinfo--save-x-pos nil)
  (posframe-delete nova-corfu-popupinfo--buffer)
  (apply orig-fun args))

(provide 'nova-corfu-popupinfo)
;;; nova-corfu-popupinfo.el ends here
