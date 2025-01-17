;;; nova-eldoc.el --- Display eldoc in a nova frame -*- lexical-binding: t; -*-
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
;; Show a eldoc-box-hover-mode in a nova frame

;; How to use
;; ----------
;; (nova-eldoc 1)

;;; Code:

(unless (require 'eldoc-box nil t)
  (message "eldoc-box must be installed in order to use nova-eldoc"))

(defvar nova-eldoc--buffer " *nova-eldoc--buffer*")
(defvar nova-eldoc--original-markdown-code (face-attribute 'markdown-code-face :background nil t))

(defun nova-eldoc--enable-theme ()
  "Set the colors for the buffer on (re)open."
  (with-current-buffer (get-buffer-create nova-eldoc--buffer)
    (setq-local nova-title-color "white")
    (setq-local nova-top-padding 10)
    (setq-local nova-left-padding 8)
    (set-face-attribute 'markdown-code-face nil :background (nova--get-local nova-background-color nova-eldoc--buffer))))

(defun nova-eldoc--disable-theme ()
  "Unset the colors for the buffer before close."
  (set-face-attribute 'markdown-code-face nil :background nova-eldoc--original-markdown-code))

;;;###autoload
(define-minor-mode nova-eldoc-mode
  "A minor mode to show eldoc-box in a flickers popup."
  :global t
  (if nova-eldoc-mode
      (progn
	(advice-add 'eldoc-box--display :around #'nova-eldoc-show)
	(advice-add 'eldoc-box-quit-frame :around #'nova-eldoc-hide)
	(nova-eldoc--enable-theme))
    (advice-remove 'eldoc-box--display #'nova-eldoc-show)
    (advice-remove 'eldoc-box-quit-frame #'nova-eldoc-hide)
    (nova-eldoc--disable-theme)))

(defun nova-eldoc-show (orig-fun &rest args)

  (nova-eldoc--enable-theme)

  (apply orig-fun args)

  (modify-frame-parameters eldoc-box--frame `((border-width . 0) (internal-border-width . 0)))
  (nova--frame-params-relative-update eldoc-box--frame '((left . -100) (top . 50)))
  (nova-show-with-posframe nova-eldoc--buffer
				"Doc"
				'top-center
				eldoc-box--frame))

(defun nova-eldoc-hide (orig-fun &rest args)
  (posframe-delete nova-eldoc--buffer)
  (nova-eldoc--disable-theme)
  (apply orig-fun args))


(provide 'nova-eldoc)
;;; nova-eldoc.el ends here
