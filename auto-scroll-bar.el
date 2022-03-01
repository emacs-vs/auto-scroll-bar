;;; auto-scroll-bar.el --- Automatically show/hide scroll-bar  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh
;; Created date 2022-03-01 03:32:33

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Automatically show/hide scroll-bar.
;; Keyword: scrollbar
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/jcs-elpa/auto-scroll-bar

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Automatically show/hide scroll-bar.
;;

;;; Code:

(require 'scroll-bar)

(defgroup auto-scroll-bar nil
  "Automatically show/hide scroll-bar."
  :prefix "auto-scroll-bar-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/auto-scroll-bar"))

(defcustom auto-scroll-bar-disabled-buffers '()
  "List of buffers to disable the scroll bar completely."
  :type 'list
  :group 'auto-scroll-bar)

(defcustom auto-scroll-bar-hide-minibuffer t
  "List of buffers to disable the scroll bar completely."
  :type 'boolean
  :group 'auto-scroll-bar)

(defun auto-scroll-bar--show-v-p ()
  "Return non-nil if we should show the vertical scroll-bar."
  (and scroll-bar-mode
       (not (string= (format-mode-line mode-line-percent-position) "All"))))

(defun auto-scroll-bar--show-h-p ()
  "Return non-nil if we should show the horizontal scroll-bar."
  (and horizontal-scroll-bar-mode))

(defun auto-scroll-bar--update (win show-v show-h)
  "Update scrollbar WIN, SHOW-V, SHOW-H."
  (let* ((bars (window-scroll-bars win))
         (shown-v (nth 2 bars))
         (shown-h (nth 5 bars)))
    (when (or (not (eq shown-v show-v)) (not (eq shown-h show-h)))
      (set-window-scroll-bars win nil show-v nil show-h t))))

(defun auto-scroll-bar--show-hide (win)
  "Show/Hide scroll-bar for WIN."
  (with-selected-window win
    (if (member (buffer-name) auto-scroll-bar-disabled-buffers)
        (auto-scroll-bar--update win nil nil)
      (let ((show-v (auto-scroll-bar--show-v-p))
            (show-h (auto-scroll-bar--show-h-p)))
        (auto-scroll-bar--update win show-v show-h)))))

(defun auto-scroll-bar--change (&rest _)
  "Window state change."
  (dolist (win (window-list))
    (auto-scroll-bar--show-hide win)))

(defun auto-scroll-bar--enable ()
  "Enable function `auto-scroll-bar-mode'."
  (add-hook 'post-command-hook #'auto-scroll-bar--change)
  (when auto-scroll-bar-hide-minibuffer
    (auto-scroll-bar--update (minibuffer-window) nil nil)))

(defun auto-scroll-bar--disable ()
  "Disable function `auto-scroll-bar-mode'."
  (remove-hook 'post-command-hook #'auto-scroll-bar--change))

;;;###autoload
(define-minor-mode auto-scroll-bar-mode
  "Minor mode 'auto-scroll-bar-mode'."
  :global t
  :require 'auto-scroll-bar-mode
  :group 'auto-scroll-bar
  (if auto-scroll-bar-mode (auto-scroll-bar--enable) (auto-scroll-bar--disable)))

(provide 'auto-scroll-bar)
;;; auto-scroll-bar.el ends here
