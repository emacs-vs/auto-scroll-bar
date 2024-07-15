;;; auto-scroll-bar.el --- Automatically show/hide scroll-bars as needed  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Shen, Jen-Chieh
;; Created date 2022-03-01 03:32:33

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-vs/auto-scroll-bar
;; Version: 0.1.1
;; Package-Requires: ((emacs "29.1") (elenv "0.1.0"))
;; Keywords: convenience scrollbar

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
;; Automatically show/hide scroll-bars as needed.
;;

;;; Code:

(require 'cl-lib)
(require 'scroll-bar)

(require 'elenv)

(defgroup auto-scroll-bar nil
  "Automatically show/hide scroll-bars as needed."
  :prefix "auto-scroll-bar-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/emacs-vs/auto-scroll-bar"))

(defcustom auto-scroll-bar-disabled-buffers
  nil
  "List of buffers to disable the scroll bar completely."
  :type 'list
  :group 'auto-scroll-bar)

(defcustom auto-scroll-bar-disabled-major-modes
  nil
  "List of major-mode to disable the scroll bar completely."
  :type 'list
  :group 'auto-scroll-bar)

(defcustom auto-scroll-bar-hide-minibuffer t
  "Non-nil to hide scrollbar in minibuffer."
  :type 'boolean
  :group 'auto-scroll-bar)

(defcustom auto-scroll-bar-horizontal t
  "Set to non-nil to auto show/hide horizontal scroll-bar."
  :type 'boolean
  :group 'auto-scroll-bar)

;;
;; (@* "Util" )
;;

(defmacro auto-scroll-bar--ensure-frame (win &rest body)
  "Run BODY only when WIN is valid."
  (declare (indent 1) (debug t))
  `(let ((after-delete-frame-functions)
         (after-focus-change-function))
     (when-let* (((and (windowp ,win) (window-live-p ,win)))
                 (frame (window-frame ,win))
                 ((frame-live-p frame)))
       ,@body)))

(defun auto-scroll-bar--str-width (str)
  "Calculate STR in pixel width."
  (let ((width (window-font-width))
        (len (string-pixel-width str)))
    (+ (/ len width)
       (if (zerop (% len width)) 0 1))))  ; add one if exceeed

(defun auto-scroll-bar--bol-at-pos (pos)
  "Return the line beginning position at POS."
  (save-excursion (goto-char pos) (line-beginning-position)))

(defun auto-scroll-bar--eol-at-pos (pos)
  "Return the line end position at POS."
  (save-excursion (goto-char pos) (line-end-position)))

;;
;; (@* "Core" )
;;

(defun auto-scroll-bar--point-all-p (wstart wend)
  "Return non-nil when point WSTART and WEND are with in the visible range."
  (let ((pmin   (point-min))
        (pmax   (point-max))
        (vstart (auto-scroll-bar--bol-at-pos wstart))
        (vend   (auto-scroll-bar--eol-at-pos wend)))
    (cond ((frame-parameter nil 'company-box)
           (and (<= vstart pmin)
                (<= (1- pmax) vend)))
          (t
           (and (<= vstart pmin)
                (<= pmax vend))))))

(defun auto-scroll-bar--show-v-p (wstart wend)
  "Return non-nil if we should show the vertical scroll-bar.

Argument WSTART and WEND is for fast access cache."
  (and vertical-scroll-bar
       (not (auto-scroll-bar--point-all-p wstart wend))))

(defun auto-scroll-bar--show-h-p (wstart wend)
  "Return non-nil if we should show the horizontal scroll-bar.

Argument WSTART and WEND is for fast access cache."
  (and horizontal-scroll-bar
       truncate-lines
       (or
        ;; (1) When window not align to the left!
        (let ((w-hscroll (max (- (window-hscroll) hscroll-step) 0)))
          (and (not (zerop w-hscroll))
               (<= w-hscroll (current-column))))
        ;; (2) When at least one line exceeds the current window width
        (when-let* ((buf (ignore-errors (buffer-substring-no-properties wstart wend)))
                    (win-w (window-max-chars-per-line))
                    (buf-width (auto-scroll-bar--str-width buf)))
          (< win-w buf-width)))))

(defun auto-scroll-bar--disabled-p ()
  "Return non-nil if scroll-bars should be ignored."
  (or (member (buffer-name) auto-scroll-bar-disabled-buffers)
      (member major-mode auto-scroll-bar-disabled-major-modes)))

(defun auto-scroll-bar--toggle-p (win show-v show-h)
  "Return non-nil if we should call function `set-window-scroll-bars'.

See function `auto-scroll-bar--update' description for arguments WIN, SHOW-V,
and SHOW-H."
  (let* ((bars (window-scroll-bars win))
         (shown-v (nth 2 bars))
         (shown-h (nth 5 bars)))
    (or (not (eq shown-v show-v)) (not (eq shown-h show-h)))))

(defun auto-scroll-bar--update (win show-v show-h &optional persistent)
  "Update scrollbar WIN, SHOW-V, SHOW-H, PERSISTENT."
  (when (auto-scroll-bar--toggle-p win show-v show-h)
    (set-window-scroll-bars win nil show-v nil show-h persistent)))

(defun auto-scroll-bar--show-hide (win)
  "Show/Hide scroll-bar for WIN."
  (auto-scroll-bar--ensure-frame win
    (cond ((equal (minibuffer-window) win)
           (auto-scroll-bar--hide-minibuffer))
          (t
           (with-selected-window win
             (if (auto-scroll-bar--disabled-p)
                 (auto-scroll-bar--update win nil nil)
               (let* ((wend   (window-end nil t))
                      (wstart (window-start))
                      (show-v (auto-scroll-bar--show-v-p wstart wend))
                      (show-h (auto-scroll-bar--show-h-p wstart wend)))
                 (auto-scroll-bar--update win show-v show-h))))))))

(defun auto-scroll-bar--hide-buffer (buffer-or-name)
  "Hide scroll bar in BUFFER-OR-NAME."
  (when-let ((windows (get-buffer-window-list buffer-or-name)))
    (dolist (win windows)
      (auto-scroll-bar--update win nil nil t))))

(defun auto-scroll-bar--hide-minibuffer (&optional frame)
  "Hide minibuffer when variable `auto-scroll-bar-hide-minibuffer' is enabled.

Optional argument FRAME is used to select frame's minibuffer."
  (when auto-scroll-bar-hide-minibuffer
    (auto-scroll-bar--update (minibuffer-window frame) nil nil t)
    (auto-scroll-bar--hide-buffer " *Echo Area 0*")
    (auto-scroll-bar--hide-buffer " *Echo Area 1*")))

(defun auto-scroll-bar--size-change (&optional frame &rest _)
  "Show/Hide all visible windows in FRAME."
  (elenv-with-no-redisplay
    (dolist (win (window-list frame))
      (auto-scroll-bar--show-hide win))))

(defun auto-scroll-bar--scroll (&optional window &rest _)
  "Show/Hide scroll-bar on WINDOW."
  (elenv-with-no-redisplay
    (auto-scroll-bar--show-hide window)))

;; XXX: Only for horizontal scroll.
;;
;; The hook `window-scroll-functions' doesn't get called on horizontal scroll.
(defun auto-scroll-bar--post-command (&rest _)
  "Hook for post command."
  (elenv-with-no-redisplay
    (auto-scroll-bar--show-hide (selected-window))))

(defun auto-scroll-bar--enable ()
  "Enable function `auto-scroll-bar-mode'."
  (cond ((display-graphic-p)
         (add-hook 'window-size-change-functions #'auto-scroll-bar--size-change 90)
         (add-hook 'window-scroll-functions #'auto-scroll-bar--scroll 90)
         (add-hook 'post-command-hook #'auto-scroll-bar--post-command 90)
         (toggle-scroll-bar 1)
         (when auto-scroll-bar-horizontal (toggle-horizontal-scroll-bar 1))
         (auto-scroll-bar--size-change))  ; execute once
        (t (auto-scroll-bar-mode -1))))

(defun auto-scroll-bar--disable ()
  "Disable function `auto-scroll-bar-mode'."
  (remove-hook 'window-size-change-functions #'auto-scroll-bar--size-change)
  (remove-hook 'window-scroll-functions #'auto-scroll-bar--scroll)
  (remove-hook 'post-command-hook #'auto-scroll-bar--post-command)
  (toggle-scroll-bar -1)
  (toggle-horizontal-scroll-bar -1))

;;;###autoload
(define-minor-mode auto-scroll-bar-mode
  "Minor mode `auto-scroll-bar-mode'."
  :global t
  :require 'auto-scroll-bar-mode
  :group 'auto-scroll-bar
  (if auto-scroll-bar-mode (auto-scroll-bar--enable) (auto-scroll-bar--disable)))

(provide 'auto-scroll-bar)
;;; auto-scroll-bar.el ends here
