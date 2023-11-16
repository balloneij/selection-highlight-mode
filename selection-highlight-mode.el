;;; selection-highlight-mode.el --- Auto highlights matches to the current active region -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Isaac Ballone

;; Author: Isaac Ballone <isaac@ballone.dev>
;; URL: https://github.com/balloneij/selection-highlight-mode
;; Keywords: matching
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides a global minor mode that automatically highlights exact matches
;; to the active region. Similar to VS Code's Selection Highlight.

;;; Code:

;;; Settings

(defgroup selection-highlight nil
  "Auto highlights matches to the current active region."
  :group 'convenience)

(defcustom selection-highlight-mode-min-length 2
  "The minimum length of selection before highlighting matches."
  :group 'selection-highlight
  :type 'natnum)

;;; State

(defvar selection-highlight-mode--active-search nil)
(defvar selection-highlight-mode--window-states nil)

;;; Faces

(defface selection-highlight-mode-match-face
  '((t :inherit region))
  "Face for highlighting matches to the active region.
See `selection-highlight-mode-alternate-match-face' for the other face."
  :group 'selection-highlight)

(defface selection-highlight-mode-alternate-match-face
  '((t :inherit region :weight bold))
  "Alternate face for highlighting adjacent matches to the active region.
See `selection-highlight-mode-match-face' for the other face."
  :group 'selection-highlight)

;;; Helpers

(defmacro selection-highlight-mode--without-case-fold-search (&rest body)
  "Temporarily disable `case-fold-search' for the current buffer and execute BODY."
  (declare (indent 0))
  `(let ((old-case-fold-search case-fold-search))
     (setq case-fold-search nil)
     (unwind-protect
         (progn ,@body)
       (setq case-fold-search old-case-fold-search))))

(defun selection-highlight-mode--face-at-point (pos)
  "If the face at POS is a match, return the face used."
  (let (face-found)
    (dolist (overlay (overlays-at pos))
      (when (and (not face-found)
                 (eq (overlay-get overlay 'category) 'selection-highlight-mode-match))
        (setq face-found (overlay-get overlay 'face))))
    face-found))

(defun selection-highlight-mode--zebra-stripe-face (beg adjacent?)
  "Determine the face to use at point BEG.
Alternate the face used when ADJACENT?."
  (if (and adjacent?
           (eq 'selection-highlight-mode-match-face
               (selection-highlight-mode--face-at-point (1- beg))))
      'selection-highlight-mode-alternate-match-face
    'selection-highlight-mode-match-face))

(defun selection-highlight-mode--create-overlay (beg end adjacent?)
  "Create an overlay from BEG to END.
ADJACENT? must be true if the last match ends where this overlay begins."
  (let ((overlay (make-overlay beg end))
        (face (selection-highlight-mode--zebra-stripe-face beg adjacent?)))
    (overlay-put overlay 'category 'selection-highlight-mode-match)
    (overlay-put overlay 'face face)
    overlay))

(defun selection-highlight-mode--highlight-next-match (str bound)
  "Find and highlight next match of STR, limited by BOUND.
Returns overlay or nil."
  (let* ((start-pos (point))
         (end (search-forward str bound t 1)))
    (when end
      (let* ((beg (- end (length str)))
             (adjacent? (= beg start-pos)))
        (if (and (region-active-p) (= beg (region-beginning)))
            ;; recur
            (selection-highlight-mode--highlight-next-match str bound)
          (selection-highlight-mode--create-overlay beg end adjacent?))))))

(defun selection-highlight-mode--highlight-matches (str start end)
  "Highlight all matches of STR from START to END.
Returns the new overlays"
  (selection-highlight-mode--without-case-fold-search
    (save-excursion
      (goto-char start)
      (let (overlays
            (searching? t))
        (while searching?
          (if-let ((new-overlay (selection-highlight-mode--highlight-next-match str end)))
              (push new-overlay overlays)
            (setq searching? nil)))
        overlays))))

(defun selection-highlight-mode--save-window-state (window start end overlays)
  "Save START, END, and OVERLAYS to `selection-highlight-mode--window-states'.
Keys off WINDOW."
  (setq selection-highlight-mode--window-states
        (assq-delete-all window selection-highlight-mode--window-states))
  (push `(,window . ((start . ,start)
                     (end . ,end)
                     (overlays . ,overlays)))
        selection-highlight-mode--window-states))

(defun selection-highlight-mode--refresh-window-p (window new-start new-end)
  "Determine if the NEW-START and NEW-END of WINDOW warrant re-highlighting."
  (let* ((state (alist-get window selection-highlight-mode--window-states))
         (start (alist-get 'start state))
         (end (alist-get 'end state)))
    (or (not start) (not end)
        (< new-start start)
        (> new-end end))))

(defun selection-highlight-mode--window-overlays (window)
  "Get overlays used in WINDOW from `selection-highlight-mode--window-states'."
  (alist-get 'overlays (alist-get window selection-highlight-mode--window-states)))

(defun selection-highlight-mode--clear-window (window)
  "Delete overlays used in WINDOW."
  (dolist (overlay (selection-highlight-mode--window-overlays window))
    (delete-overlay overlay)))

(defun selection-highlight-mode--clear-all-windows ()
  "Delete all overlays in all windows."
  (dolist (window (mapcar 'car selection-highlight-mode--window-states))
    (selection-highlight-mode--clear-window window))
  (setq selection-highlight-mode--window-states '()))

(defun selection-highlight-mode--highlight-window (window str)
  "Highlight matches to STR in WINDOW."
  (let ((start (window-start window))
        (end (window-end window t)))
    (when (selection-highlight-mode--refresh-window-p window start end)
      (selection-highlight-mode--clear-window window)
      (with-current-buffer (window-buffer window)
        (let ((matches (selection-highlight-mode--highlight-matches str start end)))
          (selection-highlight-mode--save-window-state window start end matches))))))

(defun selection-highlight-mode-get-selection ()
  "Get the active selection string or nil."
  (when (region-active-p)
    (let* ((beg (region-beginning))
           (block-cursor? (and (fboundp 'evil-visual-state-p)
                               (evil-visual-state-p)))
           (end (+ (region-end) (if block-cursor? 1 0))))
      (when (>= (abs (- end beg)) selection-highlight-mode-min-length)
        (buffer-substring beg end)))))

;;; Hooks

;;;###autoload
(defun selection-highlight-mode-window-scroll-hook (window _start)
  "Re-highlight WINDOW to account for change to the visible buffer."
  (when selection-highlight-mode--active-search
    (selection-highlight-mode--highlight-window window selection-highlight-mode--active-search)))

;;;###autoload
(defun selection-highlight-mode-post-command-hook ()
  "Highlight all live windows to match the current selection."
  (if-let ((selection (selection-highlight-mode-get-selection)))
      (when (not (eq selection-highlight-mode--active-search selection))
        (setq selection-highlight-mode--active-search selection)
        (selection-highlight-mode--clear-all-windows)
        (walk-windows (lambda (window)
                        (selection-highlight-mode--highlight-window window selection))))
    (when selection-highlight-mode--active-search
      (setq selection-highlight-mode--active-search nil)
      (selection-highlight-mode--clear-all-windows))))

;;; Modes

;;;###autoload
(define-minor-mode selection-highlight-mode
  "Automatically highlight matches to the current selection in active windows."
  :init-value nil
  :global t
  (if selection-highlight-mode
      ;; on
      (progn
        (add-hook 'window-scroll-functions 'selection-highlight-mode-window-scroll-hook)
        (add-hook 'post-command-hook 'selection-highlight-mode-post-command-hook))
    ;; off
    (remove-hook 'window-scroll-functions 'selection-highlight-mode-window-scroll-hook)
    (remove-hook 'post-command-hook 'selection-highlight-mode-post-command-hook)
    (setq selection-highlight-mode--active-search nil)
    (selection-highlight-mode--clear-all-windows)))

(provide 'selection-highlight-mode)

;;; selection-highlight-mode.el ends here
