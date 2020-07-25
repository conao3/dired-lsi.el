;;; dired-memo.el --- Add memo to directory and show it in dired  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Version: 0.0.1
;; Keywords: convenience
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/conao3/dired-memo.el

;; This program is free software: you can redistribute it and/or modify
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

;; Add memo to directory and show it in dired.


;;; Code:

(require 'cl-lib)
(require 'dired)

(defgroup dired-memo nil
  "Add memo to directory and show it in dired."
  :group 'convenience
  :link '(url-link :tag "Github" "https://github.com/conao3/dired-memo.el"))

(defface dired-memo-default-face
  '((t (:inherit font-lock-warning-face)))
  "Default face."
  :group 'dired-memo)

(defvar dired-memo-mode)

(defun dired-memo--add-overlay (pos string)
  "Add overlay to display STRING at POS."
  (let ((ov (make-overlay (1- pos) pos)))
    (overlay-put ov 'dired-memo-overlay t)
    (overlay-put ov 'after-string string)))

(defun dired-memo--overlays-in (beg end)
  "Get all dired-memo overlays between BEG to END."
  (cl-remove-if-not
   (lambda (ov)
     (overlay-get ov 'dired-memo-overlay))
   (overlays-in beg end)))

(defun dired-memo--remove-all-overlays ()
  "Remove all `dired-memo' overlays."
  (save-restriction
    (widen)
    (mapc #'delete-overlay
          (dired-memo--overlays-in (point-min) (point-max)))))

(defun dired-memo--refresh ()
  "Display the icons of files in a dired buffer."
  (dired-memo--remove-all-overlays)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (dired-move-to-filename nil)
        (let ((item (dired-get-filename 'relative 'noerror)))
          (when item
            (let* ((file (expand-file-name ".description.lsi" item))
                   (desc (when (file-readable-p file)
                           (with-temp-buffer
                             (insert-file-contents file)
                             (buffer-string))))
                   (desc* (when desc (propertize desc 'face 'dired-memo-default-face))))
              (when desc
                (end-of-line)
                (dired-memo--add-overlay
                 (point) (concat " " desc*)))))))
      (forward-line 1))))

(defun dired-memo--refresh-advice (fn &rest args)
  "Advice function for FN with ARGS."
  (apply fn args)
  (when dired-memo-mode
    (dired-memo--refresh)))

(defvar dired-memo-advice-alist
  '((dired-readin                . dired-memo--refresh-advice)
    (dired-revert                . dired-memo--refresh-advice)
    (dired-do-create-files       . dired-memo--refresh-advice)
    (dired-do-kill-lines         . dired-memo--refresh-advice)
    (dired-insert-subdir         . dired-memo--refresh-advice)
    (dired-create-directory      . dired-memo--refresh-advice)
    (dired-internal-do-deletions . dired-memo--refresh-advice)
    (dired-narrow--internal      . dired-memo--refresh-advice))
  "Alist of advice and advice functions.")

(defun dired-memo--setup ()
  "Setup `dired-memo'."
  (when (derived-mode-p 'dired-mode)
    (setq-local tab-width 1)
    (pcase-dolist (`(,sym . ,fn) dired-memo-advice-alist)
      (advice-add sym :around fn))
    (dired-memo--refresh)))

(defun dired-memo--teardown ()
  "Functions used as advice when redisplaying buffer."
  (pcase-dolist (`(,sym . ,fn) dired-memo-advice-alist)
    (advice-remove sym fn))
  (dired-memo--remove-all-overlays))

;;;###autoload
(define-minor-mode dired-memo-mode
  "Display all-the-icons icon for each files in a dired buffer."
  :lighter " dired-memo"
  (when (and (derived-mode-p 'dired-mode) (display-graphic-p))
    (if dired-memo-mode
        (dired-memo--setup)
      (dired-memo--teardown))))

(provide 'dired-memo)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; dired-memo.el ends here
