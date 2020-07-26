;;; dired-lsi.el --- Add memo to directory and show it in dired  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Version: 0.0.1
;; Keywords: convenience
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/conao3/dired-lsi.el

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
(require 'subr-x)

(defgroup dired-lsi nil
  "Add memo to directory and show it in dired."
  :group 'convenience
  :link '(url-link :tag "Github" "https://github.com/conao3/dired-lsi.el"))

(defcustom dired-lsi-separator " / "
  "Separetor between item and description."
  :group 'dired-lsi
  :type 'string)

(defface dired-lsi-default-face
  '((t (:inherit font-lock-warning-face)))
  "Default face."
  :group 'dired-lsi)

(defvar dired-lsi-mode)

(defun dired-lsi--add-overlay (pos string)
  "Add overlay to display STRING at POS."
  (let ((ov (make-overlay (1- pos) pos)))
    (overlay-put ov 'dired-lsi-overlay t)
    (overlay-put ov 'after-string string)))

(defun dired-lsi--overlays-in (beg end)
  "Get all dired-lsi overlays between BEG to END."
  (cl-remove-if-not
   (lambda (ov)
     (overlay-get ov 'dired-lsi-overlay))
   (overlays-in beg end)))

(defun dired-lsi--remove-all-overlays ()
  "Remove all `dired-lsi' overlays."
  (save-restriction
    (widen)
    (mapc #'delete-overlay
          (dired-lsi--overlays-in (point-min) (point-max)))))

(defun dired-lsi--refresh ()
  "Display the icons of files in a dired buffer."
  (dired-lsi--remove-all-overlays)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (dired-move-to-filename nil)
        (when-let* ((item (dired-get-filename 'relative 'noerror))
                    (file (expand-file-name ".description.lsi" item))
                    (desc (when (file-readable-p file)
                            (with-temp-buffer
                              (insert-file-contents file)
                              (string-trim-right (buffer-string)))))
                    (desc* (propertize desc 'face 'dired-lsi-default-face)))
          (unless (string-empty-p desc*)
            (end-of-line)
            (dired-lsi--add-overlay
             (point) (concat dired-lsi-separator desc*)))))
      (forward-line 1))))

(defun dired-lsi--refresh-advice (fn &rest args)
  "Advice function for FN with ARGS."
  (apply fn args)
  (when dired-lsi-mode
    (dired-lsi--refresh)))

(defvar dired-lsi-advice-alist
  '((dired-readin                . dired-lsi--refresh-advice)
    (dired-revert                . dired-lsi--refresh-advice)
    (dired-do-create-files       . dired-lsi--refresh-advice)
    (dired-do-kill-lines         . dired-lsi--refresh-advice)
    (dired-insert-subdir         . dired-lsi--refresh-advice)
    (dired-create-directory      . dired-lsi--refresh-advice)
    (dired-internal-do-deletions . dired-lsi--refresh-advice)
    (dired-narrow--internal      . dired-lsi--refresh-advice))
  "Alist of advice and advice functions.")

(defun dired-lsi--setup ()
  "Setup `dired-lsi'."
  (when (derived-mode-p 'dired-mode)
    (setq-local tab-width 1)
    (pcase-dolist (`(,sym . ,fn) dired-lsi-advice-alist)
      (advice-add sym :around fn))
    (dired-lsi--refresh)))

(defun dired-lsi--teardown ()
  "Functions used as advice when redisplaying buffer."
  (pcase-dolist (`(,sym . ,fn) dired-lsi-advice-alist)
    (advice-remove sym fn))
  (dired-lsi--remove-all-overlays))

;;;###autoload
(define-minor-mode dired-lsi-mode
  "Display all-the-icons icon for each files in a dired buffer."
  :lighter " dired-lsi"
  (when (and (derived-mode-p 'dired-mode) (display-graphic-p))
    (if dired-lsi-mode
        (dired-lsi--setup)
      (dired-lsi--teardown))))

(provide 'dired-lsi)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; dired-lsi.el ends here
