;;; python-reindent.el --- Reindent Python Code
;;
;; Copyright (c) 2013 Moogen Tian
;;
;; Author: Moogen Tian <ibluefocus@NOSPAM.gmail.com>
;; Homepage: http://blog.galeo.me
;; url: https://gist.github.com/galeo/5465488
;; Version: 0.0.2
;; Created: Apr 25 2013
;; Keywords: python, reindent, format
;;
;;; This file is NOT part of GNU Emacs
;;
;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;; Installation:
;;
;; Fist, install Python Reindent package (Which is released to
;; the public domain, by Tim Peters, 03 October 2000.) with pip:
;;
;;     pip install -U Reindent
;;
;; Then, copy python-reindent.el to your load-path and add to your ~/.emacs
;;
;;     (require 'python-reindent)
;;
;; Three commands are supplied to reindent python code:
;;
;;     `python-reindent-region'
;;     `python-reindent-file'
;;     `python-reindent-directory'
;;
;; Run them interactively:
;;
;;     M-x python-reindent-* RET
;;
;; Or set any key bindings you like.
;;

;;; Code:


(defun revert-buffer-keep-undo (&rest -)
  "Revert buffer but keep undo history."
  (interactive)
  (let ((inhibit-read-only t))
    (clear-visited-file-modtime)
    (erase-buffer)
    (insert-file-contents (buffer-file-name))
    (set-visited-file-modtime)
    (set-buffer-modified-p nil)))

(defun revert-python-buffers ()
  "Refresh all opened buffers of python files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name)
                 (string-match "\\.\\(py\\|pyw\\)$"
                               (file-name-nondirectory (buffer-file-name)))
                 (not (buffer-modified-p)))
        (revert-buffer-keep-undo t t t))))
  (message "Refreshed opened python files."))

(defcustom python-reindent-command "reindent -v"
  "Command used to reindent a python file.

Change Python (.py) files to use 4-space indents and no hard tab characters.
Also trim excess spaces and tabs from ends of lines, and remove empty lines
at the end of files.  Also ensure the last line ends with a newline.

One or more file and/or directory paths can be passed as the arguments
of the command, reindent overwrites files in place. If backups are
required, it will rename the originals with a .bak extension.

If it finds nothing to change, the file is left alone.

If reindent does change a file, the changed file is a fixed-point for
future runs (i.e., running reindent on the resulting .py file won't
change it again)."
  :type 'string
  :group 'python)

(defun python-reindent-directory (dir &optional backup-p recurse-p)
  "Search and reindent .py files in a directory.

If BACKUP-P is set to non-nil, backup files with .bak extension will
be generated.

All .py files within the directory will be examined, and, if RECURSE-P
is set to non-nil, subdirectories will be recursively searched.

Check `python-reindent-command' for what the indentation action will do."
  (interactive
   (let ((directory-name
          (ido-read-directory-name "Reindent directory: "))
         (recurse (y-or-n-p "Search recursively for all .py files?"))
         (backup (y-or-n-p "Before reindentation, backup the files?")))
     (list directory-name backup recurse)))
  (save-some-buffers (not compilation-ask-about-save) nil)
  (shell-command (concat python-reindent-command " "
                         (if (not backup-p)
                             "--nobackup ")
                         (if recurse-p
                             "--recurse ")
                         dir))
  (revert-python-buffers)
  (message
   (concat "Reindent files done!"
           (if backup-p
               " Backup files with '.bak' extension generated."))))

(defun python-reindent-file (file &optional backup-p)
  "Reindent a file(by default the one opened in current buffer).

If BACKUP-P is set to non-nil, a backup file with .bak extension will
be generated.

Check `python-reindent-command' for what the indentation action will do."
  (interactive
   (let* ((file-name
           (ido-read-file-name
            "Reindent file: " nil
            (file-name-nondirectory (buffer-file-name))))
          (backup (y-or-n-p "Before reindentation, backup the file?")))
     (list file-name backup)))
  (save-some-buffers (not compilation-ask-about-save) nil)
  (shell-command (concat python-reindent-command " "
                         (if (not backup-p)
                             "--nobackup ")
                         file))
  (revert-python-buffers)
  (message
   (concat "Reindent file done!"
           (if backup-p
               " A .bak file has been generated."))))

(defun python-reindent-region (beg end)
  "Reindent the code of the region or the buffer if no region selected."
  (interactive
   (if (or (null transient-mark-mode)
           mark-active)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (let ((output (with-output-to-string
                  (shell-command-on-region
                   beg end
                   python-reindent-command
                   standard-output nil)))
        (error-buffer "*Shell Output Error*"))
    (if (and output
             (not (string-match "^Traceback\\|^\\w+Error:" output)))
        ;; no error
        (progn
          (goto-char beg)
          (kill-region beg end)
          (insert output)
          (message "Code has been reindented!"))
      (progn
        (set-buffer (get-buffer-create error-buffer))
        (insert output)
        (display-buffer error-buffer)
        (message "Error occurred, please check!")))))


(provide 'python-reindent)

;;; python-reindent.el ends here
