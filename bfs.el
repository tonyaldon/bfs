;;; bfs.el --- Browse File System -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Tony Aldon

;; Author: Tony Aldon <tony.aldon.adm@gmail.com>
;; Version: 0.10.0
;; Package-Requires: ((emacs "27.1") (dash "2.17.0") (f "0.20.0"))
;; Keywords: files
;; Homepage: https://github.com/tonyaldon/bfs

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; TODO:

;;; Code:

(require 'dash)
(require 'dired)
(require 'f)
(require 'ls-lisp)

;;; User options

(defgroup bfs nil "Browsing File System." :group 'files)

(defface bfs-directory
  '((t (:inherit dired-directory)))
  "Face used for subdirectories."
  :group 'bfs)

(defface bfs-file
  '((t (:inherit default)))
  "Face used for files."
  :group 'bfs)

(defface bfs-top-parent-directory
  '((t (:inherit dired-header)))
  "Face used for parent directory path in `bfs-top-buffer-name' buffer."
  :group 'bfs)

(defface bfs-top-child-entry
  '((t (:inherit bfs-file :weight ultra-bold)))
  "Face used for child entry in `bfs-top-buffer-name' buffer."
  :group 'bfs)

(defface bfs-top-symlink-name
  '((t (:inherit dired-symlink)))
  "Face of symlink name in `bfs-top-buffer-name'."
  :group 'bfs)

(defface bfs-top-symlink-arrow
  '((t (:inherit dired-symlink)))
  "Face of the arrow link used for symlinks in `bfs-top-buffer-name'."
  :group 'bfs)

(defface bfs-top-symlink-directory-target
  '((t (:inherit bfs-directory)))
  "Face of symlink target when it is a directory in `bfs-top-buffer-name'."
  :group 'bfs)

(defface bfs-top-symlink-file-target
  '((t (:inherit bfs-file)))
  "Face of symlink target when it is a file in `bfs-top-buffer-name'."
  :group 'bfs)

(defface bfs-top-broken-symlink
  (if (>= emacs-major-version 28)
      '((t (:inherit dired-broken-symlink)))
    '((t (:inherit error))))
  "Face of broken links used in `bfs-top-buffer-name'."
  :group 'bfs)

(defvar bfs-top-mode-line-background
  (face-background 'mode-line-inactive nil t)
  "Background color of `bfs-top-buffer-name' mode line.
You can change the value with any hexa color.  For instance, if you
want the background to be white, set `bfs-top-mode-line-background'
to \"#ffffff\".")

(defvar bfs-top-mode-line-foreground
  (face-foreground 'mode-line-inactive nil t)
  "Foreground color of `bfs-top-buffer-name' mode line.
You can change the value with any hexa color.  For instance, if you
want the foreground to be black, set `bfs-top-mode-line-background'
to \"#000000\".")

(defvar bfs-top-mode-line-format
  `((:eval (format "%s" (bfs-top-mode-line))))
  "The mode line format used in `bfs-top-buffer-name'.
See `bfs-top-mode-line'.

And see `mode-line-format' if you want to customize
`bfs-top-mode-line-format'.")

(defvar bfs-top-line-function 'bfs-top-line-ellipsed
  "Function that return the formated text used in `bfs-top-buffer-name'.
This function takes one argument CHILD (a file path corresponding
to the current child entry) and return the formatted string obtained
from CHILD.

See `bfs-top-line-ellipsed', `bfs-top-line-default', `bfs-child'.")

(defvar bfs-kill-buffer-eagerly nil
  "When t, kill opened buffer upon a new child entry file is previewed.
When nil, opened buffers are killed when leaving `bfs' environment.")

(defvar bfs-ignored-extensions '("mkv" "iso" "mp4" "jpg" "png")
  "Don't preview files with those extensions.")

(defvar bfs-max-size large-file-warning-threshold
  "Don't preview files larger than this size.")

(defvar bfs-ls-function 'bfs-ls
  "Function of one argument DIR (a file path) that
return a list of filename (not file path) contained in DIR.
\".\" or \"..\" must always be omitted.
This is the function we use to fill `bfs-child-buffer-name' and
`bfs-child-buffer-name' buffers with filenames.
See `bfs-ls'.")

;;; Movements

(defvar bfs-visited-backward nil
  "List of child files that have been visited.
Child files are
added uniquely to `bfs-visited-backward' only when we use
`bfs-backward' command.  This allow `bfs-forward' to be smart.")

(defun bfs-get-visited-backward (child)
  "Return the last file in `bfs-visited-backward'.
This directory name of this file must match CHILD.
Return nil if there is no matches."
  (--first (f-equal-p child (f-dirname it)) bfs-visited-backward))

(defun bfs-update-visited-backward (child)
  "Add CHILD to `bfs-visited-backward' conditionally."
  (unless (or (and (file-directory-p child)
                   (not (file-accessible-directory-p child)))
              (not (bfs-valid-child-p child)))
    (setq bfs-visited-backward
          (cons child
                (--remove (f-equal-p (f-dirname child) (f-dirname it))
                          bfs-visited-backward)))))

(defun bfs-previous ()
  "Preview previous file."
  (interactive)
  (unless (bobp) (forward-line -1))
  (bfs-preview (bfs-child)))

(defun bfs-next ()
  "Preview next file."
  (interactive)
  (unless (= (line-number-at-pos) (1- (line-number-at-pos (point-max))))
    (forward-line))
  (bfs-preview (bfs-child)))

(defun bfs-backward ()
  "Update `bfs' environment making parent entry the child entry.
In other words, go up by one node in the file system tree."
  (interactive)
  (unless (f-root-p default-directory)
    (bfs-update-visited-backward (bfs-child))
    (bfs-update default-directory)))

(defun bfs-forward ()
  "Update `bfs' environment making `bfs-child-entry' the parent entry.
In other words, go down by one node in the file system tree.

If `bfs-child' is a readable file, leave `bfs' and visit that file.
If `bfs-child' is an empty directory, leave `bfs' and visit that file."
  (interactive)
  (let* ((child (bfs-child)))
    (cond ((and (file-directory-p child)
                (not (file-accessible-directory-p child)))
           (message "Permission denied: %s" child))
          ((file-directory-p child)
           (let ((visited (bfs-get-visited-backward child)))
             (cond (visited (bfs-update visited))
                   ((funcall bfs-ls-function child)
                    (bfs-update
                     (f-join child (car (funcall bfs-ls-function child)))))
                   (t (bfs-clean)
                      (delete-other-windows)
                      (dired child)))))
          ((bfs-broken-symlink-p child)
           (message "Symlink is broken: %s" child))
          ((f-file-p child)
           (let (child-buffer)
             (condition-case err
                 (setq child-buffer (find-file-noselect (file-truename child)))
               (file-error (message "%s" (error-message-string err))))
             (when child-buffer
               (bfs-clean)
               (delete-other-windows)
               (find-file (file-truename child))))))))

;;; Scrolling

(defun bfs-half-window-height ()
  "Compute half window height."
  (/ (window-body-height) 2))

(defun bfs-scroll-preview-down-half-window ()
  "Scroll preview window down of half window height."
  (interactive)
  (scroll-other-window-down (bfs-half-window-height)))

(defun bfs-scroll-preview-up-half-window ()
  "Scroll preview window up of half window height."
  (interactive)
  (scroll-other-window (bfs-half-window-height)))

(defun bfs-scroll-down-half-window ()
  "Scroll child window down of half window height."
  (interactive)
  (scroll-down (bfs-half-window-height))
  (bfs-preview (bfs-child)))

(defun bfs-scroll-up-half-window ()
  "Scroll child window up of half window height."
  (interactive)
  (scroll-up (bfs-half-window-height))
  (if (eobp) (bfs-previous)
    (bfs-preview (bfs-child))))

(defun bfs-beginning-of-buffer ()
  "Move to beginning of buffer."
  (interactive)
  (call-interactively 'beginning-of-buffer)
  (bfs-preview (bfs-child)))

(defun bfs-end-of-buffer ()
  "Move to beginning of buffer."
  (interactive)
  (call-interactively 'end-of-buffer)
  (if (eobp) (bfs-previous)
    (bfs-preview (bfs-child))))

;;; Find a file, isearch

(defun bfs-isearch-preview-update ()
  "Update the preview window with the current child entry file.

Intended to be added to `isearch-update-post-hook' and
`isearch-mode-end-hook'.  This allows to preview the file the
cursor has moved to using \"isearch\" commands in
`bfs-child-buffer-name' buffer."
  (when (string= (buffer-name) bfs-child-buffer-name)
    (bfs-preview (bfs-child))))

(defun bfs-find-file (file)
  "Find a FILE with your completion framework and update `bfs' environment."
  (interactive
   (list (read-file-name "Find file:" nil default-directory t)))
  (if (and (file-directory-p file)
           (bfs-first-valid-child file))
      (bfs-update (bfs-first-valid-child file))
    (bfs-update file)))

;;; bfs modes

;;;; Keymaps

(defvar bfs-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map (kbd "p") 'bfs-previous)
    (define-key map (kbd "n") 'bfs-next)
    (define-key map (kbd "b") 'bfs-backward)
    (define-key map (kbd "f") 'bfs-forward)
    (define-key map (kbd "RET") 'bfs-forward)

    (define-key map (kbd "<backspace>") 'bfs-scroll-preview-down-half-window)
    (define-key map (kbd "<SPC>") 'bfs-scroll-preview-up-half-window)
    (define-key map (kbd "C-<backspace>") 'bfs-scroll-down-half-window)
    (define-key map (kbd "C-<SPC>") 'bfs-scroll-up-half-window)
    (define-key map (kbd "<") 'bfs-beginning-of-buffer)
    (define-key map (kbd ">") 'bfs-end-of-buffer)

    (define-key map (kbd "C-f") 'bfs-find-file)

    (define-key map (kbd "D") (lambda () (interactive)
                                (let ((dir default-directory))
                                  (delete-other-windows)
                                  (bfs-clean)
                                  (dired dir))))
    (define-key map (kbd "T") (lambda () (interactive) (ansi-term "/bin/bash")))

    (define-key map (kbd "q") 'bfs-quit)
    map)
  "Keymap for `bfs-mode' used in `bfs-child-buffer-name' buffer.")

(defvar bfs-parent-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'bfs-quit)
    map)
  "Keymap for `bfs-parent-mode' used in `bfs-parent-buffer-name' buffer.")

;;;; Highlight line in child and parent buffers

(defvar-local bfs-line-overlay nil
  "Overlay used by `bfs-mode' mode to highlight the current line.")

(defun bfs-line-make-overlay ()
  "Make the overlay used in `bfs-mode'."
  (let ((ol (make-overlay (point) (point))))
    (overlay-put ol 'priority -50)
    ol))

(defun bfs-line-move-overlay (overlay)
  "Move `bfs-line-overlay' to the line including the point by OVERLAY."
  (move-overlay
   overlay (line-beginning-position) (line-beginning-position 2)))

(defun bfs-line-highlight ()
  "Activate overlay on the current line."
  (unless bfs-line-overlay
    (setq bfs-line-overlay (bfs-line-make-overlay)))
  (let ((background-dir (or (face-background 'bfs-directory nil t)
                            (face-background 'default nil t)))
        (foreground-dir (or (face-foreground 'bfs-directory nil t)
                            (face-foreground 'default nil t)))
        (background-file (or (face-background 'bfs-file nil t)
                             (face-background 'default nil t)))
        (foreground-file (or (face-foreground 'bfs-file nil t)
                             (face-foreground 'default nil t)))
        face)
    (cond ((or (equal (buffer-name (current-buffer))
                      bfs-parent-buffer-name)
               (file-directory-p (bfs-child)))
           (setq face `(:background ,foreground-dir
                        :foreground ,background-dir
                        :weight ultra-bold
                        :extend t)))
          (t (setq face `(:background ,foreground-file
                          :foreground ,background-file
                          :weight ultra-bold
                          :extend t))))
    (overlay-put bfs-line-overlay 'face face))
  (overlay-put bfs-line-overlay 'window nil)
  (bfs-line-move-overlay bfs-line-overlay))

;;;; bfs-top-mode

(defun bfs-top-mode-line (&optional child)
  "Return the string that describe CHILD file.
This string is used in the mode line of `bfs-top-buffer-name' buffer.
If CHILD is nil, default to `bfs-child'."
  (let ((file (or child (bfs-child))))
    (with-temp-buffer
      (insert-directory file "-lh")
      (delete-char -1) ; delete the last newline character
      (goto-char (point-min))
      (dired-goto-next-file)
      (delete-region (point) (point-at-eol))
      (concat " " (buffer-substring-no-properties (point-min) (point-max))))))

(defun bfs-top-mode ()
  "Mode use in `bfs-top-buffer-name' buffer.
See `bfs-top-buffer'."
  (interactive)
  (kill-all-local-variables)
  (setq-local cursor-type nil)
  (setq-local global-hl-line-mode nil)

  (setq mode-line-format bfs-top-mode-line-format)
  (face-remap-add-relative 'mode-line-inactive
                           :background bfs-top-mode-line-background)
  (face-remap-add-relative 'mode-line-inactive
                           :foreground bfs-top-mode-line-foreground)
  (face-remap-add-relative 'mode-line
                           :background bfs-top-mode-line-background)
  (face-remap-add-relative 'mode-line
                           :foreground bfs-top-mode-line-foreground)

  (setq major-mode 'bfs-top-mode)
  (setq mode-name "bfs-top")
  (setq buffer-read-only t))

;;;; bfs-preview-mode

(defun bfs-preview-mode ()
  "Mode use in `bfs-preview-buffer-name'."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'bfs-preview-mode)
  (setq mode-name "bfs-preview")
  (visual-line-mode t)
  (setq buffer-read-only t))

;;;; bfs-parent-mode

(defun bfs-parent-mode (&optional parent)
  "Mode used in `bfs-parent-buffer-name' buffer.
In `bfs-parent-mode', `default-directory' is set to PARENT, and
must be the parent directory of the file listed in
`bfs-parent-buffer-name' buffer.
See `bfs-parent-buffer' command."
  (interactive)
  (kill-all-local-variables)
  (setq default-directory (or parent default-directory))
  (setq-local cursor-type nil)
  (setq-local global-hl-line-mode nil)
  (bfs-line-highlight)
  (add-hook 'post-command-hook #'bfs-line-highlight nil t)
  (use-local-map bfs-parent-mode-map)
  (setq major-mode 'bfs-parent-mode)
  (setq mode-name "bfs-parent")
  (setq buffer-read-only t))

;;;; bfs-mode

(defun bfs-mode (&optional parent)
  "Mode used in `bfs-child-buffer-name' buffer.
In `bfs-mode', `default-directory' is set to PARENT, and
must be the parent directory of the file listed in
`bfs-child-buffer-name' buffer.
See `bfs-child-buffer' command."
  (interactive)
  (kill-all-local-variables)
  (setq default-directory (or parent default-directory))
  (setq-local cursor-type nil)
  (setq-local global-hl-line-mode nil)
  (bfs-line-highlight)
  (add-hook 'post-command-hook #'bfs-line-highlight nil t)
  (use-local-map bfs-mode-map)
  (setq major-mode 'bfs-mode)
  (setq mode-name "bfs")
  (setq buffer-read-only t))

;;; Utilities

(defun bfs-child ()
  "Return file path corresponding to the current child entry.
If `bfs-child-buffer-name' isn't lived return nil."
  (when (buffer-live-p (get-buffer bfs-child-buffer-name))
    (with-current-buffer bfs-child-buffer-name
      (f-join default-directory (bfs-child-entry)))))

(defun bfs-child-entry ()
  "Return the current child entry."
  (with-current-buffer bfs-child-buffer-name
    (buffer-substring-no-properties (point-at-bol) (point-at-eol))))

(defun bfs-parent-entry ()
  "Return the current parent entry."
  (with-current-buffer bfs-child-buffer-name
    (f-filename default-directory)))

(defun bfs-goto-entry (entry)
  "Move the cursor to the line ENTRY."
  (goto-char (point-min))
  (search-forward-regexp (concat "^" entry) nil t)
  (beginning-of-line))

(defun bfs-valid-child-p (child)
  "Return t if CHILD (file path) can be a child in `bfs' environment."
  (cond ((not (f-exists-p child))
         (message "File doesn't exist: %s" child)
         nil)
        ((f-root-p child)
         (message "Root can't be a bfs child: %s" child)
         nil)
        (t t)))

(defun bfs-first-valid-child (dir)
  "Return the first file of DIR directory satisfaying `bfs-valid-child-p'.

Return nil if DIR isn't accesible.  See `file-accessible-directory-p'.
Return nil if none are found.
Return an empty string if DIR directory is empty."
  (when (file-accessible-directory-p dir)
    (--first (bfs-valid-child-p it)
             (--map (f-join dir it) (funcall bfs-ls-function dir)))))

(defun bfs-child-default (buffer)
  "Return the file name of BUFFER.
Return `default-directory' if we can't determine a \"suitable\"
file name for BUFFER."
  (with-current-buffer buffer
    (cond ((buffer-file-name))
          ((and (equal major-mode 'dired-mode)
                (dired-file-name-at-point)
                (not (member (f-filename (dired-file-name-at-point)) '("." ".."))))
           (dired-file-name-at-point))
          ((funcall bfs-ls-function default-directory)
           (f-join default-directory (car (funcall bfs-ls-function default-directory))))
          (t default-directory))))

(defun bfs-broken-symlink-p (file)
  "Return t if FILE is a broken symlink.
Return nil if not."
  (and (file-symlink-p file) (not (file-exists-p (file-truename file)))))

(defun bfs-preview-current-buffer-name ()
  "Return the `buffer-name' of the preview window if lived.
Return nil if preview window isn't lived.

See `bfs-windows'."
  (when (window-live-p (plist-get bfs-windows :preview))
    (buffer-name (window-buffer (plist-get bfs-windows :preview)))))

(defun bfs-preview-matches-child-p ()
  "Return t if buffer in preview window match the child entry."
  (when-let* ((child (bfs-child))
              (preview-buffer-name (bfs-preview-current-buffer-name))
              (preview-file-path
               (with-current-buffer preview-buffer-name
                 (cond ((equal major-mode 'dired-mode)
                        default-directory)
                       ((string= preview-buffer-name bfs-preview-buffer-name)
                        bfs-preview-buffer-file-name)
                       (t (buffer-file-name))))))
    (if (bfs-broken-symlink-p child)
        (string= preview-file-path (file-truename child) )
      (f-equal-p preview-file-path child))))

;;; List directories

(defun bfs-ls-group-directory-first (file-alist)
  "Return a list of FILEs sorting FILE-ALIST with directories first.
Face properties are added to files and directories here.
FILE-ALIST's elements are (FILE . FILE-ATTRIBUTES).
If FILE is one of \".\" or \"..\", we remove it from
the resulting list.
If FILEs are only \".\" or \"..\", return nil."
  (let (el dirs files)
    (while file-alist
      (if (or (eq (cadr (setq el (car file-alist))) t) ; directory
              (and (stringp (cadr el))
                   (file-directory-p (cadr el)))) ; symlink to a directory
          (unless (member (car el) '("." ".."))
            (setq dirs (cons (propertize (car el) 'face 'bfs-directory)
                             dirs)))
        (setq files (cons (propertize (car el) 'face 'bfs-file)
                          files)))
      (setq file-alist (cdr file-alist)))
    (nconc (nreverse dirs) (nreverse files))))

(defun bfs-ls (dir)
  "Return the list of files in DIR.
The list is sorted alphabetically with the directories first.
Return nil, if DIR is empty.

See `bfs-ls-group-directory-first'."
  (let ((file-alist
         (sort (directory-files-and-attributes dir)
               (lambda (x y) (ls-lisp-string-lessp (car x) (car y))))))
    (bfs-ls-group-directory-first file-alist)))

(defun bfs-insert-ls (dir)
  "Insert directory listing for DIR, formatted according to `bfs-ls'.
Leave point after the inserted text."
  (insert (mapconcat 'identity (funcall bfs-ls-function dir) "\n"))
  (insert "\n"))

;;; Create top, parent, child and preview buffers

(defvar bfs-top-buffer-name " *bfs-top* "
  "Top buffer name.")

(defvar bfs-parent-buffer-name " *bfs-parent* "
  "Parent buffer name.")

(defvar bfs-child-buffer-name " *bfs-child* "
  "Child buffer name.")

(defvar bfs-preview-buffer-name " *bfs-preview* "
  "Preview buffer name when we are not visiting a file.
This buffer is used show informations explaining why
we are not previewing `bfs-child' file.")

(defvar-local bfs-preview-buffer-file-name nil)

(defun bfs-top-line-truncate (len s)
  "If S is longer than LEN, cut it down and add \"...\" to the beginning."
  (let ((len-s (length s)))
    (if (> len-s len)
        (concat (propertize "..." 'face 'bfs-directory)
                (substring s (- len-s (- len 3)) len-s))
      s)))

(defun bfs-top-line-default (child)
  "Return the string of CHILD path formated to be used in `bfs-top-buffer-name'."
  (let* ((parent (or (and (f-root-p (f-parent child)) (f-parent child))
                     (concat (f-parent child) "/")))
         (filename (f-filename child))
         (line (propertize parent 'face 'bfs-top-parent-directory)))
    (if-let ((target (file-symlink-p child)))
        (-reduce
         #'concat
         `(,line
           ,(propertize filename
                        'face (if (bfs-broken-symlink-p child)
                                  'bfs-top-broken-symlink
                                'bfs-top-symlink-name))
           ,(propertize " -> " 'face 'bfs-top-symlink-arrow)
           ,(propertize target
                        'face (cond ((bfs-broken-symlink-p child)
                                     'bfs-top-broken-symlink)
                                    ((file-directory-p (file-truename child))
                                     'bfs-top-symlink-directory-target)
                                    (t 'bfs-top-symlink-file-target)))))
      (concat line (propertize filename 'face 'bfs-top-child-entry)))))

(defun bfs-top-line-ellipsed (child)
  "Return `bfs-top-line-default' truncated with ellipses at the beginning.
The truncation is done only if `bfs-top-line-default' length showing CHILD
path is greater than the top window width."
  (bfs-top-line-truncate (window-width (plist-get bfs-windows :top))
                         (bfs-top-line-default child)))

(defun bfs-top-buffer (&optional child)
  "Produce `bfs-top-buffer-name' buffer show child information of CHILD."
  (with-current-buffer (get-buffer-create bfs-top-buffer-name)
    (read-only-mode -1)
    (erase-buffer)
    (insert (funcall bfs-top-line-function (or child (bfs-child))))
    (bfs-top-mode)))

(defun bfs-parent-buffer (parent)
  "Produce `bfs-parent-buffer-name' buffer.
The produced buffer contains the listing of the parent directory of
PARENT and put the cursor at PARENT dirname."
  (with-current-buffer (get-buffer-create bfs-parent-buffer-name)
    (read-only-mode -1)
    (erase-buffer)
    (cond ((f-root-p parent)
           (insert "/") (bfs-goto-entry "/") (bfs-parent-mode parent))
          (t (bfs-insert-ls (f-parent parent))
             (bfs-goto-entry (f-filename parent))
             (bfs-parent-mode (f-parent parent))))))

(defun bfs-child-buffer (parent child-entry)
  "Produce `bfs-child-buffer-name' buffer.
The produced buffer contains the listing of the directory PARENT
and put the cursor at CHILD-ENTRY."
  (with-current-buffer (get-buffer-create bfs-child-buffer-name)
    (read-only-mode -1)
    (erase-buffer)
    (bfs-insert-ls parent)
    (bfs-goto-entry child-entry)
    (bfs-mode parent)))

(defun bfs-preview-buffer (child reason)
  "Produce `bfs-preview-buffer-name' buffer.
Insert REASON string into the buffer that expresses why we
don't visit CHILD as any regular file."
  (with-current-buffer (get-buffer-create bfs-preview-buffer-name)
    (read-only-mode -1)
    (erase-buffer)
    (insert reason)
    (bfs-preview-mode)
    (setq-local bfs-preview-buffer-file-name (file-truename child))))

;;; Display

(defvar bfs-top-window-parameters
  '(display-buffer-in-side-window
    (side . top)
    (window-height . 2)
    (window-parameters . ((no-other-window . t)))))

(defvar bfs-parent-window-parameters
  '(display-buffer-in-side-window
    (side . left)
    (window-width . 0.2)
    (window-parameters . ((no-other-window . t)))))

(defvar bfs-child-window-parameters '(display-buffer-same-window))

(defvar bfs-preview-window-parameters
  '(display-buffer-in-direction
    (direction . right)
    (window-width . 0.6)))

(defvar bfs-frame nil
  "Frame where the `bfs' environment has been started.
Used internally.")

(defvar bfs-windows nil
  "Plist that store `bfs' windows information.
Used internally.
Properties of this plist are: :top, :parent, :child, :preview.")

(defvar bfs-visited-file-buffers nil
  "List of live buffers visited with `bfs-preview'during a `bfs' session.
Used internally.")

(defun bfs-top-update ()
  "Update `bfs-top-buffer-name' and redisplay it."
  (bfs-top-buffer)
  (let ((frame (selected-frame)))
    (select-frame bfs-frame)
    (display-buffer bfs-top-buffer-name bfs-top-window-parameters)
    (select-frame frame)))

(defun bfs-preview (child &optional first-time)
  "Preview file CHILD on the right window.
When FIRST-TIME is non-nil, set the window layout."
  (bfs-top-update)
  (let (preview-window preview-file-buffer (preview-update t))
    (cond ((and (not first-time)
                (bfs-preview-matches-child-p)
                (not (bfs-broken-symlink-p child)))
           (setq preview-update nil))
          ((member (file-name-extension child)
                   bfs-ignored-extensions)
           (bfs-preview-buffer child
                               (format "File ignored due to its extension: %s"
                                       (file-name-extension child))))
          ((and (file-exists-p child)
                (> (file-attribute-size (file-attributes (file-truename child)))
                   bfs-max-size))
           (bfs-preview-buffer child
                               (format "File ignored due to its size: %s"
                                       (file-size-human-readable
                                        (file-attribute-size
                                         (file-attributes (file-truename child)))))))
          ((bfs-broken-symlink-p child)
           (bfs-preview-buffer child "Symlink is broken"))
          (t
           (condition-case err
               (setq preview-file-buffer
                     (find-file-noselect (or (file-symlink-p child) child)))
             (file-error
              (bfs-preview-buffer child (error-message-string err))
              (if first-time
                  (display-buffer (get-buffer bfs-preview-buffer-name)
                                  bfs-preview-window-parameters)
                (display-buffer (get-buffer bfs-preview-buffer-name) t))
              (with-current-buffer bfs-child-buffer-name
                (bfs-line-highlight))))))
    (when preview-update
      (if preview-file-buffer
          (progn
            (setq preview-window
                  (if first-time
                      (display-buffer preview-file-buffer
                                      bfs-preview-window-parameters)
                    (display-buffer preview-file-buffer t)))
            (when (and bfs-kill-buffer-eagerly bfs-visited-file-buffers)
              (kill-buffer (pop bfs-visited-file-buffers)))
            (unless (-contains-p
                     (-union bfs-buffer-list-before bfs-visited-file-buffers)
                     preview-file-buffer)
              (push preview-file-buffer bfs-visited-file-buffers)))
        (if first-time
            (display-buffer (get-buffer bfs-preview-buffer-name)
                            bfs-preview-window-parameters)
          (display-buffer (get-buffer bfs-preview-buffer-name) t))))
    preview-window))

(defun bfs-update (child)
  "Update `bfs' environment according to CHILD file."
  (when (bfs-valid-child-p child)
    (let ((inhibit-message t)
          (parent (f-dirname child))
          (child-entry (f-filename child)))
      (bfs-top-update)
      (bfs-parent-buffer parent)
      (bfs-child-buffer parent child-entry)
      (bfs-preview (f-join parent child-entry)))))

(defun bfs-display (child)
  "Display `bfs' buffers in the current windows according to CHILD.
CHILD must be a file.  Intended to be called only once in `bfs'."
  (when (window-parameter (selected-window) 'window-side)
    (other-window 1))
  (delete-other-windows)
  (bfs-top-buffer child)
  (bfs-parent-buffer (f-dirname child))
  (bfs-child-buffer (f-dirname child) (f-filename child))
  (setq bfs-frame (selected-frame))
  (setq bfs-windows
        (plist-put bfs-windows
                   :top (display-buffer
                         bfs-top-buffer-name
                         bfs-top-window-parameters)))
  (setq bfs-windows
        (plist-put bfs-windows
                   :parent (display-buffer
                            bfs-parent-buffer-name
                            bfs-parent-window-parameters)))
  (setq bfs-windows
        (plist-put bfs-windows
                   :child (display-buffer
                           bfs-child-buffer-name
                           bfs-child-window-parameters)))
  (setq bfs-windows
        (plist-put bfs-windows
                   :preview (bfs-preview child t))))

;;; Leave bfs

(defvar bfs-do-not-check-after
  '(bfs
    bfs-previous bfs-next bfs-backward bfs-forward
    bfs-scroll-down-half-window
    bfs-scroll-up-half-window
    bfs-beginning-of-buffer
    bfs-end-of-buffer
    isearch-forward
    isearch-repeat-forward
    isearch-repeat-backward
    isearch-backward
    bfs-find-file)
  "List of commands after which we don't want to check `bfs' validity.")

(defun bfs-valid-layout-p ()
  "Return t if the window layout in `bfs-frame' frame is valid."
  (let ((parent-win (plist-get bfs-windows :parent))
        (child-win (plist-get bfs-windows :child))
        (preview-win (plist-get bfs-windows :preview))
        (normal-window-list
         ;; we want the bfs layout to be valid when either `transient' or
         ;; `hydra' (when using lv-message, see `hydra-hint-display-type'
         ;; and `lv')  package pops up a window.  So we don't take those
         ;; popped up windows into account to validate the layout.
         (--remove (member (buffer-name (window-buffer it))
                           '(" *transient*" " *LV*"))
                   (window-list))))
    (when (-all-p 'window-live-p `(,parent-win ,child-win ,preview-win))
      (and (equal (length normal-window-list) 4)
           (string= (buffer-name (window-buffer (window-in-direction 'right parent-win)))
                    bfs-child-buffer-name)
           (string= (buffer-name (window-buffer (window-in-direction 'right preview-win t nil t)))
                    bfs-parent-buffer-name)))))

(defun bfs-check-environment ()
  "Leave `bfs' environment if it isn't valid.

We use `bfs-check-environment' in `window-configuration-change-hook'.
This ensure not to end in an inconsistent (unwanted) Emacs state
after running any command that invalidate `bfs' environment.

For instance, your `bfs' environment stops to be valid:
1. when you switch to a buffer not attached to a file,
2. when you modify the layout deleting or rotating windows,
3. when you run any command that makes the previewed buffer
   no longer match the child entry.

See `bfs-valid-layout-p' and `bfs-preview-matches-child-p'."
  (cond
   ((or (window-minibuffer-p)
        (not (eq (selected-frame) bfs-frame))
        (memq last-command bfs-do-not-check-after))
    nil) ;; do nothing
   ((or (not (bfs-valid-layout-p))
        (not (bfs-preview-matches-child-p)))
    (bfs-clean)
    (when (window-parameter (selected-window) 'window-side)
      (other-window 1))
    (delete-other-windows))
   (t nil)))

(defun bfs-clean-if-frame-deleted (_frame)
  "Clean `bfs' environment if the frame that was running it has been deleted.
Intended to be added to `after-delete-frame-functions'."
  (unless (frame-live-p bfs-frame)
    (bfs-clean)))

(defun bfs-kill-visited-file-buffers ()
  "Kill the buffers used to preview files with `bfs-preview'.
This doesn't kill buffers in `bfs-buffer-list-before' that was lived
before entering in the `bfs' environment."
  (-each (-difference bfs-visited-file-buffers bfs-buffer-list-before)
    'kill-buffer)
  (setq bfs-visited-file-buffers nil)
  (setq bfs-buffer-list-before nil))

(defun bfs-clean ()
  "Leave `bfs' environment and clean Emacs state."
  (unless (window-minibuffer-p)
    (setq bfs-is-active nil)
    (remove-function after-delete-frame-functions 'bfs-clean-if-frame-deleted)
    (remove-hook 'window-configuration-change-hook 'bfs-check-environment)
    (remove-hook 'isearch-mode-end-hook 'bfs-isearch-preview-update)
    (remove-hook 'isearch-update-post-hook 'bfs-isearch-preview-update)
    (remove-hook 'window-state-change-hook 'bfs-top-update)
    (setq bfs-visited-backward nil)
    (setq bfs-frame nil)
    (setq bfs-windows nil)
    (bfs-kill-visited-file-buffers)
    (setq window-sides-vertical bfs-window-sides-vertical-before)
    (setq bfs-window-sides-vertical-before nil)
    (setq find-file-run-dired bfs-find-file-run-dired-before)
    (setq bfs-find-file-run-dired-before nil)
    (when (get-buffer bfs-parent-buffer-name)
      (kill-buffer bfs-parent-buffer-name))
    (when (get-buffer bfs-child-buffer-name)
      (kill-buffer bfs-child-buffer-name))
    (when (get-buffer bfs-top-buffer-name)
      (kill-buffer bfs-top-buffer-name))
    (when (get-buffer bfs-preview-buffer-name)
      (kill-buffer bfs-preview-buffer-name))))

(defun bfs-quit ()
  "Leave `bfs-mode' and restore previous window configuration."
  (interactive)
  (bfs-clean)
  (jump-to-register :bfs))

;;; bfs (main entry)

(defvar bfs-is-active nil
  "Non-nil means that `bfs' environment is active in `bfs-frame'.
Used internally.")

(defvar bfs-buffer-list-before nil
  "List of all live buffers when entering in the `bfs' environment.
Used internally.")

(defvar bfs-window-sides-vertical-before nil
  "Used to store user value of `window-sides-vertical'.")

(defvar bfs-find-file-run-dired-before nil)

;;;###autoload
(defun bfs (&optional file)
  "Start a `bfs' (Browse File System) environment in the `selected-frame'.

This pops up a 3 panes (windows) layout that allow you to browse
your file system and preview files.

If FILE (a file name) is given:
- if it is a file, preview it in the right window,
- if it is a directory, list it in the child window.

You can only have one `bfs' environment running at a time.

When you are in the child window (the middle window), you can:
- quit `bfs' environment with `bfs-quit',
- preview files with `bfs-next' and `bfs-previous',
- go up and down in the file system tree with `bfs-backward'
  and `bfs-forward',
- scroll the previewed file with `bfs-scroll-preview-down-half-window',
  `bfs-scroll-preview-up-half-window',
- \"jump\" to any file in your file system with `bfs-find-file', this
  automatically update `bfs' environment.

In the child window, when you move the cursor with functions `isearch-forward'
or `isearch-backward', this will automatically preview the file you
move to.

Any command that invalidates `bfs' environment will cause to leave
`bfs' environment.  See `bfs-check-environment'.

In the child window, the local keymap in use is `bfs-mode-map':

\\{bfs-mode-map}."
  (interactive)
  (cond
   (bfs-is-active
    (when (eq (selected-frame) bfs-frame)
      (bfs-quit)))
   (t
    (let (child)
      (if file
          (progn
            (if (and (file-directory-p file)
                     (bfs-first-valid-child file))
                (setq child (bfs-first-valid-child file))
              (setq child file))
            ;; to prevent `bfs-check-environment' to check `bfs'
            ;; environment when we are building it for the first time
            (setq this-command 'bfs))
        (setq child (bfs-child-default (current-buffer))))
      (when (and child (bfs-valid-child-p child))
        (setq bfs-is-active t)
        (window-configuration-to-register :bfs)
        (setq bfs-buffer-list-before (buffer-list))
        (setq bfs-window-sides-vertical-before window-sides-vertical)
        (setq window-sides-vertical nil)
        (setq bfs-find-file-run-dired-before find-file-run-dired)
        (setq find-file-run-dired t)
        (bfs-display child)
        (add-function :before after-delete-frame-functions 'bfs-clean-if-frame-deleted)
        (add-hook 'window-configuration-change-hook 'bfs-check-environment)
        (add-hook 'isearch-mode-end-hook 'bfs-isearch-preview-update)
        (add-hook 'isearch-update-post-hook 'bfs-isearch-preview-update)
        (add-hook 'window-state-change-hook 'bfs-top-update))))))

(global-set-key (kbd "M-]") 'bfs)

;;; Footer

(provide 'bfs)

;;; bfs.el ends here
