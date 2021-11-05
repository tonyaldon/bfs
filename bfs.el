;;; bfs.el --- Browse File System -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Tony Aldon

;; Author: Tony Aldon <tony.aldon.adm@gmail.com>
;; Version: 0.21.0
;; Package-Requires: ((emacs "27.1") (dash "2.17.0") (f "0.20.0") (s "1.12.0"))
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
(require 's)
(require 'text-property-search)
(require 'cl-macs)

;;; User options

(defgroup bfs nil "Browsing File System." :group 'files)

(defface bfs-directory
  '((t (:inherit dired-directory)))
  "Face used for subdirectories."
  :group 'bfs)

(defface bfs-top-parent-directory
  '((t (:inherit dired-header)))
  "Face used for parent directory path in `bfs-top-buffer-name' buffer."
  :group 'bfs)

(defface bfs-top-child-entry
  '((t (:inherit default :weight ultra-bold)))
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
  '((t (:inherit default)))
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

(defvar bfs-ls-parent-function 'bfs-ls
  "Function of one argument DIR (a file path) that
return a list of filename (not file path) contained in DIR.
\".\" or \"..\" must always be omitted.
This is the function we use to fill `bfs-parent-buffer-name'.
See `bfs-ls'.")

(defvar bfs-ls-child-function 'bfs-ls
  "Function of one argument DIR (a file path) that
return a list of filename (not file path) contained in DIR.
\".\" or \"..\" must always be omitted.
This is the function we use to fill `bfs-child-buffer-name'.
See `bfs-ls'.")

(defvar bfs-dired-hide-details t
  "When t, details are hidden in dired buffers in the preview window.
When nil, dired buffers are visited only with your settings
for `dired-mode'.  So, if you hide the details, they will be
hidden too, if you don't they won't be hidden.

See `dired-hide-details-mode' and the function `bfs-dired-hide-details'.")

;;; Visited files

(defvar bfs-visited-last nil
  "List of last child files visited for a given parent directory.
Child files are uniquely added to `bfs-visited-last' by the
command `bfs-backward' command.

This allow `bfs-forward' to be smart.")

(defun bfs-visited-last-in-dir (dir)
  "Return the last file visited in DIR directory.

Return nil if any file has been visited in DIR so far.
See `bfs-visited-last'."
  (--first (string= dir (f-dirname it)) bfs-visited-last))

(defun bfs-visited-last-push (child)
  "Add CHILD to `bfs-visited-last' list conditionally."
  (unless (or (null child)
              (and (file-directory-p child)
                   (not (file-accessible-directory-p child)))
              (not (bfs-valid-child-p child)))
    (cl-flet ((dirname= (x y) (string= (f-dirname x) (f-dirname y))))
      (setq bfs-visited-last
            (cons child (--remove (dirname= child it) bfs-visited-last))))))

(defvar bfs-visited nil
  "List of all the visited childs.")

(defvar bfs-visited-history nil
  "Minibuffer history of the command `bfs-visit'.")

(defun bfs-visit ()
  "Visit a file (with completion) that has already been visited in bfs.
See `bfs-visited'."
  (interactive)
  (bfs-visited-last-push (bfs-child))
  (let ((file (completing-read "Visit file: " bfs-visited
                               nil t nil 'bfs-visited-history)))
    (if-let (((file-directory-p file))
             (child-in-dir (or (bfs-visited-last-in-dir file)
                               (bfs-first-valid-child file))))
        (bfs-update child-in-dir)
      (bfs-update file))))

;;; Movements

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
  (bfs-visited-last-push (bfs-child))
  (bfs-update default-directory))

(defun bfs-forward ()
  "Update `bfs' environment making `bfs-child' the parent.
In other words, go down by one node in the file system tree.

If `bfs-child' is a readable file, leave `bfs' and visit that file.
If `bfs-child' is an empty directory, leave `bfs' and visit that file."
  (interactive)
  (if-let ((child (bfs-child)))
      (cond ((and (file-directory-p child)
                  (not (file-accessible-directory-p child)))
             (message "Permission denied: %s" child))
            ((file-directory-p child)
             (let* ((visited (bfs-visited-last-in-dir child))
                    (ls-child-filtered (bfs-ls-child-filtered child))
                    (visited-belong-child-filtered-p
                     (and visited
                          (member visited (--map (f-join child it)
                                                 ls-child-filtered)))))
               (cond
                (visited-belong-child-filtered-p
                 (bfs-update visited))
                (ls-child-filtered
                 (bfs-update (f-join child (car ls-child-filtered))))
                ((and (null ls-child-filtered)
                      (funcall bfs-ls-child-function child))
                 (message "Can't go forward, filters are in effect: %s"
                          bfs-ls-child-filter-functions))
                (t (message "Can't go forward, directory is empty")))))
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

(defun bfs-parent-goto-previous-dir ()
  "Go to the previous dir in parent buffer.

Return file path of the previous dir in parent buffer.
Return nil if the current parent entry is the first dir
in the parent buffer.

See: `bfs-parent-sibling-dir'."
  (with-current-buffer bfs-parent-buffer-name
    (unless (bobp)
      (forward-line -1)
      (let ((file (get-text-property (point) 'bfs-file)))
        (while (and (not (bobp)) file (not (file-directory-p file)))
          (forward-line -1)
          (setq file (get-text-property (point-at-bol) 'bfs-file)))
        (when (and file (file-directory-p file)) file)))))

(defun bfs-parent-goto-next-dir ()
  "Go to the next dir in parent buffer.

Return file path of the next dir in parent buffer.
Return nil if the current parent entry is the last dir
in the parent buffer.

See: `bfs-parent-sibling-dir'."
  (with-current-buffer bfs-parent-buffer-name
    (when-let ((match (text-property-search-forward 'bfs-file nil nil 'not-current))
               (file (prop-match-value match)))
      (while (and file (not (file-directory-p file)))
        (setq match (text-property-search-forward 'bfs-file nil nil 'not-current))
        (setq file (and match (prop-match-value match))))
      file)))

(defun bfs-parent-sibling-dir (sibling)
  "Make SIBLING of current parent entry the parent of the `bfs' environment.
SIBLING can be 'previous or 'next.
See: `bfs-parent-previous' and `bfs-next-previous'."
  (bfs-visited-last-push (bfs-child))
  (when-let ((dir (funcall (pcase sibling
                             ('previous 'bfs-parent-goto-previous-dir)
                             ('next 'bfs-parent-goto-next-dir)))))
    (if-let ((child-in-dir (or (bfs-visited-last-in-dir dir)
                               (bfs-first-valid-child dir))))
        (bfs-update child-in-dir)
      (with-current-buffer bfs-parent-buffer-name
        (bfs-line-highlight-parent))
      (with-current-buffer bfs-child-buffer-name
        (let ((inhibit-read-only t))
          (erase-buffer)
          (setq default-directory dir)
          (insert "No preview")))
      (bfs-preview nil)
      (bfs-top-update))))

(defun bfs-parent-previous ()
  "Make previous parent entry the parent of the `bfs' environment."
  (interactive)
  (bfs-parent-sibling-dir 'previous))

(defun bfs-parent-next ()
  "Make next parent entry the parent of the `bfs' environment."
  (interactive)
  (bfs-parent-sibling-dir 'next))

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

;;; Find files and dired commands

(defun bfs-dired ()
  "Quit bfs and open a dired buffer listing the files that was in child buffer."
  (interactive)
  (let ((dir default-directory)
        (file (bfs-child)))
    (delete-other-windows)
    (bfs-clean)
    (dired dir)
    (when file
      (dired-goto-file file))))

(defun bfs-toggle-dired-details ()
  "Toggle visibility of details in preview window if showing a Dired buffer.
See `dired-hide-details-mode'."
  (interactive)
  (with-selected-window (plist-get bfs-windows :preview)
    (when (equal major-mode 'dired-mode)
      (dired-hide-details-mode 'toggle))))

(defun bfs-find-file (file)
  "Find a FILE with your completion framework and update `bfs' environment."
  (interactive
   (list (read-file-name "Find file:" nil default-directory t)))
  (bfs-visited-last-push (bfs-child))
  (if-let (((file-directory-p file))
           (child-in-dir (or (bfs-visited-last-in-dir file)
                             (bfs-first-valid-child file))))
      (bfs-update child-in-dir)
    (bfs-update file)))

(defun bfs-project-find-file-in (filename dirs project)
  "Complete FILENAME in DIRS in PROJECT and update `bfs' environment."
  (let* ((all-files (project-files project dirs))
         (completion-ignore-case read-file-name-completion-ignore-case)
         (file (funcall project-read-file-name-function
                        "Find file" all-files nil nil
                        filename)))
    (if (string= file "")
        (user-error "You didn't specify the file")
      (if-let (((file-directory-p file))
               (child-in-dir (or (bfs-visited-last-in-dir file)
                                 (bfs-first-valid-child file))))
          (bfs-update child-in-dir)
        (bfs-update file)))))

(defun bfs-project-find-file ()
  "Update `bfs' env visiting a file (with completion) in the current project.

The completion default is the filename at point, determined by
`thing-at-point' (whether such file exists or not)."
  (interactive)
  (bfs-visited-last-push (bfs-child))
  (let* ((pr (project-current t))
         (dirs (list (project-root pr))))
    (bfs-project-find-file-in (thing-at-point 'filename) dirs pr)))

;;; bfs modes

;;;; Font Lock mode

(defvar bfs-top-font-lock-keywords nil
  "Additional expressions to highlight in `bfs-top-mode',
using `font-lock-mode'.")

(defvar bfs-preview-font-lock-keywords nil
  "Additional expressions to highlight in `bfs-preview-mode',
using `font-lock-mode'.")

(defvar bfs-parent-font-lock-keywords
  '((bfs-font-lock-match-dir-entry+info 0 'bfs-directory))
  "Additional expressions to highlight in `bfs-parent-mode',
using `font-lock-mode'.")

(defvar bfs-font-lock-keywords
  '((bfs-font-lock-match-dir-entry+info 0 'bfs-directory))
  "Additional expressions to highlight in `bfs-mode',
using `font-lock-mode'.")

(defun bfs-font-lock-match-dir-entry (_bound)
  "Matcher that matches an entry that is a directory.
BOUND is the limit of the search.  (In general, BOUND has the
value `point-max'.  See `font-lock.el' file).
This function set the match data.
Return nil if no directory entry found."
  (when-let ((match (text-property-search-forward 'bfs-entry))
             (file (get-text-property (point-at-bol) 'bfs-file)))
    (when (file-directory-p file)
      (let ((match-beg (prop-match-beginning match))
            (match-end (prop-match-end match)))
        (set-match-data `(,match-beg ,match-end))
        match-end))))

(defun bfs-font-lock-match-dir-entry+info (_bound)
  "Matcher that matches an entry that is a directory.
BOUND is the limit of the search.  (In general, BOUND has the
value `point-max'.  See `font-lock.el' file).
This function set the match data.
Return nil if no directory entry found."
  (when-let ((match (text-property-search-forward 'bfs-entry))
             (file (get-text-property (point-at-bol) 'bfs-file)))
    (when (file-directory-p file)
      (let ((match-beg (prop-match-beginning match))
            (match-end (point-at-eol)))
        (set-match-data `(,match-beg ,match-end))
        match-end))))

;;;; Keymaps

(defvar bfs-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map (kbd "p") 'bfs-previous)
    (define-key map (kbd "n") 'bfs-next)
    (define-key map (kbd "b") 'bfs-backward)
    (define-key map (kbd "f") 'bfs-forward)
    (define-key map (kbd "RET") 'bfs-forward)
    (define-key map (kbd "M-p") 'bfs-parent-previous)
    (define-key map (kbd "M-n") 'bfs-parent-next)

    (define-key map (kbd "<backspace>") 'bfs-scroll-preview-down-half-window)
    (define-key map (kbd "<SPC>") 'bfs-scroll-preview-up-half-window)
    (define-key map (kbd "C-<backspace>") 'bfs-scroll-down-half-window)
    (define-key map (kbd "C-<SPC>") 'bfs-scroll-up-half-window)
    (define-key map (kbd "<") 'bfs-beginning-of-buffer)
    (define-key map (kbd ">") 'bfs-end-of-buffer)
    (define-key map (kbd "TAB") 'bfs-toggle-dired-details)

    (define-key map (kbd "v") 'bfs-visit)
    (define-key map (kbd "C-f") 'bfs-find-file)
    (define-key map (kbd "M-f") 'bfs-project-find-file)

    (define-key map (kbd "'") 'bfs-dired)

    (define-key map (kbd "m") 'bfs-mark)
    (define-key map (kbd "u") 'bfs-unmark)
    (define-key map (kbd "U") 'bfs-unmark-all)
    (define-key map (kbd "t") 'bfs-toggle-marks)
    (define-key map (kbd "k") 'bfs-kill-marked)
    (define-key map (kbd "%") 'bfs-mark-regexp)

    (define-key map (kbd ".") 'bfs-hide-dotfiles)
    (define-key map (kbd "/") 'bfs-narrow)

    (define-key map (kbd "g") 'revert-buffer)
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
  "Overlay used to highlight the current line in `bfs-mode'.
Also used in `bfs-parent-mode'.")

(defun bfs-line-move-overlay (overlay)
  "Move `bfs-line-overlay' to the line including the point by OVERLAY."
  (move-overlay
   overlay (line-beginning-position) (line-beginning-position 2)))

(defun bfs-line-highlight-child ()
  "Highlight current child entry in child buffer.
The highlighting is peformed with an overlay.

This function must be called with `bfs-child-buffer-name' buffer current.
Here an example:
  (with-current-buffer bfs-child-buffer-name
    (bfs-line-highlight-child))"
  (unless bfs-line-overlay
    (setq bfs-line-overlay (make-overlay (point) (point))))
  (let* ((entry-point
          (or (and (get-text-property (point-at-bol) 'bfs-entry) (point-at-bol))
              (next-single-property-change (point-at-bol) 'bfs-entry nil (point-at-eol))))
         (face-entry (and entry-point
                          (if (listp (get-text-property entry-point 'face))
                              (car (get-text-property entry-point 'face))
                            (get-text-property entry-point 'face))))
         (foreground-line
          (or (and face-entry (face-foreground face-entry nil t))
              (face-foreground 'default nil t)))
         (background-line
          (or (and face-entry (face-background face-entry nil t))
              (face-background 'default nil t)))
         (face `(:background ,foreground-line
                 :foreground ,background-line
                 :weight ultra-bold
                 :extend t)))
    (overlay-put bfs-line-overlay 'face face))
  (bfs-line-move-overlay bfs-line-overlay))

(defun bfs-line-highlight-parent ()
  "Highlight current parent entry in parent buffer.
The highlighting is peformed with an overlay.

This function must be called with `bfs-parent-buffer-name' buffer current.
Here an example:
  (with-current-buffer bfs-parent-buffer-name
    (bfs-line-highlight-parent))"
  (unless bfs-line-overlay
    (setq bfs-line-overlay (make-overlay (point) (point))))
  (let ((face `(:background ,(face-foreground 'bfs-directory nil t)
                :foreground ,(or (face-background 'bfs-directory nil t)
                                 (face-background 'default nil t))
                :weight ultra-bold
                :extend t)))
    (overlay-put bfs-line-overlay 'face face))
  (bfs-line-move-overlay bfs-line-overlay))

;;;; bfs-top-mode

(defun bfs-top-mode-line (&optional child)
  "Return the string that describe CHILD file.
This string is used in the mode line of `bfs-top-buffer-name' buffer.
If CHILD is nil, default to `bfs-child'."
  (if-let ((file (or child (bfs-child))))
      (with-temp-buffer
        (insert-directory file "-lh")
        (delete-char -1) ; delete the last newline character
        (goto-char (point-min))
        (dired-goto-next-file)
        (delete-region (point) (point-at-eol))
        (concat " " (buffer-substring-no-properties (point-min) (point-max))))
    " No child entry to be previewed"))

(define-derived-mode bfs-top-mode fundamental-mode "bfs-top"
  "Mode use in `bfs-top-buffer-name' buffer.
See `bfs-top-buffer'."
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
  (setq buffer-read-only t)
  (setq-local font-lock-defaults '(bfs-top-font-lock-keywords t)))

;;;; bfs-preview-mode

(define-derived-mode bfs-preview-mode fundamental-mode "bfs-preview"
  "Mode use in `bfs-preview-buffer-name'."
  (visual-line-mode t)
  (setq buffer-read-only t)
  (setq-local font-lock-defaults '(bfs-preview-font-lock-keywords t)))

;;;; bfs-parent-mode

(defvar bfs-parent-mode-line-format nil
  "If non-nil, this is the `mode-line-format' of `bfs-parent-mode'.")

(define-derived-mode bfs-parent-mode fundamental-mode "bfs-parent"
  "Mode used in `bfs-parent-buffer-name' buffer.
In `bfs-parent-mode', `default-directory' is set to DIR, and
must be the parent directory of the file listed in
`bfs-parent-buffer-name' buffer.
See `bfs-parent-buffer' command."
  (setq-local cursor-type nil)
  (setq-local global-hl-line-mode nil)
  (add-hook 'post-command-hook #'bfs-line-highlight-parent nil t)
  (setq mode-line-format (or bfs-parent-mode-line-format ""))
  (setq buffer-read-only t)
  (setq-local font-lock-defaults '(bfs-parent-font-lock-keywords t)))

;;;; bfs-mode

(defvar bfs-mode-line-format nil
  "If non-nil, this is the `mode-line-format' of `bfs-mode'.")

(define-derived-mode bfs-mode fundamental-mode "bfs"
  "Mode used in `bfs-child-buffer-name' buffer.
In `bfs-mode', `default-directory' is set to PARENT, and
must be the parent directory of the file listed in
`bfs-child-buffer-name' buffer.
See `bfs-child-buffer' command."
  (setq-local cursor-type nil)
  (setq-local global-hl-line-mode nil)
  (add-hook 'post-command-hook #'bfs-line-highlight-child nil t)
  (setq mode-line-format (or bfs-mode-line-format ""))
  (setq buffer-read-only t)
  (setq-local revert-buffer-function #'bfs-revert)
  (setq-local font-lock-defaults '(bfs-font-lock-keywords t)))

;;; Utilities

(defun bfs-child ()
  "Return file path corresponding to the current child entry.
If `bfs-child-buffer-name' isn't lived return nil."
  (when (buffer-live-p (get-buffer bfs-child-buffer-name))
    (with-current-buffer bfs-child-buffer-name
      (get-text-property (point) 'bfs-file))))

(defun bfs-goto-entry (entry)
  "Move the cursor to the line ENTRY.
If there is no line with ENTRY or ENTRY is nil, go to the first line."
  (goto-char (point-min))
  (text-property-search-forward 'bfs-entry entry t)
  (beginning-of-line))

(defun bfs-valid-child-p (child)
  "Return t if CHILD (file path) can be a child in `bfs' environment."
  (cond ((or (string= "" child) (not (f-exists-p child)))
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
             (--map (f-join dir it) (bfs-ls-child-filtered dir)))))

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
          ((bfs-ls-child-filtered default-directory)
           (f-join default-directory
                   (car (bfs-ls-child-filtered default-directory))))
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
  (when-let*
      ((preview-buffer-name (bfs-preview-current-buffer-name))
       (preview-file-path
        (with-current-buffer preview-buffer-name
          (cond ((equal major-mode 'dired-mode) default-directory)
                ((string= preview-buffer-name bfs-preview-buffer-name)
                 bfs-preview-buffer-file-name)
                (t (buffer-file-name))))))
    (let ((child (bfs-child)))
      (cond ((and (null child) (equal preview-file-path 'no-child-entry)) t)
            ((and child (equal preview-file-path 'no-child-entry)) nil)
            ((and child (bfs-broken-symlink-p child))
             (string= preview-file-path (file-truename child)))
            (child (f-equal-p preview-file-path child))))))

(defun bfs-isearch-preview-update ()
  "Update the preview window with the current child entry file.

Intended to be added to `isearch-update-post-hook' and
`isearch-mode-end-hook'.  This allows to preview the file the
cursor has moved to using \"isearch\" commands in
`bfs-child-buffer-name' buffer."
  (when (string= (buffer-name) bfs-child-buffer-name)
    (bfs-preview (bfs-child))))

(defun bfs-dired-hide-details ()
  "Hide details in Dired mode.
This function is meant to be used as the deepest hook
of `dired-mode-hook'."
  (dired-hide-details-mode))

;;; List directories

(defvar bfs-ls-child-filter-functions nil
  "List of filter functions that are applied to `bfs-ls-child-function' list.

Each function takes one argument FILENAME (the name, in linux system, part
after the last \"/\") and returns non-nil if we want FILENAME
to be kept in the \"ls\" listing of `bfs-child-buffer-name'.

See `bfs-insert-ls-child'.")

(defun bfs-ls-group-directory-first (file-alist)
  "Return a list of FILEs sorting FILE-ALIST with directories first.
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
            (setq dirs (cons (car el) dirs)))
        (setq files (cons (car el) files)))
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

(defun bfs-ls-child-filtered (dir)
  "Filter the list returned by `bfs-ls-child-function' applied to DIR.
We apply `bfs-ls-child-filter-functions' filters."
  (if-let* ((filters bfs-ls-child-filter-functions)
            (filter (apply '-andfn filters)))
      (-filter filter (funcall bfs-ls-child-function dir))
    (funcall bfs-ls-child-function dir)))

;;; Format entries

(defvar bfs-format-parent-entry-function
  'bfs-format-entry-parent
  "Function that formats the lines to be displayed in
`bfs-parent-buffer-name'.

See `bfs-format-child-entry-function' to know how
`bfs-format-parent-entry-function' must be defined.  Not, that as
we don't implement a mark system in `bfs-parent-buffer-name' buffer,
in `bfs-format-parent-entry-function' function, you don't have
to implement this functionality.  Nevertheless, they both
have the same signature.

`bfs' provides 4 format functions for `bfs-parent-buffer-name':
- `bfs-format-entry-parent',
- `bfs-format-entry+size-parent',
- `bfs-format-icon+entry-parent',
- `bfs-format-icon+entry+size-parent'.")

(defvar bfs-format-child-entry-function
  'bfs-format-entry+size
  "Function that formats the lines to be displayed in
`bfs-child-buffer-name'.

The function is of the form:
  (entry dir &optional max-length mark) -> string
1. The returned string must have the text property 'bfs-file set
   to the concatenation of DIR and ENTRY,
2. The part of the returned string that correspond to ENTRY must
   have the text property 'bfs-entry set to ENTRY,
3. If MARK is t, the returned string must have the text property
   'bfs-marked set to t,
4. If you add some info to the right of ENTRY in the returned string,
   you might want to add spaces between in order to verticaly
   align the information in the buffer.  To do this, you can
   use MAX-LENGTH argument,that correspond to the longest string
   resulting of the concatenation of ENTRY and the info corresponding
   to the entry determined for all entries (filename) in DIR.
   See `bfs-max-length'.

`bfs' provides 4 format functions for `bfs-parent-child-name':
- `bfs-format-entry',
- `bfs-format-entry+size',
- `bfs-format-icon+entry',
- `bfs-format-icon+entry+size'.")

(defun bfs-space-between (len s1 s2)
  "Concatenate S1 and S2 with spaces in between.
Add as many spaces as necessary to make the length of the
resulting string equal to LEN.
If LEN is too small, add only one space."
  (let ((space-nb (max 1 (- len (length (concat s1 s2))))))
    (concat s1 (make-string space-nb ?\ ) s2)))

(defun bfs-size-or-number-of-files (file)
  "Return the size of FILE file in human readable format.
If FILE is an accessible directory, return the number of files it contains.
Return the empty string in any other cases."
  (cond ((file-regular-p file)
         (file-size-human-readable
          (file-attribute-size (file-attributes file)) nil " "))
        ((file-accessible-directory-p file)
         (number-to-string
          (length (--remove (member it '("." "..")) (directory-files file)))))
        (t "")))

(defun bfs-max-length (dir in-buffer &optional is-root)
  "Longest length of the concatenation of entries in DIR and their size.
The size is determine by the function `bfs-size-or-number-of-files'.
The entries are obtain by listing DIR directory with:
- `bfs-ls-child-filtered' if IN-BUFFER is 'child,
- `bfs-ls-parent-function' if IN-BUFFER is 'parent.
Return nil if there no file to list in DIR.
When IS-ROOT t, we don't list DIR, and the calculation is done only on
the entry DIR.  This case happens when we are at the top of the file
system and `bfs-parent-buffer-name' buffer has only the entry root and
`bfs-child-buffer-name' list the files of root."
  (let ((filenames
         (cond ((equal in-buffer 'child) (bfs-ls-child-filtered dir))
               ((equal in-buffer 'parent) (funcall bfs-ls-parent-function dir)))))
    (if is-root
        (+ (length dir) (length (bfs-size-or-number-of-files dir)))
      (when filenames
        (-max (--map (+ (length it)
                        (length
                         (bfs-size-or-number-of-files (f-join dir it))))
                     filenames))))))

(defun bfs-format-entry (entry dir &optional _max-length mark)
  "Return the string ENTRY with some added text properties.

Format ENTRY to be displayed in `bfs-child-buffer-name' buffer.
ENTRY is a filename belonging to DIR directory.
MAX-LENGTH argument isn't used.
If MARK is t, it means the ENTRY is marked.

See `bfs-format-child-entry-function'."
  (let* ((file (f-join dir entry))
         (bfs-entry
          (propertize
           (if mark (propertize entry 'font-lock-face 'bfs-mark) entry)
           'bfs-entry entry))
         (left-pad
          (if mark (propertize "* " 'font-lock-face 'bfs-mark) "  ")))
    (propertize (concat left-pad bfs-entry)
                'bfs-file file
                'bfs-marked mark)))

(defun bfs-format-entry+size (entry dir &optional max-length mark)
  "Return the string ENTRY with the file size of ENTRY on the right.

Format ENTRY to be displayed in `bfs-child-buffer-name' buffer.
ENTRY is a filename belonging to DIR directory.
MAX-LENGTH correspond to the value of `bfs-max-length'.
If MARK is t, it means the ENTRY is marked.

See `bfs-format-child-entry-function' and `bfs-size-or-number-of-files'."
  (let* ((left-pad (if mark (propertize "* " 'font-lock-face 'bfs-mark) "  "))
         (file (f-join dir entry))
         (bfs-entry (propertize entry 'bfs-entry entry))
         (size (bfs-size-or-number-of-files file))
         (info (propertize size 'bfs-info t))
         (space-between
          (bfs-space-between (1+ (or max-length 0)) bfs-entry info))
         (entry+info (if mark
                         (propertize space-between 'font-lock-face 'bfs-mark)
                       space-between)))
    (propertize (concat left-pad entry+info)
                'bfs-file file
                'bfs-marked mark)))

(defun bfs-format-entry-parent (entry dir &optional max-length mark)
  "A wrapper on `bfs-format-entry' where the left spaces are trimmed."
  (s-trim-left (bfs-format-entry entry dir max-length mark)))

(defun bfs-format-entry+size-parent (entry dir &optional max-length mark)
  "A wrapper on `bfs-format-entry+size' where the left spaces are trimmed."
  (s-trim-left (bfs-format-entry+size entry dir max-length mark)))

;;;; All the icons

(declare-function all-the-icons-icon-for-dir "ext:all-the-icons")
(declare-function all-the-icons-icon-for-file "ext:all-the-icons")

(defvar bfs-icon-v-adjust 0.01
  "The default vertical adjustment of the icon in `bfs-mode'.
The variable is meaningful only if you have `all-the-icons' installed
and at least one of the functions `bfs-format-child-entry-function'
or `bfs-format-parent-entry-function' is a function that uses
`all-the-icons'.

See `bfs-format-icon+entry' and `bfs-icon'.")

(defun bfs-icon (file &optional mark)
  "Return the icon string provide by `all-the-icons' corresponding to FILE.
If MARK is true, the returned icon string has the face `bfs-mark'."
  (if (file-directory-p file)
      (all-the-icons-icon-for-dir
       file
       :face (or (and mark 'bfs-mark) 'bfs-directory)
       :v-adjust bfs-icon-v-adjust)
    (if mark (all-the-icons-icon-for-file
              file :face 'bfs-mark :v-adjust bfs-icon-v-adjust)
      (all-the-icons-icon-for-file file :v-adjust bfs-icon-v-adjust))))

(defun bfs-format-icon+entry (entry dir &optional _max-length mark)
  "Return the string ENTRY preceded by the icon corresponding to ENTRY.

Format ENTRY to be displayed in `bfs-child-buffer-name' buffer.
ENTRY is a filename belonging to DIR directory.
MAX-LENGTH argument isn't used.
If MARK is t, it means the ENTRY is marked.

See `bfs-format-child-entry-function' and `bfs-icon'."
  (let* ((file (f-join dir entry))
         (bfs-entry
          (propertize
           (if mark (propertize entry 'font-lock-face 'bfs-mark) entry)
           'bfs-entry entry))
         (icon (bfs-icon file mark))
         (left-pad
          (if mark (propertize "* " 'font-lock-face 'bfs-mark) "  ")))
    (propertize (concat left-pad icon "\t" bfs-entry)
                'bfs-file file
                'bfs-marked mark)))

(defun bfs-format-icon+entry+size (entry dir &optional max-length mark)
  "Return the string ENTRY preceded by an icon and the file size at the end.

Format ENTRY to be displayed in `bfs-child-buffer-name' buffer.
ENTRY is a filename belonging to DIR directory.
MAX-LENGTH correspond to the value of `bfs-max-length'.
If MARK is t, it means the ENTRY is marked.

See `bfs-format-child-entry-function', `bfs-icon' and
`bfs-size-or-number-of-files'."
  (let* ((left-pad (if mark (propertize "* " 'font-lock-face 'bfs-mark) "  "))
         (file (f-join dir entry))
         (bfs-entry (propertize entry 'bfs-entry entry))
         (icon (bfs-icon file mark))
         (size (bfs-size-or-number-of-files file))
         (info (propertize size 'bfs-info t))
         (space-between
          (bfs-space-between (1+ (or max-length 0)) bfs-entry info))
         (entry+info (if mark
                         (propertize space-between 'font-lock-face 'bfs-mark)
                       space-between)))
    (propertize (concat left-pad icon "\t" entry+info)
                'bfs-file file
                'bfs-marked mark)))

(defun bfs-format-icon+entry-parent (entry dir &optional max-length mark)
  "A wrapper on `bfs-format-icon+entry' where the left spaces are trimmed."
  (s-trim-left (bfs-format-icon+entry entry dir max-length mark)))

(defun bfs-format-icon+entry+size-parent (entry dir &optional max-length mark)
  "A wrapper on `bfs-format-icon+entry+size' where the left spaces are trimmed."
  (s-trim-left (bfs-format-icon+entry+size entry dir max-length mark)))

;;; Mark entries

(defface bfs-mark
  '((t (:inherit dired-mark)))
  "Face used for subdirectories."
  :group 'bfs)

(defvar bfs-regexp-history nil
  "History list of regular expressions used by `bfs-mark-regex'.
This history is also used by `bfs-narrow'.")

(defun bfs-entry-at-point ()
  "Return entry on the line at `point'.
Return nil if there is no entry found."
  (if-let ((entry-match
            (save-excursion
              (goto-char (point-at-bol))
              (text-property-search-forward 'bfs-entry))))
      (prop-match-value entry-match)))

(defun bfs-mark ()
  "Mark line at point."
  (interactive)
  (let ((inhibit-read-only t))
    (when-let ((entry (bfs-entry-at-point)))
      (save-excursion
        (delete-and-extract-region (point-at-bol) (point-at-eol))
        (insert (funcall bfs-format-child-entry-function
                         entry default-directory bfs-max-length t))))))

(defun bfs-unmark ()
  "Unmark line at point."
  (interactive)
  (let ((inhibit-read-only t))
    (when-let ((entry (bfs-entry-at-point)))
      (save-excursion
        (delete-and-extract-region (point-at-bol) (point-at-eol))
        (insert (funcall bfs-format-child-entry-function
                         entry default-directory bfs-max-length))
        (font-lock-fontify-region (point-at-bol) (point-at-eol))))))

(defun bfs-unmark-all ()
  "Unmark all buffer."
  (interactive)
  (let ((inhibit-read-only t) entry)
    (save-excursion
      (goto-char (point-min))
      (while (text-property-search-forward 'bfs-marked t t)
        (setq entry (bfs-entry-at-point))
        (delete-and-extract-region (point-at-bol) (point-at-eol))
        (insert (funcall bfs-format-child-entry-function
                         entry default-directory bfs-max-length))))
    (save-excursion
      (font-lock-fontify-region (point-at-bol) (point-at-eol)))))

(defun bfs-mark-regexp (regexp)
  "Mark all files matching REGEXP.
REGEXP is matched against each bfs entry (filename).
REGEXP is an Emacs regexp, not a shell wildcard."
  (interactive
   (list (read-regexp "Mark files (regexp): " nil 'bfs-regexp-history)))
  (save-excursion
    (goto-char (point-min))
    (let (entry-match)
      (while (setq entry-match (text-property-search-forward 'bfs-entry))
        (when-let* ((entry (prop-match-value entry-match))
                    ((string-match-p regexp entry)))
          (bfs-mark)
          (forward-line))))))

(defun bfs-is-marked-p ()
  "Return t if entry at point is marked."
  (get-text-property (point-at-bol) 'bfs-marked))

(defun bfs-toggle-marks ()
  "Toggle mark in buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (and (not (eobp)) (bfs-entry-at-point))
      (if (bfs-is-marked-p) (bfs-unmark) (bfs-mark))
      (forward-line))))

(defun bfs-kill-marked ()
  "Kill all marked entries (not the files)."
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (text-property-search-forward 'bfs-marked t t)
        (delete-and-extract-region (point-at-bol) (line-beginning-position 2))))
    (bfs-preview (bfs-child))))

(defun bfs-list-marked (&optional entries)
  "Return the list of marked files in `bfs-child-buffer-name' buffer.
Return nil if no files marked.

If ENTRIES is non-nil, return entries (filenames) in the list (not files)."
  (let (marked file)
    (save-excursion
      (goto-char (point-min))
      (while (text-property-search-forward 'bfs-marked t t)
        (if-let ((entries) (entry (bfs-entry-at-point)))
            (push entry marked)
          (and (setq file (get-text-property (point-at-bol) 'bfs-file))
               (push file marked))))
      (nreverse marked))))

(defun bfs-revert (&optional _arg _noconfirm)
  "Revert `bfs-child-buffer-name'.
Bfs entries that are marked are left marked."
  (interactive)
  (let* ((child (bfs-child))
         (child-entry (bfs-entry-at-point))
         (marked-entries (bfs-list-marked 'entries)))
    (bfs-child-buffer default-directory child-entry marked-entries)
    (bfs-preview child)))

;;; Filter entries in child buffer

;;;; Hide dotfiles in child buffer

(defun bfs-hide-dotfiles-filter (filename)
  "Return non-nil if FILENAME doesn't start with a \".\"."
  (not (string-match-p "^\\." filename)))

(defun bfs-hide-dotfiles ()
  "Toggle visibility of dotfiles in `bfs-child-buffer-name'."
  (interactive)
  (if (member 'bfs-hide-dotfiles-filter bfs-ls-child-filter-functions)
      (setq bfs-ls-child-filter-functions
            (--remove (equal it 'bfs-hide-dotfiles-filter)
                      bfs-ls-child-filter-functions))
    (push 'bfs-hide-dotfiles-filter bfs-ls-child-filter-functions))
  (bfs-child-buffer default-directory
                    (or (and (bfs-child) (f-filename (bfs-child))) ""))
  (bfs-preview (bfs-child)))

;;;; Narrow child buffer interactively

(defvar bfs-narrow-current-regexp nil
  "Regexp used to narrow child buffer dynamically.
This variable is set and used by `bfs-narrow-update'.
This is how we dynamically modify the filter function
`bfs-narrow-filter' and so narrow the child buffer.

See `bfs-narrow'.")

(defvar bfs-narrow-marked-entries nil
  "List of marked entries before narrowing with `bfs-narrow'.")

(defvar bfs-narrow-child-entry nil
  "child before narrowing with `bfs-narrow'.")

(defun bfs-narrow-filter (entry)
  "Return t when `bfs-narrow-current-regexp' matches ENTRY.
Unconditionally return t when `bfs-narrow-current-regexp' isn't
a valid regexp.
This function is meant to be added to `bfs-ls-child-filter-functions'
temporary when we are dynamically narrowing the child buffer
with `bfs-narrow'."
  (condition-case nil
      (string-match-p bfs-narrow-current-regexp entry)
    (error t)))

(defun bfs-narrow-minibuffer-setup ()
  "Set minibuffer for dynamic narrowing.
This function is meant to be added to the hook `minibuffer-setup-hook'.
See `bfs-narrow-update'."
  (add-hook 'post-command-hook 'bfs-narrow-update nil 'local))

(defun bfs-narrow-update ()
  "Narrow the child buffer based on the contents of the minibuffer.
This function is meant to be added in the hook `post-command-hook'
locally in minibuffer.  See `bfs-narrow-minibuffer-setup' and `bfs-narrow'.

This function locally set `bfs-narrow-current-regexp'.
This function depends on the value of `bfs-narrow-child-entry' and
`bfs-narrow-marked-entries'."
  (let* ((bfs-narrow-current-regexp (minibuffer-contents-no-properties))
         (child-window (plist-get bfs-windows :child))
         (child-entry (or (and (s-blank-p bfs-narrow-current-regexp)
                               bfs-narrow-child-entry)
                          (and (bfs-child) (f-filename (bfs-child))))))
    (with-selected-window child-window
      (bfs-child-buffer default-directory
                        child-entry
                        bfs-narrow-marked-entries)
      (bfs-preview (bfs-child)))))

(defun bfs-narrow ()
  "Narrow bfs child buffer to filenames matching a regexp read from minibuffer.
See `bfs-narrow-filter', `bfs-narrow-update' and `bfs-narrow-minibuffer-setup'."
  (interactive)
  (let ((bfs-narrow-child-entry (and (bfs-child) (f-filename (bfs-child))))
        quit-normaly-p)
    (unwind-protect
        (progn
          (setq bfs-narrow-marked-entries (bfs-list-marked 'entries))
          (add-hook 'minibuffer-setup-hook 'bfs-narrow-minibuffer-setup)
          (push 'bfs-narrow-filter bfs-ls-child-filter-functions)
          ;;`read-regexp' returns `nil' when minibuffer is quitted with C-g
          (setq quit-normaly-p
                (read-regexp "narrow files (regexp): "
                             nil 'bfs-regexp-history)))
      (setq bfs-ls-child-filter-functions
            (--remove (equal it 'bfs-narrow-filter)
                      bfs-ls-child-filter-functions))
      (remove-hook 'minibuffer-setup-hook 'bfs-narrow-minibuffer-setup)
      (if quit-normaly-p
          (with-selected-window (plist-get bfs-windows :child)
            (bfs-preview (bfs-child)))
        (with-selected-window (plist-get bfs-windows :child)
          (bfs-child-buffer default-directory
                            bfs-narrow-child-entry
                            bfs-narrow-marked-entries)
          (bfs-preview (bfs-child))))
      (setq bfs-narrow-current-regexp nil)
      (setq bfs-narrow-marked-entries nil)
      (setq bfs-narrow-child-entry nil))))

;;; Create top, parent, child and preview buffers

(defvar bfs-top-buffer-name "*bfs-top*"
  "Top buffer name.")

(defvar bfs-parent-buffer-name "*bfs-parent*"
  "Parent buffer name.")

(defvar bfs-child-buffer-name "*bfs-child*"
  "Child buffer name.")

(defvar bfs-preview-buffer-name "*bfs-preview*"
  "Preview buffer name when we are not visiting a file.
This buffer is used show informations explaining why
we are not previewing `bfs-child' file.")

(defvar-local bfs-max-length
  nil
  "Hold the longest length of the concatenation of an entry and its info.
Entries are filenames (not the pathes), and infos can be file sizes, or
any information we might want to add on the right of the entry,
in `bfs-child-buffer-name' and `bfs-parent-buffer-name' buffers.

The value of this local variable is computed by the function
`bfs-max-length'.

See: `bfs-insert-ls-child'.")

(defun bfs-insert-ls (dir in-buffer &optional is-root marked-entries)
  "Insert directory listing for DIR.
Leave point after the inserted text.

This function is used to fill `bfs-parent-buffer-name'
and `bfs-child-buffer-name' buffers depending on the
value of IN-BUFFER which can be 'child or 'parent.

If IS-ROOT is non-nil, don't do the listing of DIR, and just
insert DIR in the buffer.

If MARKED-ENTRIES is non-nil, this is a list of the entries
that must be marked in the child buffer (so it only works
with IN-BUFFER equal to 'child).

See functions: `bfs-ls-parent-function', `bfs-ls-child-function',
`bfs-ls-child-filtered', `bfs-format-parent-entry-function',
`bfs-format-child-entry-function'."
  (if is-root
      (progn
        (setq bfs-max-length (bfs-max-length dir 'parent 'is-root))
        (insert (funcall bfs-format-parent-entry-function
                         dir dir bfs-max-length)))
    (let (filenames format-entry)
      (pcase in-buffer
        ('parent
          (setq filenames (funcall bfs-ls-parent-function dir))
          (setq format-entry bfs-format-parent-entry-function)
          (setq bfs-max-length (bfs-max-length dir 'parent)))
        ('child
          (setq filenames (bfs-ls-child-filtered dir))
          (setq format-entry bfs-format-child-entry-function)
          (setq bfs-max-length (bfs-max-length dir 'child))))
      (insert (s-join "\n" (--map (funcall format-entry
                                           it dir bfs-max-length
                                           (and (member it marked-entries) t))
                                  filenames)))))
  (insert "\n"))

(defun bfs-parent-buffer (parent)
  "Produce `bfs-parent-buffer-name' buffer.
The produced buffer contains the listing of the parent directory of
PARENT and put the cursor at PARENT dirname."
  (with-current-buffer (get-buffer-create bfs-parent-buffer-name)
    (unless (bound-and-true-p bfs-parent-mode)
      (bfs-parent-mode))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (cond
       ((f-root-p parent)
        (bfs-insert-ls parent 'parent 'is-root)
        (bfs-goto-entry parent)
        (setq default-directory parent))
       (t (bfs-insert-ls (f-parent parent) 'parent)
          (bfs-goto-entry (f-filename parent))
          (setq default-directory (f-parent parent)))))
    (bfs-line-highlight-parent))
  (bury-buffer bfs-parent-buffer-name))

(defun bfs-child-buffer (parent child-entry &optional marked-entries)
  "Produce `bfs-child-buffer-name' buffer.
The produced buffer contains the listing of the directory PARENT
and put the cursor at CHILD-ENTRY.
If CHILD-ENTRY is nil, cursor is put in the first line (see `bfs-goto-entry')."
  (with-current-buffer (get-buffer-create bfs-child-buffer-name)
    (unless (bound-and-true-p bfs-mode)
      (bfs-mode))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (bfs-insert-ls parent 'child nil marked-entries))
    (setq-local default-directory parent)
    (bfs-goto-entry child-entry)
    ;; `bfs-line-highlight-child' depends on the faces
    ;; font-lock adds to the text in the buffer.  So,
    ;; the buffer must be totally fontify before calling
    ;; `bfs-line-highlight-child'.
    (font-lock-ensure (point-min) (point-max))
    (bfs-line-highlight-child))
  (bury-buffer bfs-child-buffer-name))

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
         (line (propertize parent 'font-lock-face 'bfs-top-parent-directory)))
    (if-let ((target (file-symlink-p child)))
        (-reduce
         #'concat
         `(,line
           ,(propertize filename
                        'font-lock-face (if (bfs-broken-symlink-p child)
                                            'bfs-top-broken-symlink
                                          'bfs-top-symlink-name))
           ,(propertize " -> " 'font-lock-face 'bfs-top-symlink-arrow)
           ,(propertize target
                        'font-lock-face (cond ((bfs-broken-symlink-p child)
                                               'bfs-top-broken-symlink)
                                              ((file-directory-p (file-truename child))
                                               'bfs-top-symlink-directory-target)
                                              (t 'bfs-top-symlink-file-target)))))
      (concat line (propertize filename 'font-lock-face 'bfs-top-child-entry)))))

(defun bfs-top-line-ellipsed (child)
  "Return `bfs-top-line-default' truncated with ellipses at the beginning.
The truncation is done only if `bfs-top-line-default' length showing CHILD
path is greater than the top window width."
  (bfs-top-line-truncate (window-width (plist-get bfs-windows :top))
                         (bfs-top-line-default child)))

(defun bfs-top-buffer (&optional child)
  "Produce `bfs-top-buffer-name' buffer showing CHILD file information."
  (with-current-buffer (get-buffer-create bfs-top-buffer-name)
    (read-only-mode -1)
    (erase-buffer)
    (if-let ((child (or child (bfs-child))))
        (insert (funcall bfs-top-line-function child))
      (insert "No child entry to be previewed"))
    (bfs-top-mode))
  (bury-buffer bfs-top-buffer-name))

(defvar-local bfs-preview-buffer-file-name nil)

(defun bfs-preview-buffer (child reason)
  "Produce `bfs-preview-buffer-name' buffer.
Insert REASON string into the buffer that expresses why we
don't visit CHILD as any regular file."
  (with-current-buffer (get-buffer-create bfs-preview-buffer-name)
    (read-only-mode -1)
    (erase-buffer)
    (insert reason)
    (bfs-preview-mode)
    (if child
        (setq-local bfs-preview-buffer-file-name (file-truename child))
      (setq-local bfs-preview-buffer-file-name 'no-child-entry)))
  (bury-buffer bfs-preview-buffer-name))

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
  (with-selected-frame bfs-frame
    (display-buffer bfs-top-buffer-name
                    bfs-top-window-parameters)))

(defun bfs-preview (child &optional first-time)
  "Preview file CHILD on the right window.
When FIRST-TIME is non-nil, set the window layout."
  (let (preview-window preview-file-buffer (preview-update t))
    (cond ((and (not first-time)
                (null child))
           (bfs-preview-buffer child "No child entry to be previewed"))
          ((and (not first-time)
                (bfs-preview-matches-child-p)
                (not (bfs-broken-symlink-p child)))
           (setq preview-update nil))
          ((member (file-name-extension child) bfs-ignored-extensions)
           (bfs-preview-buffer child
                               (format "File ignored due to its extension: %s"
                                       (file-name-extension child))))
          ((and (file-exists-p child) bfs-max-size
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
               (progn
                 (setq preview-file-buffer
                       (find-file-noselect (or (file-symlink-p child) child)))
                 (setq bfs-visited (-uniq (cons child bfs-visited))))
             (file-error
              (bfs-preview-buffer child (error-message-string err))
              (if first-time
                  (display-buffer (get-buffer bfs-preview-buffer-name)
                                  bfs-preview-window-parameters)
                (display-buffer (get-buffer bfs-preview-buffer-name) t))
              (with-current-buffer bfs-child-buffer-name
                (bfs-line-highlight-child))))))
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
                     (-union (plist-get bfs-state-before :buffer-list)
                             bfs-visited-file-buffers)
                     preview-file-buffer)
              (push preview-file-buffer bfs-visited-file-buffers)))
        (if first-time
            (display-buffer (get-buffer bfs-preview-buffer-name)
                            bfs-preview-window-parameters)
          (display-buffer (get-buffer bfs-preview-buffer-name) t))))
    (bfs-top-update)
    preview-window))

(defun bfs-update (child)
  "Update `bfs' environment according to CHILD file."
  (when (bfs-valid-child-p child)
    (let ((inhibit-message t)
          (parent (f-dirname child))
          (child-entry (f-filename child)))
      (bfs-parent-buffer parent)
      (bfs-child-buffer parent child-entry)
      (bfs-top-update)
      (if (bfs-ls-child-filtered parent)
          (bfs-preview child)
        (bfs-preview nil)))))

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
    bfs-find-file
    bfs-hide-dotfiles)
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
    nil)
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
This doesn't kill buffers in (plist-get bfs-state-before :buffer-list)
that was lived before entering in the `bfs' environment.
See: `bfs-state-before'."
  (-each (-difference bfs-visited-file-buffers
                      (plist-get bfs-state-before :buffer-list))
    'kill-buffer)
  (setq bfs-visited-file-buffers nil))

(defun bfs-clean ()
  "Leave `bfs' environment and clean Emacs state."
  (unless (window-minibuffer-p)
    (setq bfs-is-active nil)
    (remove-function after-delete-frame-functions 'bfs-clean-if-frame-deleted)
    (remove-hook 'window-configuration-change-hook 'bfs-check-environment)
    (remove-hook 'isearch-mode-end-hook 'bfs-isearch-preview-update)
    (remove-hook 'isearch-update-post-hook 'bfs-isearch-preview-update)
    (remove-hook 'window-state-change-hook 'bfs-top-update)
    (setq bfs-frame nil)
    (setq bfs-windows nil)
    (bfs-kill-visited-file-buffers)
    (setq window-sides-vertical
          (plist-get bfs-state-before :window-sides-vertical))
    (setq find-file-run-dired
          (plist-get bfs-state-before :find-file-run-dired))
    (when (bound-and-true-p which-key-mode)
      (setq which-key-popup-type
            (plist-get bfs-state-before :which-key-popup-type)))
    (setq bfs-state-before nil)
    (remove-hook 'dired-mode-hook 'bfs-dired-hide-details)
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

(defvar bfs-state-before nil
  "Store partial emacs user state before entering `bfs' environment.
`bfs-state-before' is a property list used internally where:
  :buffer-list             is for evalutaion of (buffer-list),
  :window-sides-vertical   for the variable `window-sides-vertical',
  :find-file-run-dired     for the variable `find-file-run-dired',
  :which-key-popup-type    for the variable `which-key-popup-type'.")

;;;###autoload
(defun bfs (&optional file)
  "Start a `bfs' (Browse File System) environment in the `selected-frame'.

This pops up a 3 panes (windows) layout that allow you to browse
your file system and preview files.

If FILE (a file name) is given:
- if it is a file, preview it in the right window,
- if it is a directory, list it in the child window.

You can only have one `bfs' environment running at a time.

If you call `bfs' with universal argument, `bfs' starts with
the filename of the `current-buffer' in the child window
(see `bfs-child-default').

If you call `bfs' without universal argument, `bfs' starts with
the last file you've visited in the `bfs' environment
(see `bfs-visited' and `bfs-visit').

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

      ;; set `child'
      (cond (current-prefix-arg
             (setq child (bfs-child-default (current-buffer))))
            (file
             (if (and (file-directory-p file)
                      (bfs-first-valid-child file))
                 (setq child (bfs-first-valid-child file))
               (setq child file))
             ;; to prevent `bfs-check-environment' to check `bfs'
             ;; environment when we are building it for the first time
             (setq this-command 'bfs))
            ((and (car bfs-visited)
                  (bfs-valid-child-p (car bfs-visited)))
             (setq child (car bfs-visited)))
            (t (setq child (bfs-child-default (current-buffer)))))

      ;; active `bfs'
      (condition-case-unless-debug err
          (when (and child (bfs-valid-child-p child))
            (setq bfs-is-active t)
            (window-configuration-to-register :bfs)
            (setq bfs-state-before
                  `(:buffer-list ,(buffer-list)
                    :window-sides-vertical ,window-sides-vertical
                    :find-file-run-dired ,find-file-run-dired))
            (setq window-sides-vertical nil)
            (setq find-file-run-dired t)
            (when (bound-and-true-p which-key-mode)
              (setq bfs-state-before
                    (plist-put bfs-state-before
                               :which-key-popup-type which-key-popup-type))
              (setq which-key-popup-type 'minibuffer))
            (when bfs-dired-hide-details
              ;; the depth 99 is because we want to be sure that
              ;; `bfs-dired-hide-details' is called last and
              ;; so override `dired-hide-details-mode'.
              (add-hook 'dired-mode-hook 'bfs-dired-hide-details 99))
            (bfs-display child)
            (add-function :before after-delete-frame-functions 'bfs-clean-if-frame-deleted)
            (add-hook 'window-configuration-change-hook 'bfs-check-environment)
            (add-hook 'isearch-mode-end-hook 'bfs-isearch-preview-update)
            (add-hook 'isearch-update-post-hook 'bfs-isearch-preview-update)
            (add-hook 'window-state-change-hook 'bfs-top-update))
        (error
         (bfs-quit)
         (message "error with `bfs': %s" err)))))))

;;; Footer

(provide 'bfs)

;;; bfs.el ends here
