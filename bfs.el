;;; Packages

(require 's)
(require 'f)

;;; Movements

(defvar bfs-backward-last-visited nil
  "Alist of (PARENT . CHILD-ENTRY) that have been visited.
The cons (PARENT . CHILD-ENTRY) is added each time we use
`bfs-backward'.  PARENT must be uniq.  This allow `bfs-forward' to
lookup `bfs-backward-last-visited' and make a better choice of
the child-entry we want the cursor to be, given a PARENT we
want to visite with a \"bfs forward movement\".")

(defun bfs-backward-has-been-visited (parent)
  "Return child-entry from `bfs-backward-last-visited' if (PARENT . child-entry)
is found in `bfs-backward-last-visited'.
Return nil if not."
  (cdr (--first (f-equal-p (car it) parent)
                bfs-backward-last-visited)))

(defun bfs-backward-update-visited (parent child-entry)
  "Add (PARENT . CHILD-ENTRY) to `bfs-backward-last-visited'."
  (setq bfs-backward-last-visited
        (cons (cons parent child-entry)
              (--remove (f-equal-p parent (car it))
                        bfs-backward-last-visited))))

(defun bfs-previous ()
  "Preview previous file."
  (interactive)
  (unless (bobp) (previous-line))
  (bfs-preview default-directory (bfs-child-entry)))

(defun bfs-next ()
  "Preview next file."
  (interactive)
  (unless (= (line-number-at-pos) (1- (line-number-at-pos (point-max))))
    (next-line))
  (bfs-preview default-directory (bfs-child-entry)))

(defun bfs-backward ()
  "Browse the parent directory."
  (interactive)
  (let ((new-parent
         (if (f-root-p default-directory) "/" (f-parent default-directory)))
        (new-child-entry (bfs-parent-entry)))
    (bfs-backward-update-visited default-directory (bfs-child-entry))
    (bfs-update new-parent new-child-entry)))

(defun bfs-forward ()
  "Browse current child entry if it is a directory.
If not, leave bfs and visit child-entry file."
  (interactive)
  (let* ((child-entry (bfs-child-entry))
         (child-entry-path (f-join default-directory child-entry))
         new-parent new-child-entry)
    (cond ((f-directory-p child-entry)
           (setq new-parent child-entry-path)
           (setq new-child-entry
                 (or (bfs-backward-has-been-visited new-parent)
                     (bfs-first-listed new-parent)))
           (message "%s" new-child-entry)
           (bfs-update new-parent new-child-entry))
          (t
           (bfs-clean)
           (delete-other-windows)
           ;; FIXME: when trying to open a file I'm not
           ;; permitted. For instance, when I'm not the root user
           ;; and the file has this permission: drwx------
           (find-file child-entry-path)))))

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
  (bfs-preview default-directory (bfs-child-entry)))

(defun bfs-scroll-up-half-window ()
  "Scroll child window up of half window height."
  (interactive)
  (scroll-up (bfs-half-window-height))
  (if (eobp) (bfs-previous)
    (bfs-preview default-directory (bfs-child-entry))))

(defun bfs-beginning-of-buffer ()
  "Move to beginning of buffer."
  (interactive)
  (call-interactively 'beginning-of-buffer)
  (bfs-preview default-directory (bfs-child-entry)))

(defun bfs-end-of-buffer ()
  "Move to beginning of buffer."
  (interactive)
  (call-interactively 'end-of-buffer)
  (if (eobp) (bfs-previous)
    (bfs-preview default-directory (bfs-child-entry))))

;;; Utilities

(defun bfs-child-entry ()
  "Return the current child entry."
  (buffer-substring-no-properties (point-at-bol) (point-at-eol)))

(defun bfs-parent-entry ()
  "Return the current parent entry."
  (f-filename default-directory))

(defun bfs-first-listed (parent)
  "Return the first listed file of PARENT directory."
  (car (s-lines
        (shell-command-to-string
         (s-concat "ls -Ap --group-directories-first " parent)))))

(defun bfs-child-entry-initial (buffer)
  "Return the file name of BUFFER.
If BUFFER is not attached to a file, return the first listed file
of `default-directory'."
  (with-current-buffer buffer
    (if-let (bfn (or (buffer-file-name) (dired-file-name-at-point)))
        (f-filename bfn)
      (bfs-first-listed default-directory))))

(defun bfs-goto-entry (entry)
  "Move the cursor to the line ENTRY."
  (beginning-of-buffer)
  (search-forward-regexp (s-concat "^" entry) nil t)
  (beginning-of-line))

;;; Create, display and update buffers

(defvar bfs-parent-buffer-name "*bfs-parent*"
  "Parent buffer name.")

(defvar bfs-child-buffer-name "*bfs-child*"
  "Child buffer name.")

(defvar bfs-parent-window-parameters
  '(display-buffer-in-side-window
    (side . left)
    (window-width . 0.2)
    (window-parameters . ((no-other-window . t)))))

(defvar bfs-child-window-parameters '(display-buffer-same-window))

(defvar bfs-preview-window-parameters
  '(display-buffer-in-direction
    (direction . right)
    (window-width . 0.6)
    (window-parameters . ((no-other-window . t)))))

(defun bfs-parent (parent)
  "Produce *bfs-parent* buffer with the listing
of the directory containing PARENT directory."
  (with-current-buffer (get-buffer-create bfs-parent-buffer-name)
    (read-only-mode -1)
    (erase-buffer)
    (cond ((f-root-p parent) (insert "/") (bfs-goto-entry "/"))
          (t (insert (shell-command-to-string
                      (s-concat "ls -Ap --group-directories-first "
                                (f-parent parent))))
             (bfs-goto-entry (f-filename parent))))
    (bfs-mode parent)))

(defun bfs-child (parent child-entry)
  "Produce `bfs-child-buffer-name' buffer with the listing
of the directory PARENT and the cursor at CHILD entry."
  (with-current-buffer (get-buffer-create bfs-child-buffer-name)
    (read-only-mode -1)
    (erase-buffer)
    (insert (shell-command-to-string
             (s-concat "ls -Ap --group-directories-first " parent)))
    (bfs-goto-entry child-entry)
    (bfs-mode parent)))

(defun bfs-preview (parent child-entry &optional first-time)
  "Preview file CHILD of PARENT.
When FIRST-TIME is non-nil, set the window layout."
  ;; TODO: a way to not try to display with specific extension
  ;;       like "mp4", "mkv"... and the files that are too huge.
  ;;       Take care, if done wrong this can slowdown (freeze) emacs.
  ;;       You can profile emacs if you need with: profiler-start
  ;;       and profiler-report
  (let ((preview-window
         (display-buffer
          (find-file-noselect (f-join parent child-entry))
          (if first-time bfs-preview-window-parameters t))))
    (push (window-buffer preview-window) bfs-visited-file-buffers)
    preview-window))

(defun bfs-preview-update ()
  "Update the preview window with the current child entry file.

Intended to be added to `isearch-update-post-hook' and
`isearch-mode-end-hook'.  This allows to preview the file the
cursor has moved to using \"isearch\" commands in
`bfs-child-buffer-name' buffer."
  (bfs-preview default-directory (bfs-child-entry)))

(defun bfs-update (parent child-entry)
  "Update \"bfs\" buffers according to the argument PARENT and
CHILD-ENTRY."
  (bfs-parent parent)
  (bfs-child parent child-entry)
  (bfs-preview parent child-entry))

(defvar bfs-frame nil
  "Frame where the bfs environment has been started.
Used internally.")

(defvar bfs-environment-is-on-p nil
  "t means that `bfs' environment has been turned on
in the frame `bfs-frame'.
Used internally.")

(defun bfs-display (parent child-entry)
  "Display \"bfs\" buffers in a 3 panes layout for PARENT and
CHILD-ENTRY arguments."
  (delete-other-windows)
  (bfs-parent parent)
  (bfs-child parent child-entry)
  (display-buffer bfs-parent-buffer-name bfs-parent-window-parameters)
  (display-buffer bfs-child-buffer-name bfs-child-window-parameters)
  (bfs-preview parent child-entry t)
  (setq bfs-frame (selected-frame)))

;;; Find a file

(defun bfs-find-file (filename &optional wildcards)
  "Find a file with your completion framework and update `bfs' environment."
  (interactive (find-file-read-args "Find file: " t))
  (cond ((f-root-p filename)
         (bfs-update "/" "/"))
        (t
         (bfs-update (f-dirname filename) (f-filename filename)))))

;;; Leave bfs

(defun bfs-preview-buffer-does-not-match-child-entry (preview-buffer-name)
  "Return t if PREVIEW-BUFFER-NAME buffer in the bfs preview window
doesn't match with the child entry."
  (let ((child-entry-path
         (with-current-buffer bfs-child-buffer-name
           (f-join default-directory (bfs-child-entry))))
        preview-major-mode preview-directory preview-bfn)
    (with-current-buffer preview-buffer-name
      (setq preview-major-mode major-mode)
      (setq preview-directory default-directory)
      (setq preview-bfn (buffer-file-name)))
    (cond ((equal preview-major-mode 'dired-mode)
           (not (f-equal-p child-entry-path preview-directory)))
          (preview-bfn
           (not (f-equal-p child-entry-path preview-bfn)))
          (t t))))

(defun bfs-environment-is-corrupted-p ()
  "Return t if the frame environment isn't a conform `bfs' environment.
For instance, any command that change the frame layout lead to a
corrupted `bfs' environment."
  (let* ((window-buffer-name-list
          (--map (buffer-name (window-buffer it)) (window-list)))
         (bfs-child-buffer-is-not-displayed
          (not (-contains-p window-buffer-name-list bfs-child-buffer-name)))
         (bfs-parent-buffer-is-not-displayed
          (not (-contains-p window-buffer-name-list bfs-parent-buffer-name)))
         (wrong-amount-of-displayed-windows
          (not (equal (length window-buffer-name-list) 3)))
         (preview-buffer-name
          (car (--remove (member it `(,bfs-child-buffer-name ,bfs-parent-buffer-name))
                         window-buffer-name-list))))
    (cond
     ((window-minibuffer-p) nil)
     ((not (eq (selected-frame) bfs-frame)) nil)
     ((memq last-command '(bfs bfs-backward bfs-forward bfs-find-file)) nil)
     ((or bfs-child-buffer-is-not-displayed
          bfs-parent-buffer-is-not-displayed
          wrong-amount-of-displayed-windows)
      t)
     ((bfs-preview-buffer-does-not-match-child-entry preview-buffer-name) t)
     (t nil))
    ))

(defun bfs-check-environment ()
  "Leave `bfs' environment if it is corrupted.
Intended to be used in `window-configuration-change-hook'.
This ensure not to end in an inconsistent state.

When you are in `bfs' environment you Browse the File System.
When you have the insight you want on the file system, you
leave `bfs'environment.  If you want to modify your File System,
use `dired' and any shell in your favorite terminal emulator.

See `bfs-environment-is-corrupted-p'."
  (when (bfs-environment-is-corrupted-p)
    (bfs-clean)
    (delete-other-windows)))

(defun bfs-done-if-frame-deleted (frame)
  "Clean `bfs' environment if the frame that was running it has been deleted.
Intended to be added to `after-delete-frame-functions'."
  (unless (frame-live-p bfs-frame)
    (bfs-clean)))

(defvar bfs-visited-file-buffers nil
  "List of live buffers visited with `bfs-preview' function
during a `bfs' session.
Used internally.")

(defvar bfs-buffer-list nil
  "List of all live buffers when entering in the `bfs' environment.
Used internally.")

(defun bfs-kill-visited-file-buffers ()
  "Kill the buffers used to preview files with `bfs-preview'.
This doesn't kill buffers in `bfs-buffer-list' that was lived
before entering in the `bfs' environment."
  (-each (-difference bfs-visited-file-buffers bfs-buffer-list)
    'kill-buffer)
  (setq bfs-visited-file-buffers nil)
  (setq bfs-buffer-list nil))

(defun bfs-clean ()
  "Leave `bfs' environment and clean emacs state."
  (unless (window-minibuffer-p)
    (setq bfs-environment-is-on-p nil)
    (remove-function after-delete-frame-functions 'bfs-done-if-frame-deleted)
    (remove-hook 'isearch-mode-end-hook 'bfs-preview-update)
    (remove-hook 'isearch-update-post-hook 'bfs-preview-update)
    (remove-hook 'window-configuration-change-hook 'bfs-check-environment)
    (kill-new (f-join default-directory (bfs-child-entry)))
    (setq bfs-backward-last-visited nil)
    (setq bfs-frame nil)
    (bfs-kill-visited-file-buffers)
    (when (get-buffer bfs-parent-buffer-name)
      (kill-buffer bfs-parent-buffer-name))
    (when (get-buffer bfs-child-buffer-name)
      (kill-buffer bfs-child-buffer-name))))

(defun bfs-quit ()
  "Leave `bfs-mode' and restore previous window configuration."
  (interactive)
  (bfs-clean)
  (jump-to-register :bfs))

;;; bfs-mode

(defface bfs-directory
  '((t (:foreground "#458b74")))
  "Face used for subdirectories.")

(defvar bfs-directory-face 'bfs-directory
  "Face name used for subdirectories.")

(defvar bfs-re-dir ".*/$")

(defvar bfs-font-lock-keywords
  `((,bfs-re-dir . bfs-directory-face)))

(defvar bfs-hl-line-background "#394851"
  "Background color of `hl-line' in `bfs-mode'.")

(defvar bfs-hl-line-foreground "#cfcdba"
  "forground color of `hl-line' in `bfs-mode'.")

(defvar bfs-hl-line-is-bold t
  "Whether `hl-line' must be 'bold or not in `bfs-mode'.")

(defvar bfs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") 'bfs-previous)
    (define-key map (kbd "n") 'bfs-next)
    (define-key map (kbd "b") 'bfs-backward)
    (define-key map (kbd "f") 'bfs-forward)
    (define-key map (kbd "RET") 'bfs-forward)

    (define-key map (kbd "d") 'bfs-scroll-preview-down-half-window)
    (define-key map (kbd "s") 'bfs-scroll-preview-up-half-window)
    (define-key map (kbd "u") 'bfs-scroll-down-half-window)
    (define-key map (kbd "i") 'bfs-scroll-up-half-window)
    (define-key map (kbd "<") 'bfs-beginning-of-buffer)
    (define-key map (kbd ">") 'bfs-end-of-buffer)

    (define-key map (kbd "C-f") 'bfs-find-file)

    (define-key map (kbd "D") (lambda () (interactive) (dired default-directory)))
    (define-key map (kbd "T") (lambda () (interactive) (ansi-term "/bin/bash")))

    (define-key map (kbd "q") 'bfs-quit)
    map)
  "Keymap for `bfs-mode'.")

(defun bfs-mode (&optional parent)
  "Mode use in `bfs-child-buffer-name' and `bfs-parent-buffer-name'
buffers when `bfs' environment is \"activated\" with `bfs' command.

See `bfs-child' and `bfs-parent' commands."
  (interactive)
  (kill-all-local-variables)
  (setq default-directory (or parent default-directory))
  (use-local-map bfs-mode-map)
  (hl-line-mode)
  (face-remap-add-relative 'hl-line :background bfs-hl-line-background)
  (face-remap-add-relative 'hl-line :foreground bfs-hl-line-foreground)
  (if bfs-hl-line-is-bold (face-remap-add-relative 'hl-line 'bold))
  (setq major-mode 'bfs-mode
        mode-name "bfs"
        buffer-read-only t)
  (setq-local font-lock-defaults '(bfs-font-lock-keywords t nil nil)))

;;; bfs (main entry)

(defun bfs ()
  "Pop 3 panes frame to browse file system and preview files
from `current-buffer'. "
  (interactive)
  (cond
   (bfs-environment-is-on-p
    (when (eq (selected-frame) bfs-frame)
      (bfs-quit)))
   (t
    (setq bfs-environment-is-on-p t)
    (window-configuration-to-register :bfs)
    (setq bfs-buffer-list (buffer-list))
    (let* ((parent default-directory)
           (child-entry-initial (bfs-child-entry-initial (current-buffer))))
      (bfs-display parent child-entry-initial))
    (add-function :before after-delete-frame-functions 'bfs-done-if-frame-deleted)
    (add-hook 'isearch-mode-end-hook 'bfs-preview-update)
    (add-hook 'isearch-update-post-hook 'bfs-preview-update)
    (add-hook 'window-configuration-change-hook 'bfs-check-environment))))

(global-set-key (kbd "M-]") 'bfs)

;;; Footer

(provide 'bfs)
