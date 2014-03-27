;============================================================
;; Basic functionality.
;;============================================================

(defvar platform "linux")

(defun is-mac () (string= platform "mac"))
(defun is-win () (string= platform "win"))
(defun is-linux () (string= platform "linux"))

(if (is-mac) (setq ns-command-modifier (quote meta)))

(server-start)
(setq completion-styles '(partial-completion initials))
(setq completion-pcm-complete-word-inserts-delimiters t)
(ido-ubiquitous-mode t)
(set-frame-parameter (selected-frame) 'alpha '(98 100))  ;; Transparency
(tool-bar-mode 0)                                        ;; Disable ugly toolbar
(setq use-file-dialog nil)                               ;; No GUI file dialogs
(setq inhibit-startup-message t)                         ;; No emacs logo
(setq tags-revert-without-query 1)                       ;; Auto-load tags file
(winner-mode t)                                          ;; Undo window mess-ups
(global-set-key (kbd "M-o") 'ido-find-file)              ;; Better find-file
(global-set-key (kbd "M-s") 'save-buffer)                ;; Better save buffer
(global-set-key (kbd "C-3") 'ido-switch-buffer)          ;; Switch buffers easily
(global-set-key "\C-m" 'newline-and-indent)              ;; Always auto-indent
(global-set-key "\C-x\C-m" 'execute-extended-command)    ;; Better than M-x
(show-paren-mode 1)                                      ;; I like highlighted parens
(fset 'yes-or-no-p 'y-or-n-p)                            ;; quick prompt for 'yes or no'
(put 'scroll-left 'disabled nil)                         ;; Scrolling right-left
(transient-mark-mode t)                                  ;; Show regions with color
(global-linum-mode t)                                    ;; Show line numbers
(global-set-key (kbd "C-c a") 'org-agenda)
(setq visible-bell t)                                    ;; Visible bell
(setq ring-bell-function '(lambda ()))


;; Decent starting frame size:

(when window-system (set-frame-size (selected-frame) 180 54))

;;Switch windows easily. C-x o is too much work.
(global-set-key "\M-+" 'other-window)
(global-set-key "\M-_" '(lambda ()
              (interactive)
              (other-window -1)))
(global-set-key (kbd "C-S-l") 'other-window)
(global-set-key (kbd "C-S-h") '(lambda ()
              (interactive)
              (other-window -1)))
(global-set-key "\M-p" 'backward-kill-word)

;; Window resizing
(global-set-key (kbd "C-{") '(lambda () (interactive) (shrink-window-horizontally 5)))
(global-set-key (kbd "C-}") '(lambda () (interactive) (enlarge-window-horizontally 5)))
(global-set-key (kbd "C-M-{") '(lambda () (interactive) (shrink-window 2)))
(global-set-key (kbd "C-M-}") '(lambda () (interactive) (enlarge-window 2)))

;; List functions
(defun local-tags ()
  (interactive)
  (list-tags (buffer-file-name))
  (select-window (get-buffer-window "*Tags List*"))
  (local-set-key [return] '(lambda () (interactive) (find-tag (thing-at-point 'symbol))))
  (isearch-forward))
(global-set-key (kbd "C-c l") 'local-tags)

(defun find-todos-in-buffers (regexp)
  (interactive "MBuffers matching regexp: ")
  (multi-occur-in-matching-buffers regexp  "\\(todo\\|TODO\\)"))

(defun duplicate-line ()
  (interactive)
  (move-beginning-of-line nil)
  (kill-line nil)
  (yank)
  (newline)
  (yank))
(global-set-key "\C-x\C-d" 'duplicate-line)

;; -----------------------------
;; Use M-n to do "forward-up-list"
(global-set-key (kbd "M-n") '(lambda ()
			       (interactive)
			       (backward-up-list)
			       (evil-normal-state)
			       (evil-jump-item)
			       (evil-append 1)))

(defun shell-command-on-buffer (cmd)
  (interactive "MCommand: ")
  (save-excursion
    (shell-command-on-region (beginning-of-buffer)
			     (end-of-buffer) cmd)))
(global-set-key (kbd "C-M-|") 'shell-command-on-buffer)

;; Command to run when compilation is succesful
(defvar my-working-command "" "*Program to run after compilation")

;; Auto-hide compile window && run command
(setq compilation-window-height 12)
(setq mode-compile-always-save-buffer-p t
      compilation-finish-functions
      (list
       (lambda (buf str)
         (unless (string-match "exited abnormally" str)
           (run-at-time
              "1 sec" nil 'delete-windows-on
              (get-buffer-create "*compilation*"))
           (shell-command my-working-command (get-buffer-create "*program-output*"))))))

;; Place backups somewhere I can't see them...
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)

;; Nice fonts
(if (is-mac)
    (set-default-font "Monaco-10"))
(if (is-win)
    (set-default-font "Consolas-10"))

;;================================================================================
;; Packages
;;================================================================================

(require 'package)

(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

;; home bin dir
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; The power of the dark side...
(evil-mode)
;; jj to escape
(require 'key-chord)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(global-set-key "\M-;" 'evilnc-comment-or-uncomment-lines)
(global-set-key "\M-:" 'evilnc-comment-or-uncomment-to-the-line)
(global-set-key (kbd "C-c -") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c +") 'evil-numbers/dec-at-pt)
(require 'surround)
(global-surround-mode t)

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-;") 'mc/mark-next-like-this)
(global-set-key (kbd "C-:") 'mc/mark-previous-like-this)
;; ======== Ace jump mode
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))

(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)

;(require 'auto-complete-config)
;(ac-config-default)

(require 'autopair)
(autopair-global-mode t)
(setq autopair-autowrap t)

;; (color-theme-initialize)

;; (require 'monokai-theme)
;; (setq solarized-bold nil) ;; I think this makes it faster.
;; ;; Select theme based on the time of day.
;; (let ((hour (string-to-int (first (split-string (nth 4 (split-string (current-time-string) " ")) ":")))))
;;   (message (concat "Hour: " (int-to-string hour)))
;;   (if (or (>=
;;            hour 18)
;;           (<=
;;            hour 8))
;;     (load-theme 'solarized-dark)
;;     (load-theme 'solarized-light)))

;; ======== yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs "~/emacs-config/snippets")
(yas-load-directory "~/emacs-config/snippets")
(yas-global-mode 1)

;; ======= ack
(require 'ack-and-a-half)
;; Create shorter aliases
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)
(setq ack-and-a-half-executable "/usr/local/bin/ack")

;; ======= magit
(global-set-key (kbd "C-c C-v") 'magit-status)

;; ==== C++
; (load-file "~/emacs-config/my-cpp.el")

;; ==== Python
;(load-file "~/emacs-config/my-python.el")

; ==== Slime
(cond
 ((is-mac) (progn
             (setq slime-lisp-implementation '((sbcl ("/usr/local/bin/sbcl"))))
             (setq inferior-lisp-program "/usr/local/bin/sbcl")))
 ((is-linux) (setq slime-lisp-implementation '((sbcl ("sbcl"))))))

; ==== Clojure


;;(setq slime-default-lisp "sbcl")
;;(setq inferior-lisp-program "sbcl")

;;(setq slime-auto-connect 'ask)
;;(add-hook 'slime-mode-hook 'set-up-slime-ac)
;;(add-hook 'comint-mode-hook 'set-up-slime-ac)
;;(add-hook 'comint-mode-hook 'auto-complete-mode)
;;(eval-after-load "auto-complete"
;;  '(add-to-list 'ac-modes 'slime-repl-mode))
;;(defvar slime-setup-done nil)
;;(defun slime-setup-once ()
;;  (unless slime-setup-done
;;    (require 'slime)
;;    (slime-setup)
;;    (setq slime-setup-done t)))
;;(defadvice lisp-mode (before slime-setup-once activate)
;;  (slime-setup-once))

;;(define-key lisp-mode-map (kbd "C-M-u") '(lambda ()
;;                                           (interactive)
;;                                            (progn
;;                                              (backward-up-list)
;;                                              (forward-sexp 1))))

;; Trailing whitespace
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

(global-company-mode t)
(setq company-idle-delay t)
(color-theme-monokai)
(find-file "~/Dropbox/log.txt")
