;============================================================
;; Basic functionality.
;;============================================================

(defvar platform "win")

(defun is-mac () (string= platform "mac"))
(defun is-win () (string= platform "win"))
(defun is-linux () (string= platform "linux"))

(if (is-mac) (setq ns-command-modifier (quote meta)))

(server-start)
(ido-mode t)                                           ;; Best. Thing. Ever.
(setq ido-enable-flex-matching t)                      ;; Cool completion for ido
(setq completion-styles '(partial-completion initials))
(setq completion-pcm-complete-word-inserts-delimiters t)
(tool-bar-mode 0)                                      ;; Disable ugly toolbar
(setq use-file-dialog nil)                             ;; No GUI file dialogs
(setq inhibit-startup-message t)                       ;; No emacs logo
(setq tags-revert-without-query 1)                     ;; Auto-load tags file
(winner-mode t)                                        ;; Undo window mess-ups
(global-set-key (kbd "M-o") 'ido-find-file)            ;; Better find-file
(global-set-key (kbd "M-s") 'save-buffer)              ;; Better save buffer
(global-set-key (kbd "C-3") 'ido-switch-buffer)        ;; Switch buffers easily
(global-set-key "\C-m" 'newline-and-indent)            ;; Always auto-indent
(global-set-key "\C-x\C-m" 'execute-extended-command)  ;; Better than M-x
(show-paren-mode 1)                                    ;; I like highlighted parens
(fset 'yes-or-no-p 'y-or-n-p)                          ;; quick prompt for 'yes or no'
(put 'scroll-left 'disabled nil)                       ;; Scrolling right-left
(transient-mark-mode t)                                ;; Show regions with color
(global-linum-mode t)                                  ;; Show line numbers

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


;; Nice mac font
(if (is-mac)
    (set-default-font "Monaco-12"))
(if (is-win)
    (set-default-font "Consolas-10"))


;;================================================================================
;; Packages
;;================================================================================

(require 'package)

(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

;(add-to-list 'package-archives
;	     '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

; Installed packages:
;ack-and-a-half
;auto-complete
;autopair
;color-theme
;color-theme-sol...
;column-marker
;evil-nerd-comme...
;evil-numbers
;ido-ubiquitous
;lua-mode
;popup
;yasnippet


(require 'auto-complete-config)
(ac-config-default)

(require 'autopair)
(autopair-global-mode t)
(setq autopair-autowrap t)

(color-theme-initialize)
(setq solarized-bold nil) ;; I think this makes it faster.
;; Select theme based on the time of day.
(if (>= (string-to-int (first (split-string (nth 3 (split-string (current-time-string) " ")) ":"))) 19)
    (color-theme-solarized-dark)
  (color-theme-solarized-light))

(require 'yasnippet)
(setq yas-snippet-dirs "~/emacs/snippets")
(yas-load-directory "~/emacs/snippets")
(yas-global-mode 1)

(require 'ack-and-a-half)
;; Create shorter aliases
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)
(setq ack-and-a-half-executable "/usr/local/bin/ack")

;; ==== C++

(load-file "~/emacs-config/my-cpp.el")
;(load-file "~/emacs-config/my-python.el")

;; Vim envy
(global-set-key "\M-;" 'evilnc-comment-or-uncomment-lines)
(global-set-key "\M-:" 'evilnc-comment-or-uncomment-to-the-line)
(global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)

;; Trailing whitespace
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))
