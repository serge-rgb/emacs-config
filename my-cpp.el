
;; File with some semantic helper funcs
(load-file "~/emacs/eassist.el")

;; .h -> c++ not c
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
;; Use c-mode for glsl shaders
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . c-mode))

(setq-default indent-tabs-mode nil)

(setq compile-command "make -j8")

(defun cpp-todos ()
  (interactive)
  (find-todos-in-buffers "\\(cpp\\'\\|cc$\\|h\\'\\|hpp\\'\\|h\\'\\)"))

;; My c-style.
(c-add-style "sergio"
             '("k&r"
               (c-basic-offset . 4)
               (c-offsets-alist
                (innamespace . 0))))

(add-to-list 'load-path "~/emacs/emacs-clang-complete-async")
(require 'auto-complete-clang-async)

(defun ac-cc-mode-setup ()
  (setq ac-clang-complete-executable "~/emacs/emacs-clang-complete-async/clang-complete")
  ;(setq ac-sources '(ac-source-abbrev ac-source-words-in-same-mode-buffers ac-source-gtags ac-source-clang-async))
  ;(setq ac-sources (append '(ac-source-clang-async) ac-sources))
  (setq ac-sources '(ac-source-clang-async))
  (ac-clang-launch-completion-process))

(defun my-ac-config ()
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))

(my-ac-config)

;; Semantic path for c++ stl
(defun my-c-hook ()
  (interactive)

  (c-set-style "sergio")

  ;; Semantic stuff
  (semantic-mode t)
  (global-semantic-decoration-mode t)
  (define-key c-mode-base-map [C-tab] 'eassist-switch-h-cpp)
  (define-key c-mode-base-map (kbd "C-c l") 'eassist-list-methods)
;  (if linux
 ;     (semantic-add-system-include "/usr/include/c++/4.6/"))

  (autopair-mode t)

  ;;-- 80 char rule
  (require 'whitespace)
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face lines-tail))
  (whitespace-mode t)
  (require 'column-marker)
  (column-marker-1 80)
  (column-marker-2 100)
  ;; compile
  (define-key c-mode-base-map [C-f5] 'compile)
  (define-key c-mode-base-map [f5] 'recompile)


  (define-key c-mode-base-map [M-f11] 'recompile)
  (define-key c-mode-base-map [C-S-down] 'c-end-of-defun)
  (define-key c-mode-base-map [C-S-up] 'c-beginning-of-defun)
  (define-key c-mode-base-map (kbd "C-a") (lambda ()
				(interactive)
				(beginning-of-line)
				(indent-for-tab-command)))
  (define-key c-mode-base-map (kbd "S-C-a") 'move-beginning-of-line)
  (define-key c-mode-base-map (kbd "S-C-]") 'next-error)
  (define-key c-mode-base-map (kbd "S-C-[") 'previous-error)
  (define-key c-mode-base-map [M-tab] 'ac-complete))

(add-hook 'c-mode-common-hook 'my-c-hook)
