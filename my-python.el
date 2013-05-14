;;python-mode
;;     from https://code.launchpad.net/python-mode
;(add-path "python-mode")

(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
;;===============Mac OS. Use my version of python
;;(setq py-python-command
;;      "/Library/Frameworks/Python.framework/Versions/2.6/bin/python")

;;-------------------- List functions and classes
;;TODO: Make the list remember last position.
(require 'python)
;;(require 'python-mode)


(defun py-list-defs ()
  (interactive)

  (defun rebind-occur-keys ()
    "Return to the standard occur-mode behaviour"
    (define-key occur-mode-map (kbd "q") 'quit-window)
    (define-key occur-mode-map [return] 'occur-mode-goto-occurrence))

  ;; Modify occur mode to make the function list more usable----------

  ; python buffer
  (setq buffer (window-buffer))

  ;; q deletes the windows
  (define-key occur-mode-map (kbd "q") '(lambda ()
                      (interactive)
                      (switch-to-buffer buffer)
                      (rebind-occur-keys)
                      ))
  ;;return deletes the window and sends you to the function/class
  (define-key occur-mode-map [return] '(lambda ()
					 (interactive)
                                         (setq win (selected-window))
					 (occur-mode-goto-occurrence)
                                         (switch-to-prev-buffer)
                                         (select-window win)
                                         (switch-to-buffer buffer)
                                         (rebind-occur-keys)
                                         ))
  (setq curr-win (selected-window))
  (occur "def\\|class")
  (pop-to-buffer "*Occur*")
  (setq occ-win (selected-window))
  (switch-to-prev-buffer)
  (select-window curr-win)
  (switch-to-buffer "*Occur*")
  )


(define-key python-mode-map (kbd "C-c l") 'py-list-defs)

(defun py-move-beggining-of-line ()
  (interactive)
  (move-beginning-of-line nil)
  (indent-for-tab-command)
  )
(define-key python-mode-map (kbd "C-a") 'py-move-beggining-of-line)
(define-key python-mode-map (kbd "S-C-a") 'move-beginning-of-line) ;;standard command

;;Navigate through funcs easily
(define-key python-mode-map (kbd "C-S-p") 'py-beginning-of-def-or-class)
(define-key python-mode-map (kbd "C-S-n") 'py-end-of-def-or-class)
(define-key python-mode-map (kbd "M-P") 'py-beginning-of-def-or-class)
(define-key python-mode-map (kbd "M-N") 'py-end-of-def-or-class)

;;foo bar -> foo.__name__ = bar.__name__
;;Useful in decorators
(defun py-equal-names ()
  (interactive)
  (backward-word 2)
  (dotimes (i 2)
    (forward-word)
    (insert ".__name__"))
  (backward-word 2)
  (insert "= ")
  (move-end-of-line nil))

(defun py-end-def ()
  (interactive)
  (move-end-of-line nil)
  (insert "):")
  (py-newline-and-indent))
(define-key python-mode-map [C-return] 'py-end-def)

(defun py-todos ()
  (interactive)
  (find-todos-in-buffers "py"))

(defun my-py-hook ()
  (interactive)
  (auto-complete-mode t))

(add-hook 'python-mode-hook 'my-py-hook)

(yas-define-snippets 'python-mode '(
                                    (".." "self.$0" "self")))

;;------------------------------------------------------------
;;   Pyflakes
;;------------------------------------------------------------

;;not working
;;(add-hook 'python-mode-hook '(lambda ()
;; (when (load "flymake" t)
;;          (defun flymake-pyflakes-init ()
;;            (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                               'flymake-create-temp-inplace))
;;               (local-file (file-relative-name
;;                            temp-file
;;                            (file-name-directory buffer-file-name))))
;;              (list "pyflakes" (list local-file))))

;;          (add-to-list 'flymake-allowed-file-name-masks
;;                   '("\\.py\\'" flymake-pyflakes-init)))

;;    (add-hook 'find-file-hook 'flymake-find-file-hook)

;;			      (flymake-mode)))
