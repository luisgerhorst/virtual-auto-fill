;; Required: Install straight.el, copied from https://github.com/raxod502/straight.el#getting-started
;;
;; This is required but your only need it once in your whole config. AOT Virtual
;; Auto Fill mode is not yet available via MELPA (i.e. package.el).
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Required: Install Virtual Auto Fill mode using straight.el
(straight-use-package
 '(virtual-auto-fill
   :type git
   :host github
   :repo "luisgerhorst/virtual-auto-fill"))

;; Optional: Enable Virtual Auto Fill mode in all markdown files. To do this we
;; first install `markdown-mode':
(straight-use-package 'markdown-mode)
;; ... then wait until Markdown mode is autoloaded by straight.el (usually
;; happend the first time we open a markdown file) which makes the variable
;; `markdown-mode-hook' available:
(with-eval-after-load 'markdown-mode
  ;; .. and finally set up the automatic activation of Virtual Auto Fill mode
  ;; when we enable Markdown mode. Calling `virtual-auto-fill-mode' the first
  ;; time will autoload the mode.
  (add-hook 'markdown-mode-hook #'virtual-auto-fill-mode))
