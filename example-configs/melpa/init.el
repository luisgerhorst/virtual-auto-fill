;;; Required:

;; If I test this config on Debian 10 with Emacs 26.1 I get a "Bad Request"
;; error when accessing GNU ELPA.
;;
;; The following fixes this problem (via
;; https://github.com/syl20bnr/spacemacs/issues/12535, likely related to
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341). If you don't have this
;; problem don't copy it!
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Set up MELPA. Code copied from https://melpa.org/#/getting-started
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;
  ;; Virtual Auto Fill does not support melpa-stable! Also, read
  ;; https://www.reddit.com/r/emacs/comments/etikbz/speaking_as_a_package_maintainer_please_do_not/
  ;; before enabling melpa-stable and reevaluate whether this is really the
  ;; thing that solves your problem.
  ;;
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )
(package-initialize)

;; You probably want to skip the following and just run 'M-x package-install
;; virtual-auto-fill' which does all of this for you (modifies
;; `package-selected-packages' using `customize').
(add-to-list 'package-selected-packages 'virtual-auto-fill)
;;
;; Load archives and install packages when Emacs opens for the first time.
(when (not package-archive-contents)
  (package-refresh-contents)
  ;; We'll use the `package-selected-packages' variable to remember installed
  ;; packages if .emacs.d/elpa gets deleted. The following code ensures all
  ;; listed packages are installed. Before calling this function you should
  ;; always call `package-refresh-contents' because otherwise newly pinned
  ;; packages may not be considered. Packages installed by the user
  ;; (e.g. using `package-install') will be added automatically to
  ;; `package-selected-packages' which is stored in `custom-file'.
  (when (version<= "25.1" emacs-version)
    (package-install-selected-packages)))

;; Now M-x virtual-auto-fill-mode work!

;;; Optional:

;; Enable Virtual Auto Fill mode in every Markdown file. To do this we first
;; install `markdown-mode' (usually better to be done using 'M-x package-install
;; markdown-mode'):
(add-to-list 'package-selected-packages 'markdown-mode)
(package-install-selected-packages)
;; ... then wait until Markdown mode is autoloaded (usually happens the first
;; time we open a Markdown file). This makes the variable `markdown-mode-hook'
;; available:
(with-eval-after-load 'markdown-mode
  ;; Finally set up the automatic activation of Virtual Auto Fill mode when we
  ;; enable Markdown mode. The mode will be loaded the first time a Markdown
  ;; file is opened thereby keeping your Emacs startup slick.
  (add-hook 'markdown-mode-hook #'virtual-auto-fill-mode))
