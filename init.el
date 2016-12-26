;;; setting
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq kill-whole-line t)
(setq visible-bell t)
(setq ring-bell-function 'ignore)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default truncate-lines t)
(setq custom-file (locate-user-emacs-file "custom.el"))

;;; disable scrollbar
(set-scroll-bar-mode nil)

;;; theme
(setq custom-theme-directory "~/.emacs.d/themes/")
(load-theme 'custom t)

;;; func
(load-file "~/.emacs.d/func.el")

;;; for mac
(when (memq window-system '(mac ns))
  (load-file "~/.emacs.d/mac.el"))

;;; frame
(defvar frame-parameters
  '((width . 180)
    (height . 50)
    (top . 64)
    (left . 192)
    (tool-bar-lines . nil)))

(when (memq window-system '(x w32 mac ns))
  (setq frame-title-format '(multiple-frames "%b" ("" invocation-name)))
  (setq default-frame-alist frame-parameters))

;;; paren-mode(対応する括弧の強調表示
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'mixed)

(global-set-key "\C-h" 'backward-delete-char)
(global-set-key "\C-ce" 'eshell)
(global-set-key "\C-x\C-b" 'electric-buffer-list)
(global-set-key "\C-a" 'beggining-of-indented-line)
(global-set-key "\C-k" 'kill-line)
(global-set-key "\M-k" '(lambda () (interactive) (kill-line 0)))
(add-hook 'electric-buffer-menu-mode-hook
          '(lambda()
             (local-set-key "x" 'Buffer-menu-execute)))

;;; scratch自動復活
(add-hook
 'kill-buffer-query-functions
 (lambda ()
   (if (string= "*scratch*" (buffer-name))
       (progn (make-scratch nil) nil)
     t)))
(add-hook
 'after-save-hook
 (lambda ()
   (unless (member (get-buffer "*scratch*") (buffer-list))
     (make-scratch t))))

;;; settings.elをロードする
(add-hook 'find-file-hook 'load-dir-settings)

;;; c-mode
(add-hook 'c-mode-common-hook
          '(lambda()
             (setq c-default-style "stroustrup")
             (setq c-basic-offset 4)
             (setq tab-width 4)
             (setq indent-tabs-mode nil)
             (local-set-key "\C-t" 'ff-find-other-file)
             (local-set-key "\C-h" 'hungry-backspace)
             (local-set-key "\C-d" 'hungry-delete)
             (local-set-key "\C-f" 'hungry-forward-char)
             (local-set-key "\C-b" 'hungry-backward-char)))

;;; c++-mode
(setq auto-mode-alist
      (append
       '(("\\.h$" . c++-mode)
         ("\\.hpp$"     . c++-mode))
       auto-mode-alist))
(add-hook 'c++-mode-hook
          '(lambda()
             (c-set-offset 'innamespace 0)
             (c-set-offset 'arglist-close 0)
             (c-set-offset 'arglist-cont-nonempty 0)))
;; (defadvice c-lineup-arglist (around my activate)
;;   "Improve indentation of continued C++11 lambda function opened as argument."
;;   (setq ad-return-value
;;         (if (and (equal major-mode 'c++-mode)
;;                  (ignore-errors
;;                    (save-excursion
;;                      (goto-char (c-langelem-pos langelem))
;;                      ;; Detect "[...](" or "[...]{". preceded by "," or "(",
;;                      ;;   and with unclosed brace.
;;                      (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
;;             0 ; no additional indent
;;           ad-do-it))) ; default behavior

;;; csharp-mode
(add-hook 'csharp-mode-hook
          '(lambda()
             (local-set-key "\C-t" 'ff-find-other-file)
             (local-set-key "\C-h" 'hungry-backspace)
             (local-set-key "\C-d" 'hungry-delete)
             (local-set-key "\C-f" 'hungry-forward-char)
             (local-set-key "\C-b" 'hungry-backward-char)))

;;; js-mode
(add-hook 'js-mode-hook
          '(lambda()
             (setq js-indent-level 2)
             (setq tab-width 2)
             (setq indent-tabs-mode nil)
             (local-set-key "\C-t" 'ff-find-other-file)
             (local-set-key "\C-h" 'hungry-backspace)
             (local-set-key "\C-d" 'hungry-delete)
             (local-set-key "\C-f" 'hungry-forward-char)
             (local-set-key "\C-b" 'hungry-backward-char)))

;;; white space
(require 'whitespace)
(setq whitespace-style '(face
                         trailing
                         tabs
                         space-mark
                         tab-mark
                         empty))
(set-face-attribute 'whitespace-tab nil
                    :background nil
                    :foreground nil
                    :underline nil)
(set-face-attribute 'whitespace-empty nil
                    :background nil)
(setq whitespace-display-mappings
      '((tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
(setq whitespace-action '(auto-cleanup))

;;; package.el
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(setq package-user-dir "~/.emacs.d/packages/")
(package-initialize)

;; install packages
(eval-when-compile (require 'cl))
(defvar package-list '(ddskk undo-tree csharp-mode web-mode swift-mode yasnippet flycheck flycheck-irony flycheck-pos-tip irony company-irony rtags cmake-ide))
(let ((not-installed
       (loop for x in package-list
             when (not (package-installed-p x))
             collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
      (package-install pkg))))

;;; ddskk
(when (require 'skk nil t)
  (require 'skk-study)
  (setq default-input-method "japanese-skk")
  (add-hook 'isearch-mode-hook 'skk-isearch-mode-setup)
  (add-hook 'isearch-mode-end-hook 'skk-isearch-mode-cleanup)
  (setq skk-isearch-start-mode 'latin))

;;; redo+
(when (require 'undo-tree nil t)
  (global-undo-tree-mode)
  (global-set-key (kbd "C-M-/") 'undo-tree-redo))

;;; web-mode
(when (require 'web-mode nil t)
  (setq auto-mode-alist
        (append
         '(("\\.php$" . web-mode)
           ("\\.tpl$" . web-mode)
           ("\\.html?$"     . web-mode))
         auto-mode-alist))

  (add-hook 'web-mode-hook
            '(lambda()
               (setq web-mode-markup-indent-offset 4)
               (setq web-mode-enable-auto-pairing nil)
               (setq web-mode-enable-auto-quoting nil)
               (setq web-mode-script-padding 0)
               (setq web-mode-style-padding 0)
               (setq indent-tabs-mode nil)
               (local-set-key "\C-t" 'ff-find-other-file)
               (local-set-key "\C-h" 'hungry-backspace)
               (local-set-key "\C-d" 'hungry-delete)
               (local-set-key "\C-f" 'hungry-forward-char)
               (local-set-key "\C-b" 'hungry-backward-char))))

(when (require 'swift-mode nil t)
  (add-hook 'swift-mode-hook
            '(lambda()
               (local-set-key "\C-h" 'hungry-backspace)
               (local-set-key "\C-d" 'hungry-delete)
               (local-set-key "\C-f" 'hungry-forward-char)
               (local-set-key "\C-b" 'hungry-backward-char))))

;; yasnippet
(when (require 'yasnippet nil t)
  (yas-global-mode))

;; flycheck
(when (require 'flycheck nil t)
  (add-hook 'c-mode-common-hook 'flycheck-mode);
  (when (require 'flycheck-irony nil t)
    (add-hook 'flycheck-mode-hook 'flycheck-irony-setup))
  (when (require 'flycheck-pos-tip nil t)
    (flycheck-pos-tip-mode)))

;; company
(when (require 'company nil t)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (define-key company-active-map (kbd "C-h") nil)
  (global-company-mode))

;; company-irony
(when (require 'irony nil t)
  (when (require 'company-irony nil t)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'objc-mode-hook 'irony-mode)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
    (add-to-list 'company-backends 'company-irony)
    (add-hook 'irony-mode-hook
              '(lambda()
                 (define-key irony-mode-map
                   [remap completion-at-point]
                   'irony-completion-at-point-async)
                 (define-key irony-mode-map
                   [remap complete-symbol]
                   'irony-completion-at-point-async)))))

(when (require 'cmake-ide nil t)
 (setq cmake-ide-rdm-executable (executable-find "rdm"))
 (setq cmake-ide-rc-executable  (executable-find "rc")))

(when (require 'rtags nil t)
  (rtags-enable-standard-keybindings c-mode-base-map)
  (cmake-ide-setup))
