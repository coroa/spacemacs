;;; packages.el --- Notmuch Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq notmuch-packages
      '(
        (counsel-notmuch :requires ivy)
        (helm-notmuch :requires helm)
        notmuch
        org
        persp-mode
        window-purpose
        bbdb
        ))

(defun notmuch/init-counsel-notmuch ()
  (use-package counsel-notmuch
    :defer t
    :init (spacemacs/set-leader-keys "aNn" 'counsel-notmuch)))

(defun notmuch/init-helm-notmuch ()
  (use-package helm-notmuch
    :defer t
    :init (spacemacs/set-leader-keys "aNn" 'helm-notmuch)))

(defun notmuch/init-notmuch ()
  (use-package notmuch
    :defer t
    :commands notmuch
    :init
    (progn
      (spacemacs/declare-prefix "aN" "notmuch")
      (spacemacs/set-leader-keys
        "an"  'spacemacs/notmuch-unread
        "aNN" 'notmuch
        "aNi" 'spacemacs/notmuch-inbox
        "aNj" 'notmuch-jump-search
        "aNs" 'notmuch-search))
    :config
    (progn
      (dolist (prefix '(("ms" . "stash")
                        ("mp" . "part")
                        ("mP" . "patch")))
        (spacemacs/declare-prefix-for-mode 'notmuch-show-mode
          (car prefix) (cdr prefix)))
      ;; key bindings
      (spacemacs/set-leader-keys-for-major-mode 'notmuch-show-mode
        "a" 'notmuch-show-save-attachments
        ;; part
        "pm" 'notmuch-show-choose-mime-of-part
        "pp" 'spacemacs/notmuch-show-as-patch
        "p|" 'notmuch-show-pipe-part
        "po" 'notmuch-show-interactively-view-part
        "pv" 'notmuch-show-view-part
        "ps" 'notmuch-show-save-part
        ;; stash
        "sG" 'notmuch-show-stash-git-send-email
        "sL" 'notmuch-show-stash-mlarchive-link-and-go
        "sl" 'notmuch-show-stash-mlarchive-link
        "st" 'notmuch-show-stash-to
        "sT" 'notmuch-show-stash-tags
        "ss" 'notmuch-show-stash-subject
        "sI" 'notmuch-show-stash-message-id-stripped
        "si" 'notmuch-show-stash-message-id
        "sf" 'notmuch-show-stash-from
        "sF" 'notmuch-show-stash-filename
        "sd" 'notmuch-show-stash-date
        "sc" 'notmuch-show-stash-cc
        ;; patch
        "Po" 'spacemacs/notmuch-show-open-github-patch
        "Pa" 'spacemacs/notmuch-git-apply-patch
        "PA" 'spacemacs/notmuch-git-apply-patch-part
        ;; navigation
        "l" 'notmuch-show-filter-thread
        "j" 'notmuch-jump-search)

      ;; evilified maps
      (evilified-state-evilify-map notmuch-hello-mode-map
        :mode notmuch-hello-mode)
      (evilified-state-evilify-map notmuch-show-mode-map
        :mode notmuch-show-mode
        :bindings
        "n" 'notmuch-show-next-open-message
        "C-n" 'notmuch-show-next-message
        "N" 'notmuch-show-previous-open-message
        "C-N" 'notmuch-show-previous-message
        "L" 'notmuch-show-filter-thread
        "J" 'notmuch-jump-search
        (kbd "o")   'notmuch-show-open-or-close-all
        (kbd "O")   'spacemacs/notmuch-show-close-all)

      (evilified-state-evilify-map notmuch-tree-mode-map
        :mode notmuch-tree-mode
        :bindings
        (kbd "d") 'spacemacs/notmuch-message-delete-down
        (kbd "D") 'spacemacs/notmuch-message-delete-up
        (kbd "M") 'compose-mail-other-frame)

      (evil-define-key 'visual notmuch-search-mode-map
        "*" 'notmuch-search-tag-all
        "a" 'notmuch-search-archive-thread
        "-" 'notmuch-search-remove-tag
        "+" 'notmuch-search-add-tag)
      (evilified-state-evilify-map notmuch-search-mode-map
        :mode notmuch-search-mode
        :bindings
        (kbd "a") 'spacemacs/notmuch-search-archive-thread-down
        (kbd "A") 'spacemacs/notmuch-search-archive-thread-up
        (kbd "d") 'spacemacs/notmuch-message-delete-down
        (kbd "D") 'spacemacs/notmuch-message-delete-up
        (kbd "S") 'spacemacs/notmuch-message-spam
        (kbd "J") 'notmuch-jump-search
        (kbd "L") 'notmuch-search-filter
        (kbd "gg") 'notmuch-search-first-thread
        (kbd "gr") 'notmuch-refresh-this-buffer
        (kbd "gR") 'notmuch-refresh-all-buffers
        (kbd "G") 'notmuch-search-last-thread
        (kbd "k") 'notmuch-search-previous-thread
        (kbd "j") 'notmuch-search-next-thread
        (kbd "N") 'notmuch-search-previous-thread
        (kbd "n") 'notmuch-search-next-thread
        (kbd "M") 'compose-mail-other-frame
        (kbd "C-d") 'notmuch-search-scroll-down
        (kbd "C-u") 'notmuch-search-scroll-up
        )

      (setq-default
       notmuch-search-oldest-first nil
       notmuch-show-all-multipart/alternative-parts nil
       notmuch-crypto-process-mime t
       mm-discouraged-alternatives '("text/html" "text/richtext")
       mm-decrypt-option 'known
       message-citation-line-function 'message-insert-formatted-citation-line)

      (defadvice smtpmail-via-smtp
          (around change-smtp-by-message-from-field (recipient buffer &optional ask) activate)
        (with-current-buffer buffer
          (loop with from = (save-excursion
                              (save-restriction
                                (message-narrow-to-headers)
                                (message-fetch-field "from")))
                for (addr fname server user service) in smtp-accounts
                when (string-match addr from)
                return (let ((user-mail-address addr)
                             (user-full-name fname)
                             (smtpmail-smtp-server server)
                             (smtpmail-smtp-user user)
                             (smtpmail-smtp-service service))
                         ad-do-it)
                finally do
                (error (format (concat "address '%s' doesn't match"
                                       " any entry from smtp-accounts.") from)))))

      (setq-default
       message-send-mail-function 'smtpmail-send-it
       smtpmail-stream-type 'starttls
       smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
       mail-specify-envelope-from 'header
       mml2015-sign-with-sender t
       mml2015-encrypt-to-self t)

;;; more visible faces for flagged, unread and deleted

      (setq-default
       notmuch-search-line-faces '(("unread" :weight bold)
                                   ("flagged" :foreground "red")
                                   ("deleted" :foreground "gray60")
                                   ("spam" :foreground "gray60")))

      ;; fixes: killing a notmuch buffer does not show the previous buffer
      (add-to-list 'spacemacs-useful-buffers-regexp "\\*notmuch.+\\*")

      ;;; epa mail mode for decrypting inline PGP
      ;; (require 'epa-mail)
      ;; (defun turn-on-epa-mail-mode ()
      ;;   (epa-mail-mode t))
      ;; (add-hook 'notmuch-show-hook 'turn-on-epa-mail-mode)
      ;; (add-hook 'message-mode-hook 'turn-on-epa-mail-mode)

      (spacemacs/set-leader-keys-for-minor-mode 'epa-mail-mode
        "ed"  'spacemacs/notmuch-show-decrypt-message)

      (setq notmuch-address-use-company nil
            notmuch-address-command nil)
      (bbdb-initialize 'gnus 'message)

      (defun turn-on-orgstruct-mode ()
        (orgstruct-mode t)
        (set (make-local-variable 'org-footnote-auto-label) 'plain)
        (local-set-key (kbd "C-c f") 'org-footnote-action))

      (add-hook 'notmuch-show-hook 'turn-on-orgstruct-mode)
      (add-hook 'message-mode-hook 'turn-on-orgstruct-mode)
      )))

(defun notmuch/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config
    (require 'org-notmuch)
    (require 'org-footnote)))

(defun notmuch/pre-init-persp-mode ()
  (spacemacs|use-package-add-hook persp-mode
    :post-config
    (progn
      (add-to-list 'persp-filter-save-buffers-functions
                   'spacemacs//notmuch-persp-filter-save-buffers-function)
      (spacemacs|define-custom-layout notmuch-spacemacs-layout-name
        :binding notmuch-spacemacs-layout-binding
        :body
        (progn
          (dolist (mode notmuch-modes)
            (let ((hook (intern (concat (symbol-name mode) "-hook"))))
              (add-hook hook #'spacemacs//notmuch-buffer-to-persp)))
          (call-interactively 'notmuch))))))

(defun notmuch/pre-init-window-purpose ()
  (spacemacs|use-package-add-hook window-purpose
    :pre-config
    (dolist (mode notmuch-modes)
      (add-to-list 'purpose-user-mode-purposes (cons mode 'mail)))))

(defun notmuch/init-bbdb ()
  (use-package bbdb
    :defer t
    :config
    (progn
      (setq-default
       bbdb-auto-revert t
       bbdb-check-auto-save-file t
       bbdb-expand-mail-aliases t
       bbdb-complete-mail-allow-cycling t
       bbdb-phone-style nil
       bbdb-pop-up-window-size 10))))
