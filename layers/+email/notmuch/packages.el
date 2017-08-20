;;; packages.el --- notmuch layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst notmuch-packages
  '(notmuch
    helm-notmuch
    bbdb)
  "The list of Lisp packages required by the notmuch layer.")

(defun notmuch/init-notmuch ()
  (use-package notmuch
    :defer t
    :init
    (spacemacs/set-leader-keys "an" 'notmuch)
    :config
    (progn
      (dolist (prefix '(("ms" . "stash")
                        ("mp" . "part")))
        (spacemacs/declare-prefix-for-mode 'notmuch-show-mode (car prefix) (cdr prefix)))

      (spacemacs/set-leader-keys-for-major-mode 'notmuch-show-mode
        ;; part
        ;; "p?" 'notmuch-subkeymap-help
        "pm" 'notmuch-show-choose-mime-of-part
        "p|" 'notmuch-show-pipe-part
        "po" 'notmuch-show-interactively-view-part
        "pv" 'notmuch-show-view-part
        "ps" 'notmuch-show-save-part
        ;; stash
        ;; "s?" 'notmuch-subkeymap-help
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
        )

      ;; (evilified-state-evilify notmuch-hello-mode notmuch-hello-mode-map
      ;;   (kbd "J") 'notmuch-jump-search
      ;;   )
      (evilified-state-evilify-map notmuch-show-mode-map
        :mode notmuch-show-mode
        :bindings
        "j" 'notmuch-show-next-open-message
        "J" 'notmuch-show-next-message
        "k" 'notmuch-show-previous-open-message
        "K" 'notmuch-show-previous-message
        "l" 'notmuch-show-filter-thread
        )

      (spacemacs/set-leader-keys-for-major-mode notmuch-show-mode-map
        "j" 'notmuch-jump-search)
      ;;   (kbd "J") 'notmuch-jump-search
      ;;   (kbd "N") 'notmuch-show-next-message
      ;;   (kbd "n") 'notmuch-show-next-open-message
      ;;   (kbd "T") 'spacemacs/notmuch-trash-show)

      (evilified-state-evilify-map notmuch-search-mode-map
        :mode notmuch-search-mode
        :bindings
        "C-d" 'notmuch-search-scroll-down
        "C-u" 'notmuch-search-scroll-up
        "gg" 'notmuch-search-first-thread
        "G" 'notmuch-search-last-thread
        "k" 'notmuch-search-previous-thread
        "j" 'notmuch-search-next-thread
        )
      ;; (evilified-state-evilify notmuch-tree-mode notmuch-tree-mode-map)
      ;; (evilified-state-evilify notmuch-search-mode notmuch-search-mode-map
      ;;   ;TODO: [[file:~/dotfiles/spacemacs/load-conf-files/notmuch.el][file:~/dotfiles/spacemacs/load-conf-files/notmuch.el]]
      ;;   (kbd "a") 'spacemacs/notmuch-message-archive
      ;;   (kbd "d") 'spacemacs/notmuch-message-delete-down
      ;;   (kbd "D") 'spacemacs/notmuch-message-delete-up
      ;;   (kbd "J") 'notmuch-jump-search
      ;;   (kbd "L") 'notmuch-search-filter
      ;;   (kbd "gg") 'notmuch-search-first-thread
      ;;   (kbd "G") 'notmuch-search-last-thread
      ;;   (kbd "T") 'spacemacs/notmuch-trash
      ;;   (kbd "M") 'compose-mail-other-frame)

      (evil-define-key 'normal notmuch-search-mode-map
        "F" 'spacemacs/notmuch-remove-inbox-tag)

      (evil-define-key 'visual notmuch-search-mode-map
        "*" 'notmuch-search-tag-all
        "a" 'notmuch-search-archive-thread
        "-" 'notmuch-search-remove-tag
        "+" 'notmuch-search-add-tag
        "F" '(lambda (&optional beg end)
               "archive by removing inbox tag"
               (interactive (notmuch-search-interactive-region))
               (notmuch-search-tag (list "+archive" "-inbox") beg end)))

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
      (require 'epa-mail)
      (defun turn-on-epa-mail-mode ()
        (epa-mail-mode t))
      (add-hook 'notmuch-show-hook 'turn-on-epa-mail-mode)
      (add-hook 'message-mode-hook 'turn-on-epa-mail-mode)

      (spacemacs/set-leader-keys-for-minor-mode 'epa-mail-mode
        "ed"  'spacemacs/notmuch-show-decrypt-message))
      ))

(defun notmuch/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config
    (progn
      (defun turn-on-orgstruct-mode ()
        (orgstruct-mode t)
        (set (make-local-variable 'org-footnote-auto-label) 'plain)
        (local-set-key (kbd "C-c f") 'org-footnote-action))

      (eval-after-load 'notmuch
        (add-hook 'notmuch-show-hook 'turn-on-orgstruct-mode))
      (add-hook 'message-mode-hook 'turn-on-orgstruct-mode)

      ;;; org-notmuch
      (require 'org-notmuch))))

(defun notmuch/init-helm-notmuch ()
  (use-package helm-notmuch
    :defer t))

(defun notmuch/init-bbdb ()
  (use-package bbdb
    :defer t
    :config
    (progn)))



;;; packages.el ends here
