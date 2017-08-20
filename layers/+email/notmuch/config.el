(when (configuration-layer/layer-used-p 'notmuch)
  ;; multiplesmtpaccounts
  ;; also refer to http://emacswiki.org/emacs/MultipleSMTPAccounts

  (defvar smtp-accounts nil
    "List of available SMTP Accounts

Upon sending a message via SMTP the entry which matches the message's from-header is used to set the smtp configuration.

An entry is a list of:
\(address full-name host user service\)

The corresponding credentials are looked for in ~/.authinfo.gpg.

Example:
'\(\(\"coroa@online.de\" \"Jonas Hörsch\" \"smtp.gmail.com\" \"coroan\" \"submission\"\)
  \(\"coroa@0x2c.org\" \"Jonas Hörsch\" \"smtp.0x2c.org\" \"coroa\" \"submission\"\)\)")
  )
