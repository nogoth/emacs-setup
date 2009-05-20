(setq user-mail-address "livingood.pw@gmail.com")
(setq user-full-name "Ben Livingood")
(load-library "smtpmail")
(load-library "nnimap")
(load-library "starttls")
(load-library "nnir")

(setq gnus-select-method '(nnimap "imap.gmail.com"
           (nnimap-address "imap.gmail.com")
					            (nnimap-server-port 993)
											           (nnimap-authinfo-file "~/.authinfo")
																      (nnir-search-engine imap)
																			           (nnimap-stream ssl)))



(setq gnus-secondary-select-methods
      '(

      (nnimap "finn.cns.montana.edu"
	      (nnimap-list-pattern ("~/mail/in-SPAM_BUCKET"))
		(nnimap-address "finn.cns.montana.edu")
		(nnimap-server-port 993)
		(nnir-search-engine imap)
		(nnimap-authinfo-file "~/.authinfo")
		(nnimap-stream ssl)
		)
      )
)

(setq smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
smtpmail-smtp-server "smtp.gmail.com"
smtpmail-default-smtp-server "smtp.gmail.com"
send-mail-function 'smtpmail-send-it
message-send-mail-function 'smtpmail-send-it
smtpmail-smtp-service 587
;; smtpmail-auth-credentials '(("smtp.gmail.com"
;; 587
;; "livingood.pw@gmail.com"
;; nil))
)
(add-hook 'gnus-topic-mode-hook 'gnus-topic-mode)

(setq gnus-invalid-group-regexp "[:`'\"]\\|^$")
(setq gnus-ignored-newsgroups "")
(setq gnus-outgoing-message-group "[Google Mail]/Sent Mail")

     (setq gnus-extract-address-components
           'mail-extract-address-components)

(setq mail-yank-prefix "> ")

;; from http://emacs.wordpress.com/category/gnus/

;; (setq gnus-summary-line-format "%U%R%z%a%I%(%[%d:%ub%-23,23f%]%) %s\n")

;;C-h v gnus-summary-line-format

(setq gnus-summary-mark-below 0)
