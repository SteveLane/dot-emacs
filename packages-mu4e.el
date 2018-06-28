;; Time-stamp: <2018-06-28 20:17:45 (slane)>
;; Commands to load mu4e related stuff
(use-package mu4e
  ;; Must be installed first...
  :load-path "/usr/local/Cellar/mu/1.0/share/emacs/site-lisp/mu/mu4e"
  :config
  (setq mu4e-sent-folder "/unimelb/Sent Items"
	mu4e-drafts-folder "/unimelb/Drafts"
	mu4e-trash-folder "/unimelb/Deleted Items/")
  ;; Better behaviour with moving on mbsync
  (setq mu4e-change-filenames-when-moving t
	;; HTML
	mu4e-view-prefer-html t
	;; Get email using mbsync every 60 minutes
	mu4e-get-mail-command "mbsync -aq"
	mu4e-update-interval 3600
	;; For wrapping nicer in other clients
	mu4e-compose-format-flowed t
	;; ;; 'fancy' characters for marks and threads
	;; mu4e-use-fancy-chars t
	;; display attached images
	mu4e-view-show-images t
	;; shows email addresses rather than only names (may want to change later)
	mu4e-view-show-addresses t
	;; shortcuts access with 'j' ('jump')
	mu4e-maildir-shortcuts '(("/unimelb/Inbox" . ?i)
				 ("/unimelb/Sent Items" . ?s)
				 ("/unimelb/Drafts" . ?d))
	;; don't keep message buffers around
	message-kill-buffer-on-exit t
	;; default save directory for attachments
	mu4e-attachment-dir "~/Downloads"
	;; default sending
	send-mail-function 'smtpmail-send-it
	message-send-mail-function 'smtpmail-send-it
	;; Some user details
	user-full-name "Steve Lane"
	mu4e-compose-signature (concat
				"Steve Lane\n"
				"P // +61 3 8344 0071\n"
				"W // https://cebra.unimelb.edu.au/\n"
				"W // https://gtown-ds.netlify.com/\n"
				"T // https://twitter.com/stephenelane/\n")
	;; smtpmail-auth-credentials
	user-mail-address "lane.s@unimelb.edu.au"
	smtpmail-smtp-user "lane.s@unimelb.edu.au"
	smtpmail-default-smtp-server "smtp.office365.com"
	smtpmail-local-domain "office365.com"
	smtpmail-smtp-server "smtp.office365.com"
	smtpmail-stream-type 'starttls
	smtpmail-smtp-service 587)
  )
