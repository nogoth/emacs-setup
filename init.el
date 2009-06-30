(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/emacs-w3m")
(add-to-list 'load-path "~/.emacs.d/elim/elisp")
(add-to-list 'load-path "~/.emacs.d/bbdb/lisp")
(add-to-list 'load-path "~/.emacs.d/egg")
(load-file "~/.emacs.d/egg/egg.el")

(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;;(require 'w3m)

 (setq browse-url-browser-function 'w3m-browse-url)
 (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
 (setq w3m-use-cookies t)
 ;; so we can browse w/o it asking to verify it's safe
 (setq mm-w3m-safe-url-regexp 'nil)

(require 'twittering-mode)
 (setq twittering-username "nogoth")

(require 'rcirc)
(require 'rcirc-extension)
;; (require 'twit)

(ido-mode t)

(global-set-key (kbd "C-x C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c b") 'browse-url-at-point)
(global-set-key (kbd "C-c <left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-c <right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-c <down>") 'shrink-window)
(global-set-key (kbd "C-c <up>") 'enlarge-window)

(defun-rcirc-command reconnect (arg)
     "Reconnect the server process."
     (interactive "i")
     (unless process
       (error "There's no process for this target"))
     (let* ((server (car (process-contact process)))
            (port (process-contact process :service))
            (nick (rcirc-nick process))
            channels query-buffers)
       (dolist (buf (buffer-list))
         (with-current-buffer buf
           (when (eq process (rcirc-buffer-process))
             (remove-hook 'change-major-mode-hook
                          'rcirc-change-major-mode-hook)
             (if (rcirc-channel-p rcirc-target)
                 (setq channels (cons rcirc-target channels))
               (setq query-buffers (cons buf query-buffers))))))
       (delete-process process)
       (rcirc-connect server port nick
                      nick
                      nick
                      channels)))


(windmove-default-keybindings)

(menu-bar-mode -1 )
(tool-bar-mode -1 )
;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

;; To install, add the following to your .emacs file:
 (autoload 'kill-ring-search "kill-ring-search"
;;  "Search the kill ring in the minibuffer."
  (interactive))
 (global-set-key "\M-\C-y" 'kill-ring-search)
;;
;; Just call kill-ring-search and enter your search.
;; M-y and C-y work as usual.  You can also use C-r like in a shell.
;; C-v, M-v, C-n and C-p will scroll the view.

;; emacs-fu
(blink-cursor-mode nil)             ;; stop cursor from blinking
(setq search-highlight t            ;; highlight when searching... 
  query-replace-highlight t)

(setq save-place-file "~/.emacs.d/saveplace") ;; keep my ~/ clean
(setq-default save-place t)                   ;; activate it for all buffers
(require 'saveplace)                          ;; get the package

(setq savehist-additional-variables    ;; also save...
  '(search-ring regexp-search-ring kill-ring)    ;; ... my search entries
  savehist-file "~/.emacs.d/savehist") ;; keep my home clean
(savehist-mode t)                      ;; do customization before activate

(push '("." . "~/.emacs.d/savefiles") backup-directory-alist) 

;; mark current line:
(global-hl-line-mode 1)
;; color for current line:
(set-face-background 'hl-line "#e0f8ff")

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")



(require 'xcite)
(autoload 'xcite "xcite" "Exciting cite" t)
(autoload 'xcite-yank-cur-msg "xcite" "Exciting cite" t)
(autoload 'xcite-indent-citation "xcite")
;;(setq message-citation-line-function nil message-citation-line-function 'xcite-indent-citation)
(setq message-citation-line-function nil
message-indent-citation-function
'xcite-indent-citation)

(defun default-custom-header ()
(format "On %s %s%s wrote:\n"
date (or handle "") (if id (concat " (" id ")" ) "")))
(setq xcite:insert-header-function 'default-custom-header)


;; ruby items
(when (require 'ruby-mode nil t)
  (autoload 'ruby-mode "ruby-mode" "Major mode for editing Ruby code" t)
  (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
  (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
  (add-hook 'ruby-mode-hook (lambda () (local-set-key "\r" 'ruby-reindent-then-newline-and-indent))))

(when (require 'ruby-electric nil t)
  (require 'ruby-electric)
  (add-hook 'ruby-mode-hook (lambda () (ruby-electric-mode t))))



(add-hook 'rcirc-print-hooks 'my-rcirc-print-hook)
(setq alert "uri=file:///usr/share/sounds/purple/alert.wav")

(defun my-rcirc-print-hook (process sender response target text)
  (when (and (string-match (rcirc-nick process) text)
	     (not (string= (rcirc-nick process) sender))
	     (not (string= (rcirc-server-name process) sender)))
    (call-process "/usr/bin/gst-launch-0.10" nil nil nil "playbin" alert)
    )
  )
(add-hook 'rcirc-print-hooks 'chat-rcirc-print-hook)
(setq chat "uri=file:///usr/share/sounds/question.wav")
(defun chat-rcirc-print-hook (process sender response target text)
  (when (and (not (string-match (rcirc-nick process) text))
	     (not (string-match "QUIT" text))
	     (not (string-match "JOIN" text))
	     (not (string= (rcirc-nick process) sender))
	     (not (string= (rcirc-server-name process) sender)))
    (call-process "/usr/bin/gst-launch-0.10" nil nil nil "playbin" chat)
    )
  )