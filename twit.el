;;; Twit.el --- interface with twitter.com
(defvar twit-version-number "0.0.26")
;; Copyright (c) 2007 Theron Tlax
;;           (c) 2008-2009 Jonathan Arkell
;; Time-stamp: <2007-03-19 18:33:17 thorne>
;; Author: thorne <thorne@timbral.net>, jonnay <jonnay@jonnay.net>
;; Created: 2007.3.16
;; Keywords: comm
;; Favorite Poet: E. E. Cummings

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation version 2.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; For a copy of the GNU General Public License, search the Internet,
;; or write to the Free Software Foundation, Inc., 59 Temple Place,
;; Suite 330, Boston, MA 02111-1307 USA

;;; Commentary:

;; This is the beginnings of a library for interfacing with
;; twitter.com from Emacs.  It is also (more importantly) some
;; interactive functions that use that library.  It's a hack, of
;; course; RMS i am not.  Maybe one of you real programmers would
;; like to clean it up?
;;    -- Theron Tlax

;; Okay! ;)
;;    -- JonathanArkell

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `twit-direct'
;;    Send a user a direct tweet.
;;  `twit-post'
;;    Send a post to twitter.com.
;;  `twit-post-region'
;;    Send text in the region as a post to twitter.com.
;;  `twit-post-buffer'
;;    Post the entire contents of the current buffer to twitter.com.
;;  `twit-list-followers'
;;    Display a list of all your twitter.com followers' names.
;;  `twit-follow-recent-tweets'
;;    Sets up a timer, and shows you the most recent tweets approx every 90 seconds.
;;  `twit-stop-following-tweets'
;;    When you want to stop following tweets, you can use this function to turn off the timer.
;;  `twit-show-recent-tweets'
;;    Display a list of the most recent tweets from people you're following.
;;  `twit-show-direct-tweets'
;;    Display a list of the most recent direct tweets.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `twit-user'
;;    Your twitter username.
;;    default = ""
;;  `twit-pass'
;;    Your twitter password.
;;    default = ""
;;  `twit-new-tweet-hook'
;;    Functions to execute when there is a new tweet.
;;    default = (quote nil)
;;  `twit-follow-idle-interval'
;;    How long in time to wait before checking for new tweets.
;;    default = 90
;;  `twit-show-user-images'
;;    Show user images beside each users tweet.
;;    default = nil
;;  `twit-user-image-dir'
;;    Directory where twitter user images are to be stored.
;;    default = (concat (car image-load-path) "twitter")
;;  `twit-debug'
;;    Whether or not to run twit.el in debug mode
;;    default = nil
;;  `twit-debug-mem'
;;    Turn on memory debugging.
;;    default = nil
;;  `twit-filter-tweets-regex'
;;    Filter all tweets with this regex.
;;    default = ""
;;  `twit-filter-at-tweets'
;;    Whether or not to filter any tweets that have @user of a user you do not know.
;;    default = nil

;; This uses Twitter's XML-based api, not the JSON one because i
;; would like to avoid making the user install third-party libraries
;; to use it.

;; But remember that your posts can't be longer than 140 characters
;; long.  All of these functions will also prompt you for your user
;; name (usually the email address you signed up to twitter with)
;; and password the first time in a given Emacs session.  Note that
;; twitter uses `Basic Authentication' for user authentication,
;; which translates to, basically none.  It's not secure for
;; anything more than casual attacks.


;; Hacking:
;; See `twit-post-function', which is the backend for posting, and
;; `twit-parse-xml' which grabs an xml file from HTTP and turns it
;; into a list structure (using `xml-parse-fragment').  This is a work
;; in progress.

;; Testing: 
;; Best way to test it in default mode: 
;;   emacs --no-site-file --no-init-file
;; In scratch buffer:
;;   (load-file "~/my-elisp/twit.el")


;; Installing:

;; There's not much to it.  It you want it always there and ready, you
;; can add something to your .emacs file like:

;;  (load-file "/path/to/twit.el")

;; or get fancier, to the extent you want and know how (autoloading,
;; keybinding, etc).

;; You might want to run M-x customize-group twit and set your twitter
; username/password. 

;; Notes:

;; `twit-user' gets my vote for variable name of the year.  Ditto
;; `twit-mode' for mode names.

;; Integration:

;; This package integrates nicely with todochiku.el, which you
;; can download here:
;;  http://www.emacswiki.org/cgi-bin/wiki/todochiku.el
;; Just customize twit-new-tweet-hook and set it to twit-todochiku 

;;; History:

;; 2007-3-16 theron tlax <thorne@timbral.net>
;; * 0.0.1 -- Initial release.  Posting only.
;; * 0.0.2 -- Near-total rewrite; better documentation; use standard
;;            Emacs xml and url packages; minor mode; a little
;;            abstraction; some stubs for the reading functions.
;; * 0.0.3 -- Doc and other minor changes.
;; * 0.0.4 -- (released as 0.0.3 -- Added twit-show-recent-tweets 
;;             by Jonathan Arkell)
;; * 0.0.5 -- Add source parameter to posts
;; * 0.0.6 -- Re-working twit-show-recent-tweets to show more info
;;            (and to get it working for me) -- by H Durer
;; * 0.0.7 -- Keymaps in the buffers for twit-show-recent-tweets and
;;            twit-list-followers; encode the post argument so that it 
;;            is a valid post request
;; * 0.0.8 -- faces/overlays to make the *Twit-recent* buffer look
;;            prettier and more readable (at least for me) -- by H Durer
;; * 0.0.9 -- follow-recent-tweets function created so automagickally 
;;            follow tweets every 5 mins.  Also removed twit-mode 
;;            on twit-show-recent-tweets.  (it was setting twit-mode
;;            globally, and interfering with planner)  (JA)
;; * 0.0.10 - There is a hook that is run when a new tweet is seen.
;;            This can be used to interface with the todochiku package
;;            to send a notification that there is a new tweet.
;;            (or anything else for that matter)
;;            Twit-user and Twit-pass are now customizeable variables
;;            that work.  Finally, no more constant re-entry of your
;;            username and password. (JA)
;; * 0.0.11 - Updated to set a customization for the
;;            follow-recent-tweets idle timer.  This is so that you
;;            wont get throttled when twitter changes their throttle
;;            time (JA)
;; * 0.0.12 - Changed syncronous url call to an ascynronous one that
;;            doesn't suck, and polls properly.
;;            You can finally stop following recent tweets. (Rev 22)
;; * 0.0.13 - Fixed twit-debug to be a customizeable variable.  Duh.
;;            Image handling is on the way. (done, just buggy)
;;            Better face definitions, now customizeable.
;;            Zebra-tabling of the recent tweets is half there.
;;            Retrieval of the rate-limiting is working. (Rev 23)
;; * 0.0.14 - Finished zebra-table for recent tweets. (uses overlays)
;;            Fix for a really crazy bug in carbon emacs. (thanks gr3p3)
;;            Tweaked default fonts to not suck, added more faces.
;;            If showing recent tweets fails, the buffer is no longer
;;            set to blank. (JA)
;; * 0.0.15 - Fixed the automatic rate-limiting on 400 messages from
;;            twitter to work.
;;            Updated rate limiting to use the new format
;;            More messages are handed off to todochiku
;;            Most rate limiting (except the initial follow) is done
;;            Asyncronously.
;;            Verified that twit-list-followers works.
;;            URLs now are fontified and hot.
;;            Maybe (maybe?) fixed the bug where following tweets
;;            kills the mark, and dumps the point at the bottom. (JA)
;; * 0.0.16 - Fixed most compilation warnings. (PeterJones)
;; * 0.0.17 - Fixed a bug where a users username/password pair is
;;            not properly updated through customization.  It's not
;;            100%, but it should be much better now. (JA)
;;            twit-show-recent-tweets doesn't change focus. (thanks Ben Atkin)
;; * 0.0.18 - Fixed a bug where xml entities were not converted while
;;            tweet messages (JonathanCreekmore)
;; * 0.0.19 - Fixed a bug where the previous tweets in the *Twit-recent* buffer
;;            were saved on the undo list every time new tweets came in. (JonathanCreekmore)
;; * 0.0.20 - Added support for "Reply to" in the twit-post function. (JonathanCreekmore)
;; * 0.0.21 - The rate-limit timer is cancelled as well as the main
;;            timer.
;;            Regexp filtering of tweets.  (JonathanArkell)
;; * 0.0.22 - Fixed infinite loop problem if you cannot retrieve the rate-limit.
;;            Could be the cause of the memory leak. (JA)
;;          - Added elisp function to return following users as list.
;;          - fixed regex problem
;; * 0.0.23 - made sure that url-retrive is wrapped around a let to shadow
;;            url-request-method with GET.
;;          - Optional filtering of @ message to people you don't know. (JA)
;;          - Added Images!  Whute!  Make sure you check customization options.
;; * 0.0.24 - Updated image handling to only download and install when images are
;;            turned on.
;;          - Enabled confirmation of posting tweets
;;          - Enabled direct messages.
;; * 0.0.25 - Updated image handling again so that the local copy of image are used
;;            when available.
;;          - Now you can view direct tweets!  Whute!
;; * 0.0.26 - You can hit "d" in a list of tweets to direct-message that user.
;;          - Documentation improved, especially interactive and customs
;;          - Documentation merged.
;;          - source code is made more tagging friendly (for free-tagging.el)
;;          - zebra table face definition sucks less.  Hopefully this will
;;            look better for those of you on dark-background screens.
;;          - older tweets and direct messages are viewable with prefix
;;            arguments.  
;;
;; Bugs:
;; * Follow-recent-tweets might have a serious memory leak.  This
;;   probably has to do with the temporary buffers that are being created
;; * zebra tables are hosed with filtering
;; * The parse-xml interface needs to be a little better.  On url-retrive
;;   the HTTP status message (at the very least) should be parsed out
;;   and delt with.  This will stop the problem where the first
;;   listing of followers, or showing of tweets comes up blank.
;;
;; Please report bugs to the twit emacs wiki page at:
;;   http://www.emacswiki.org/cgi-bin/wiki/TwIt
;;
;;; Roadmap (todo):
;; v1.0 release
;; - optionally authenticate via open-auth instead of http auth.
;; - Fix memory leak.
;; - fix http headers bug.
;;
;; Post 1.0
;; - make the user images float right.  (thanks busytoby)
;; - Make @names a different color, and make them hot, so that hitting return on them will take you
;;   to their page
;; - Integrate the other twit.el that is out there.  Looks like it might have some serious sexxxy to it.

;;; Code:

(require 'xml)
(require 'url)
(require 'url-http)

(eval-when-compile
  (require 'cl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar twit-status-mode-map (make-sparse-keymap))
(defvar twit-followers-mode-map (make-sparse-keymap))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cusomtization functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun twit-set-user-pass (sym val)
  "Set the username/password pair after a customization.

Note that this function uses a really cheap hack.
Basically the problem is that we need to run this whenever the twit-user
and twit-pass variables are customized and loaded.  The problem is, this
funciton is executed by cutomzie on emacs initialization, during the
setting of twit-user, but before the binding to twit-pass, throwing an 
error.

We get around this by using condition-case and handling the void-varible
error."
  (set-default sym val)
  (condition-case nil
    (let ((twit-pass-var 'twit-pass))
	  (when (and (not (string= (symbol-value twit-pass-var) ""))   
				 (not (string= twit-user "")))
			(let ((old-storage (assoc "twitter.com:80" (symbol-value url-basic-auth-storage))))
			  (when old-storage 
					(set url-basic-auth-storage (delete old-storage (symbol-value url-basic-auth-storage)))))
			(set url-basic-auth-storage
				 (cons (list "twitter.com:80"
							 (cons "Twitter API"
								   (base64-encode-string (format "%s:%s" twit-user (symbol-value twit-pass-var)))))
					   (symbol-value url-basic-auth-storage)))))
	(void-variable nil)))

;; Use this when testing the basic storage engine.
;; Set up for easy space for your C-x C-e execution pleasure.
(when nil
	  (set url-basic-auth-storage nil)
	  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup twit nil
  "twit.el is an emacs package for interfacing with Twitter (http://www.twitter.com)
a microblogging service.  The twit.el package provides you with the ability to post
status updates and driect messages, as well as recieve them.  Additionally you can
opt to \"follow\", and the timeline will be updated automagickally.

twit.el also makes use of the Todochiku package, which you can install from here:
http://www.emacswiki.org/emacs/todochiku.el"
  :version "0.1"
  :group 'twit)

(defcustom twit-user
  ""
  "Your twitter username.
If this is set, attempt to automagickally log into twitter."
  :group 'twit
  :type 'string
  :set 'twit-set-user-pass)

(defcustom twit-pass
  ""
  "Your twitter password.
If this is set, attempt to automagickally log into twitter."
  :group 'twit
  :type 'string
  :set 'twit-set-user-pass)

(defcustom twit-new-tweet-hook
  '()
  "Functions to execute when there is a new tweet.
If you have Todochiku, add \"twit-todochiku\" here, and you will be notified when a new tweet appears. "
  :type 'hook
  :group 'twit)

(defcustom twit-follow-idle-interval
  90
  "How long in time to wait before checking for new tweets.
Right now it will check every 90 seconds, Which will generate a maximum of 40 requests, leaving you another 30 per hour to play with.

The variable name is a bit of a misnomer, because it is not actually based on idle time (anymore)."
  :type 'integer
  :group 'twit)

(defvar twit-shadow-follow-idle-interval
  twit-follow-idle-interval
  "Shadow definition of `twit-follow-idle-interval' that we can modify on the fly.")

(defcustom twit-show-user-images nil
   "Show user images beside each users tweet." 
   :type 'boolean
   :group 'twit)

(defcustom twit-user-image-dir
  (concat (car image-load-path) "twitter")
  "Directory where twitter user images are to be stored.

This directory need not be created."
  :type 'string
  :group 'twit)

(defcustom twit-debug
  nil
  "Whether or not to run twit.el in debug mode"
  :group 'twit
  :type 'boolean)

(defcustom twit-debug-mem
  nil
  "Turn on memory debugging."
  :group 'twit
  :type 'boolean)

(defcustom twit-filter-tweets-regex ""
  "Filter all tweets with this regex.

This is useful if you do not want to see a particular style of tweet.
For isntance, if hash-tagging pisses you off, you could set this to \"#\" and
no hash-tagging messages would get to you.
"
  :type 'regexp
  :group 'twit)

(defcustom twit-filter-at-tweets nil 
   "Whether or not to filter any tweets that have @user of a user you do not know.

If enabled, every tweet will be scanned for any @msgs.  If they contain one, and
the username is not a user that you are following (or you) then it will be ignored."   
   :type 'boolean
   :group 'twit)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface twit-message-face
  '((default
	  :family "helv"
	  :height 1.1))
  "The font face to use for a twitter message."
  :group 'twit)

(defface twit-author-face
  '((t
	  (:height 0.8
	   :weight bold
	   :family "mono")))
  "The font face to use for the authors name"
  :group 'twit)

(defface twit-info-face
  '((t (:height 0.8 :slant italic)))
  "Face for displaying where, how and when someone tweeted."
  :group 'twit)

(defface twit-title-face
  '((((class color) (background light))
	 (:background "PowderBlue" :underline "DeepSkyBlue"))
	(((class color) (background dark))
	 (:background "PowderBlue" :underline "DeepSkyBlue" :foreground "Black"))
	(t (:underline "white")))
  "Title Area of the recent tweets buffer."
  :group 'twit)

(defface twit-zebra-1-face
  '((((class color) (background light))
	 (:background "gray89"))
	(((class color) (background dark))
	 (:background "grey15"))
	(t (:inverse)))
  "Color one of zebra-striping of recent tweets and followers list."
  :group 'twit)

(defface twit-zebra-2-face
  '((((class color) (background light))
	 (:background "AliceBlue" :inverse))
	(((class color) (background dark))
	 (:background "MidnightBlue")))
  "Color two of zebra-striping of recent tweets and followers list."
  :group 'twit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; More variables and constants
;; 'r' key for reloading/refreshing the buffer
(defvar twit-key-list
  '(("s" . twit-show-recent-tweets)
	("f" . twit-list-followers)
	("p" . twit-post)
    ("d" . twit-direct)
	("h" . twit-mode-help)
	("?" . twit-mode-help)))

(define-key twit-status-mode-map "r" 'twit-show-recent-tweets)
(define-key twit-followers-mode-map "r" 'twit-list-followers)


(dolist (info twit-key-list)
  (define-key twit-status-mode-map (car info) (cdr info))
  (define-key twit-followers-mode-map (car info) (cdr info)))

(defun twit-mode-help ()
	(interactive)
	(message "Help: %s" (append twit-key-list '(("r" . "Reload Current Page")))))

(defvar twit-timer
  nil
  "Timer object that handles polling the followers")

(defvar twit-rate-limit-timer
  nil
  "Timer object to poll the rate-limiting.")

(defvar twit-first-time-through nil)

;; Most of this will be used in the yet-to-be-written twitter
;; reading functions.
(defvar twit-base-url "http://twitter.com")

(defconst twit-update-url 
  (concat twit-base-url "/statuses/update.xml"))
(defconst twit-puplic-timeline-file
  (concat twit-base-url "/statuses/public_timeline.xml"))
(defconst twit-friend-timeline-file
  (concat twit-base-url "/statuses/friends_timeline.xml?page=%s"))
(defconst twit-followers-file
  (concat twit-base-url "/statuses/followers.xml"))
(defconst twit-friend-list-url
  (concat twit-base-url "/statuses/friends.xml"))
(defconst twit-mentions-url
  (concat twit-base-url "/statuses/mentions.xml"))

(defconst twit-rate-limit-file
  (concat twit-base-url "/account/rate_limit_status.xml"))

(defconst twit-direct-msg-send-url
  (concat twit-base-url "/direct_messages/new.xml"))
(defconst twit-direct-msg-get-url
  (concat twit-base-url "/direct_messages.xml"))


(defconst twit-post-success-msg 
  "Post sent!")
(defconst twit-direct-success-msg
  "Direct Message sent!")

(defconst twit-post-failed-msg "Your posting has failed.")

(defconst twit-too-long-msg 
  "Post not sent because length exceeds 140 characters")

(defconst twit-standard-rate-limit 100)

(defconst twit-rate-limit-offset 5
  "Number of seconds to add to a throttled rate limit for insurance.")

(defconst twit-rate-limit-interval (* 2 60 60)
  "Every 2 Hours check for rate limiting.")

(defconst twit-filter-at-tweets-retweet-regex "\\bRT[ :]*@"
  "Retweets are tweets that do contain at messages that might be actually interesting.")

(defconst twit-request-headers `(("X-Twitter-Client" . "twit.el")
								 ("X-Twitter-Client-Version" . ,twit-version-number)
								 ("X-Twitter-Client-URL" . "http://www.emacswiki.org/cgi-bin/emacs/twit.el")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal Library Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* helper
(defun twit-alert (msg &optional title)
  "Send some kind of alert to the user.  
If todochiku is available, use that.  Instead, just message the user."
  (when (null title) (setq title "twit.el"))
  (when (featurep 'todochiku)
	  (todochiku-message title msg (todochiku-icon 'social)))
  (message "%s: %s" title msg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General purpose library to wrap twitter.com's api
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;* xmlparse refactorme
(defun twit-parse-xml (url method)
  "Retrieve file at URL and parse with `xml-parse-fragment'.
Emacs' url package will prompt for authentication info if required.

Note that this parses the entire HTTP request as an xml fragment
and not the response.  THIS BEHAVIOR IS SUBJECT TO CHANGE!"
  (let ((result nil)
		(url-request-method method))
    (save-window-excursion
      (set-buffer (url-retrieve-synchronously url))
      (goto-char (point-min))
      (setq result (xml-parse-fragment))
      (kill-buffer (current-buffer)))
    result))

;;* xmlparse var
(defvar twit-async-buffer 'nil
  "Buffer that stores the temporary XML result for tiwt.el")

;;* xmlparse async
(defun twit-parse-xml-async (url callback)
  "Retrieve the resource at URL, and when retrieved call callback
This is the asyncronous version of twit-parse-xml.  Once that function is
refactored, and its named changed, so should this one."
  (let ((url-request-method "GET"))
	(setq twit-async-buffer (url-retrieve url 'twit-parse-xml-async-retrieve (list url callback)))))

;;* rate-limit var
(defvar twit-rate-limit-halt-flag 'nil "Rate Limit flag
This is a flag to  make sure we don't try and get the rate limit, if
there is an error with retrieving the rate limiter. it is meant to
be used dynamically (i.e. inside of a (let (())) statement. )")

;;* xmlparse async
(defun twit-parse-xml-async-retrieve (status url callback)
  (if (null status)   ; no news is good news.  
	  (let ((result nil))
		(if (bufferp twit-async-buffer)
			(save-excursion
			 (set-buffer twit-async-buffer)
			 (goto-char (point-min))
			 (setq result (xml-parse-fragment))
			 (kill-buffer (current-buffer))))
		(funcall callback status url result))
	  (progn
	   (twit-alert (format "Cannot retrieve twit URL.  Status is: %S" status))
	   (when (equal status '(:error (error http 400)))
			 (if (null twit-rate-limit-halt-flag)
				 (twit-get-and-set-async-rate-limit)
				 (error "URL retrieval error when trying to get rate limiter.  twit-rate-limit-halt-flag is true!"))))))

;;* post handler
(defun twit-handle-post (err success-msg error-msg)
  "General method to hande a twit posting.
This will give us a Guarantee that our posting atually did work."
  (cond
    ((null err) (twit-alert success-msg))
	(t (twit-alert error-msg)
	   (message "Post HTTP error was: %s" err)
	   (if twit-debug (message ("%s" buffer-string)))))
  (kill-buffer (current-buffer)))

;;* post status
(defun twit-post-function (url post)
  (let ((url-request-method "POST")
	(url-request-data (concat "source=twit.el&status=" (url-hexify-string post)))
        ;; these headers don't actually do anything (yet?) -- the 
        ;; source parameter above is what counts
        (url-request-extra-headers twit-request-headers))
    (if twit-debug (twit-alert url-request-data))
    (url-retrieve url 'twit-handle-post (list twit-post-success-msg twit-post-failed-msg))))

;;* post direct 
(defun twit-direct-message (user msg)
  (let ((url-request-method "POST")
		(url-request-data (concat "source=twit.el"
								  "&user=" (url-hexify-string user)
								  "&text=" (url-hexify-string msg)))
		(url-request-headers twit-request-headers))
	(if twit-debug (twit-alert url-request-data))
	(url-retrieve twit-direct-msg-send-url 'twit-handle-post (list twit-direct-success-msg twit-post-failed-msg))))

;;* rate 
(defun twit-parse-rate-limit (xml)
  "Parse the rate limit file, and return the hourly limit.  XML should be the twitter ratelimit sxml.
XML should not have any HTTP header information in its car."
  (let ((limit (assoc 'hourly-limit xml)))
	(if twit-debug (message "Parsed limit %s from xml %s" limit xml))
	(if limit
		(string-to-number (caddr limit)))))

;;* rate
(defun twit-get-rate-limit ()
  (interactive)
  "Returns the rate limit as a number from the xml."
  (let ((limit-xml (twit-parse-xml twit-rate-limit-file "GET")))
	(twit-parse-rate-limit (cadr limit-xml))))

;; rate async
(defun twit-get-and-set-async-rate-limit ()
  (interactive)
  "Check rate limiting asyncronously, and automagickally set it."
  (twit-parse-xml-async twit-rate-limit-file 'twit-get-and-set-async-rate-limit-callback))

;; rate async
(defun twit-get-and-set-async-rate-limit-callback (status url result)
  "callback for twit-get-and-set-async-rate-limit"
  (if (null status)
	  (if twit-debug (message "Rate Limit XML is %S" result))
	  (twit-verify-and-set-rate-limit (twit-parse-rate-limit (cadr result)))
	  (twit-alert (format "Cannot retrieve rate limit URL %S! Status: %S" url status))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers for the interactive functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun twit-query-for-post (prompt-heading initial-input)
  "Query for a Twitter.com post text in the minibuffer."
  (read-string (concat prompt-heading " (140 char max): ") initial-input))

(defvar twit-last-tweet '()
  "The last tweet that was posted.
This is a bit of an ugly hack to store the last tweet that was shown through twit-write-recent-tweets.
It is in the format of (timestamp user-id message) ")

(setq twit-last-tweet '())
; damn.. most recent tweet is HACKY.
(defun twit-write-recent-tweets-old (xml-data)  ;(twit-parse-xml twit-friend-timeline-file)
  (buffer-disable-undo)
  (delete-region (point-min) (point-max))
  (twit-insert-with-overlay-attributes (format-time-string "Last updated: %c\n")
									   '((face . "twit-title-face")))
  (let ((last-tweet '())
		(times-through 1)
		(overlays 'nil))
	(labels ((xml-first-child (node attr)
							  (car (xml-get-children node attr)))
			 (xml-first-childs-value (node addr)
									 (car (xml-node-children (xml-first-child node addr)))))
			(dolist (status-node (xml-get-children (cadr xml-data) 'status))
					(let* ((user-info (xml-first-child status-node 'user))
						   (user-id (or (xml-first-childs-value user-info 'screen_name) "??"))
						   (user-name (xml-first-childs-value user-info 'name))
						   (location (xml-first-childs-value user-info 'location))
						   (src-info (xml-first-childs-value status-node 'source))
						   (user-img (if twit-show-user-images
										 (twit-get-user-image (xml-first-childs-value user-info 'profile_image_url))
										 nil))
						   (timestamp (xml-first-childs-value status-node 'created_at))
						   (message (xml-substitute-special (xml-first-childs-value status-node 'text))))
					  (when (and  (or (string-equal "" twit-filter-tweets-regex)
									  (null twit-filter-tweets-regex)
									  (not (string-match twit-filter-tweets-regex message)))
								  (or (not twit-filter-at-tweets)
									  (not (string-match "@" message))
									  (string-match twit-filter-at-tweets-retweet-regex message)
									  (and twit-filter-at-tweets
										   (twit-at-message-was-from-friend message))))
							(when (= times-through 1)
								  (setq last-tweet (list timestamp user-id message))
								  (setq twit-first-time-through nil))
   							(when (and src-info (string-match (concat "<a h" "ref=\"\\(.*\\)\">\\(.*\\)<" "/a>") ; the string-match is a bit weird, as emacswiki.org won't accept pages with the href in it per se					 
															  src-info))
								  ;; remove the HTML link info; leave just the name (for now)
								  (setq src-info (match-string 2 src-info)))

							(setq overlay-start (point))

							(when (and twit-show-user-images user-img) ; (and user-img (not (bufferp user-img)))  ;Images aren't ready for primetime.  
								  (insert " ")
								  (insert-image user-img)
								  (insert " "))
					  
							(twit-insert-with-overlay-attributes (format "%25s" 
																		 (concat user-id
																				 (if user-name
																					 (concat " (" user-name ")")
																					 "")))
																 '((face . "twit-author-face")))
							(insert ": ")
							(twit-insert-with-overlay-attributes message
																 '((face . "twit-message-face")))
							(insert "\n")
					  
							(when (or timestamp location src-info)
								  (twit-insert-with-overlay-attributes
								   (concat "                          "
										   (when timestamp (concat " posted " timestamp))
										   (when location (concat " from " location))
										   (when src-info (concat " (via " src-info ")"))
										   "\n")
								   '((face . "twit-info-face"))))
							(setq overlay-end (point))
							(let ((o (make-overlay overlay-start overlay-end)))
							  (overlay-put o 'face (if (= 0 (% times-through 2))
													   "twit-zebra-1-face"
													   "twit-zebra-2-face"))
							  (add-to-list 'overlays o))
							(setq times-through (+ 1 times-through))))))
	(if (not (equal last-tweet twit-last-tweet))
		(progn (setq twit-last-tweet last-tweet)
			   (setq twit-first-time-through nil)
			   (run-hooks 'twit-new-tweet-hook))))
  ;; go back to top so we see the latest messages
  (goto-address)
  (goto-char (point-min))
  (if twit-debug-mem (message (garbage-collect))))

;;* xmlparse
(when (not (functionp 'xml-first-child))
	  (defun xml-first-child (node attr)
		"Return the first child of some sxml"
		(car (xml-get-children node attr))))

;;* xmlparse
(when (not (functionp 'xml-first-childs-value))
	  (defun xml-first-childs-value (node addr)
		"Return the value of the first child of some sxml. "
		(car (xml-node-children (xml-first-child node addr)))))

;;; xml parsing is a little hacky and needs work.
;;* tweets writing memoryleak
(defun twit-write-recent-tweets (xml-data) 
  (buffer-disable-undo)
  (delete-region (point-min) (point-max))
  (twit-insert-with-overlay-attributes (format-time-string "Last updated: %c\n")
									   '((face . "twit-title-face")))
  (let* ((first-tweet (xml-first-child (cadr xml-data) 'status))
		 (most-recent-tweet (list (xml-first-childs-value first-tweet 'created_at)
								  (or (xml-first-childs-value (xml-first-child first-tweet 'user) 'screen_name) "??")
								  (xml-substitute-special (xml-first-childs-value first-tweet 'text))))
		 (times-through 1))
	(dolist (status-node (xml-get-children (cadr xml-data) 'status))
			(twit-write-tweet status-node nil times-through)
			(setq times-through (+ 1 times-through)))
	
	(when (not (equal most-recent-tweet twit-last-tweet))
		  (setq twit-last-tweet most-recent-tweet)
		  (run-hooks 'twit-new-tweet-hook)))
  
  ;; go back to top so we see the latest messages
  (goto-address)
  (goto-char (point-min))

  ;; this needs more TLC
  (if twit-debug-mem (message (garbage-collect))))

;;* tweet direct write
(defun twit-write-tweet (tweet &optional filter-tweets times-through)
  "Inserts a tweet into the current buffer.
`tweet' should be an xml parsed node, which could be a message node or a status node.
`filter-tweets' is an optional boolean to disregard filtering.
`times-through' is an integer representing the number of times a tweet has been
  displayed, for zebra-tabling."
  (let* ((user-info (or (xml-first-child tweet 'user) (xml-first-child tweet 'sender)))
		 (user-id (or (xml-first-childs-value user-info 'screen_name) "??"))
		 (user-name (xml-first-childs-value user-info 'name))
		 (location (xml-first-childs-value user-info 'location))
		 (user-img (if twit-show-user-images
					   (twit-get-user-image (xml-first-childs-value user-info 'profile_image_url))
					   nil))
		 
		 (timestamp (xml-first-childs-value tweet 'created_at))
		 (message (xml-substitute-special (xml-first-childs-value tweet 'text)))
		 (src-info (xml-first-childs-value tweet 'source))
		 
		 (overlay-start 0)
		 (overlay-end 0))
	
	(when (and  (or (string-equal "" twit-filter-tweets-regex)
					(null twit-filter-tweets-regex)
					(null filter-tweets)
					(not (string-match twit-filter-tweets-regex message)))
				(or (not twit-filter-at-tweets)
					(not (string-match "@" message))
					(string-match twit-filter-at-tweets-retweet-regex message)
					(and twit-filter-at-tweets
						 (twit-at-message-was-from-friend message))))
		  (when (and src-info (string-match (concat "<a h" "ref=\"\\(.*\\)\">\\(.*\\)<" "/a>") ; the string-match is a bit weird, as emacswiki.org won't accept pages with the href in it per se					 
											src-info))
				;; remove the HTML link info; leave just the name (for now)
				(setq src-info (match-string 2 src-info)))

		  (setq overlay-start (point))

		  (when (and twit-show-user-images user-img)
				(insert " ")
				(insert-image user-img)
				(insert " "))
					  
		  (twit-insert-with-overlay-attributes (format "%25s" 
													   (concat user-id
															   (if user-name
																   (concat " (" user-name ")")
																   "")))
											   '((face . "twit-author-face")))
		  (insert ": ")
		  (twit-insert-with-overlay-attributes message
											   '((face . "twit-message-face")))
		  (insert "\n")
					  
		  (when (or timestamp location src-info)
				(twit-insert-with-overlay-attributes
				 (concat "                          "
						 (when timestamp (concat " posted " timestamp))
						 (when location (concat " from " location))
						 (when src-info (concat " (via " src-info ")"))
						 "\n")
				 '((face . "twit-info-face"))))
		  (setq overlay-end (point))
		  (let ((o (make-overlay overlay-start overlay-end)))
			(overlay-put o 'face (if (= 0 (% times-through 2))
									 "twit-zebra-1-face"
									 "twit-zebra-2-face"))))))

;;* image
(defvar twit-user-image-list 'nil
  "List containing all user images")

;;* image
(setq twit-user-image-list 'nil)

;;* image todo
; This should check to see if the url is stored locally, and if so, don't retrieve
(defun twit-get-user-image (url)
  "Retrieve the user image from the list, or from the URL"
  (let ((img (assoc url twit-user-image-list)))
	(if (and img (not (bufferp (cdr img))))
		(cdr (assoc url twit-user-image-list))
		(if (file-exists-p (concat twit-user-image-dir "/" (file-name-nondirectory url)))
			(let ((img (create-image (concat twit-user-image-dir "/" (file-name-nondirectory url)))))
			  (add-to-list 'twit-user-image-list (cons url img))
			  img)
			(let ((url-buffer (url-retrieve url 'twit-write-user-image (list url))))
			  (if url-buffer
				  (progn
				   (add-to-list 'twit-user-image-list (cons url url-buffer))
				   (if twit-debug (message "Added image. List is %s" twit-user-image-list)))
				  (twit-alert (format "Warning, couldn't load %s " url)))
			  nil)))))

;;* image todo
(defun twit-write-user-image (status url)
  "Called by twit-get-user-image, this performs the actual writing of the status url."
  (let ((image-file-name (concat twit-user-image-dir "/" (file-name-nondirectory url))))
	(when (not (file-directory-p twit-user-image-dir))
		  (make-directory twit-user-image-dir))
	(setq buffer-file-coding-system 'no-conversion)
	(setq buffer-file-name image-file-name)
	(goto-char (point-min))
	(delete-region (point-min) (search-forward "\C-j\C-j"))
	(save-buffer)
	(delete (cons url (current-buffer)) twit-user-image-list)
	(kill-buffer (current-buffer))
	(add-to-list 'twit-user-image-list (cons url (create-image image-file-name)))))

;;;
;; Recent tweets timer funciton and callback
;;* recent timer 
(defun twit-follow-recent-tweets-timer-function ()
  "Timer function for recent tweets, called via a timer"
  (twit-parse-xml-async (format twit-friend-timeline-file 1) 'twit-follow-recent-tweets-async-callback))

;;* recent async 
(defun twit-follow-recent-tweets-async-callback (status url xml)
  (when (not status)
		(save-window-excursion
		 (set-buffer (get-buffer-create "*Twit-recent*"))
		 (toggle-read-only 0)
		 (twit-write-recent-tweets xml)
		 (toggle-read-only 1))))

;;* rate
(defvar twit-last-rate-limit
  twit-standard-rate-limit
  "What is the previous rate limit?")

;;* rate
(defun twit-verify-and-set-rate-limit (limit)
  "Check if limiting is in effect, and if so, set the timer."
  (let ((limit-reset nil)
		(twit-rate-limit-halt-flag 't)) 
	(if twit-debug (message  "Rate limit is %s, doing ratelimit magic." limit))
	(when (and limit
			   (not (= limit 0))
			   (not (= twit-last-rate-limit limit)))
		  (cond ((< limit twit-standard-rate-limit)
				 (progn
				  (setq twit-shadow-follow-idle-interval (+ (/ (* 60 60) limit)
															twit-rate-limit-offset))
				  (setq limit-reset 't)
				  (twit-alert (format "Twitter is under a rate limit.  Timer set to %s seconds." twit-shadow-follow-idle-interval))))
				((= limit twit-standard-rate-limit)
				 (progn
				  (setq twit-shadow-follow-idle-interval twit-follow-idle-interval)
				  (setq limit-reset 't)
				  (twit-alert (format "Rate limiting relaxed.  Timer set to normal timeout (%s seconds)" twit-shadow-follow-idle-interval))))
				(t ; exceptional case...
				 (progn
				  (setq twit-shadow-follow-idle-interval twit-follow-idle-interval)
				  (setq limit-reset 't)
				  (twit-alert (format "The twitter rate has exceeded its expected maximum.  This is weird."))))))
	(when (and limit-reset (timerp twit-timer))
		  (progn
		   (if twit-debug (message "Cancelling and restarting timer."))
		   (cancel-timer 'twit-timer)
		   (twit-follow-recent-tweets)))
	(setq twit-last-rate-limit limit))) 

;;* friends 
(defun twit-get-friends ()
  "Get a list of friends."
  (save-excursion
   (loop for screen_name in
		 (loop for user in
			   (xml-get-children (cadr (twit-parse-xml twit-friend-list-url "GET"))
								 'user)
			   collect (eighth user))
		 collect (third screen_name))))

;;* friends at-filter var
(defvar twit-at-friends-cache 'nil
  "A cached list of the people who you are following, with a @ in front of their name."
  )
(setq twit-at-friends-cache 'nil)  ; provided here so it cacn be eval-ed to reload friends cache.
;;* friends at-filter
(defun twit-at-message-was-from-friend (tweet)
  "Tell us if the text in the tweet contains an @ that we care about."
  (when (null twit-at-friends-cache)
		(setq twit-at-friends-cache
			  (mapcar (lambda (s) (concat "@" s))
					  (cons twit-user (twit-get-friends)))))
  (let ((at-regex "@\\([a-zA-Z0-9]+\\b\\)+")
		(found-match nil))
	(while  (and (not found-match)
				 (string-match at-regex tweet))
			(setq found-match (member (match-string 0 tweet) twit-at-friends-cache))
			(setq tweet (substring tweet (+ 1 (string-match at-regex tweet)))))
	found-match))

(when nil
	  (twit-at-message-was-from-friend "@jonnay is the bomb")
	  (twit-at-message-was-from-friend "@sunnay is the bomb"))

;;; funciton to integrade with growl.el or todochiku.el
(defun twit-todochiku ()
  (todochiku-message "twit.el" (format "From %s:\n%s" (cadr twit-last-tweet) (caddr twit-last-tweet)) (todochiku-icon 'social)))

(defun twit-grab-author-of-tweet ()
  (let* ((find-overlays-specifying (lambda (prop)
                                     (let ((overlays (overlays-at (point)))
                                           found)
                                       (while overlays
                                         (let ((overlay (car overlays)))
                                           (if (overlay-get overlay prop)
                                               (setq found (cons overlay found))))
                                         (setq overlays (cdr overlays)))
                                       found)))
         (find-overlays-matching (lambda (prop value)
                                   (let ((overlays (funcall find-overlays-specifying prop))
                                         match)
                                     (when overlays
                                       (dolist (overlay overlays)
                                         (let ((val (overlay-get overlay prop)))
                                           (if (equalp val value)
                                               (setq match val)))))
                                     match))))
  (save-excursion
    (while (and (not (funcall find-overlays-matching 'face "twit-author-face"))
                (not (eq (point) (point-min))))
      (goto-char (previous-overlay-change (point))))
    (back-to-indentation)
    (thing-at-point 'word))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main interactive functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;* interactive direct
;;;###autoload
(defun twit-direct (user msg)
  "Send a user a direct tweet.

If you are currently positioned over a tweet, then it will fill in the author of that
tweet as the default recipient.

This will attempt to do a completing read based on the people you
are following, if you have images turned on."
  (interactive
    (list (let ((friends (mapcar (lambda (s) (substring s 1)) twit-at-friends-cache))
				(cur-author (twit-grab-author-of-tweet)))
			(completing-read (if cur-author
								 (concat "To (" cur-author "): ")
								 "To: ")
							 friends
							 nil
							 nil
							 nil
							 nil
							 (twit-grab-author-of-tweet)))
		  (read-string "Message: ")))
  (if (> (length msg) 140)
	  (error twit-too-long-msg)
      (twit-direct-message user msg)))

;;* post interactive
;;;###autoload
(defun twit-post (prefix)
  "Send a post to twitter.com.
Prompt the first time for password and username \(unless
`twit-user' and/or `twit-pass' is set\) and for the text of the
post; thereafter just for post text.  Posts must be <= 140 chars
long.

A prefix argument will prompt you for your post in reply to a
specific author that hte cursor is nearest to.
"
  (interactive "P")
  (let* ((reply-to (when prefix 
                     (twit-grab-author-of-tweet)))
         (post (twit-query-for-post (if reply-to
                                        (concat "Reply to " reply-to)
                                      "Post") 
                                    (when reply-to
                                      (concat "@" reply-to " ")))))
    (if (> (length post) 140)
		(error twit-too-long-msg)
		(twit-post-function twit-update-url post))))

;;* post interactive
;;;###autoload
(defun twit-post-region (start end)
  "Send text in the region as a post to twitter.com.
Uses `twit-post-function' to do the dirty work and to obtain
needed user and password information.  Posts must be <= 140 chars
long."
  (interactive "r")
  (let ((post (buffer-substring start end)))
    (if (> (length post) 140)
	(error twit-too-long-msg)
    (twit-post-function twit-update-url post))))

;;* post interactive
;;;###autoload
(defun twit-post-buffer ()
  "Post the entire contents of the current buffer to twitter.com.
Uses `twit-post-function' to do the dirty work and to obtain
needed user and password information.  Posts must be <= 140 chars
long."
  (interactive)
  (let ((post (buffer-substring (point-min) (point-max))))
    (if (> (length post) 140)
	    (error twit-too-long-msg)
	    (twit-post-function twit-update-url post))))

;;* show followers refactorme interactive memoryleak
;; minor modes might be a cause of the memoryleak, see about removing them
;;;###autoload
(defun twit-list-followers ()
  "Display a list of all your twitter.com followers' names."
  (interactive)
  (pop-to-buffer "*Twit-followers*")
  (kill-region (point-min) (point-max))
  (loop for name in 
        (loop for name in
              (loop for user in 
                    (xml-get-children
                     (cadr (twit-parse-xml twit-followers-file "GET")) 'user)
                    collect (sixth user))
              collect (third name))
        do (insert (concat name "\n")))
  ;; set up mode as with twit-show-recent-tweets
  (text-mode)
  (use-local-map twit-followers-mode-map))

;;; Helper function to insert text into buffer, add an overlay and
;;; apply the supplied attributes to the overlay
;;* helper write
(defun twit-insert-with-overlay-attributes (text attributes)
  (let ((start (point)))
    (insert text)
    (let ((overlay (make-overlay start (point))))
      (dolist (spec attributes)
        (overlay-put overlay (car spec) (cdr spec))))))


;;* twit follow timer interactive
;;;###autoload
(defun twit-follow-recent-tweets ()
  "Sets up a timer, and shows you the most recent tweets approx every 90 seconds.

You can change the time between each check by customizing `tiwt-follow-idle-interval'."
  (interactive)
  (twit-show-recent-tweets)
  (twit-verify-and-set-rate-limit (twit-get-rate-limit))
  (setq twit-rate-limit-timer (run-with-timer twit-rate-limit-interval twit-rate-limit-interval 'twit-get-and-set-async-rate-limit))
  (setq twit-timer (run-with-timer twit-shadow-follow-idle-interval twit-shadow-follow-idle-interval 'twit-follow-recent-tweets-timer-function)))

;;* twit follow timer interactive
(defun twit-stop-following-tweets ()
  "When you want to stop following tweets, you can use this function to turn off the timer."
  (interactive)
  (if (featurep 'todochiku)
	  (todochiku-message "Twit.el" "Twit.el Stopped Following Tweets" (todochiku-icon 'social)))
  (cancel-timer twit-timer)
  (cancel-timer twit-rate-limit-timer))

;;* tweet show refactorme interactive memoryleak
;; minor modes might be a cause of the memoryleak, see about removing them
;;;###autoload
(defun twit-show-recent-tweets (page)
  "Display a list of the most recent tweets from people you're following.

You can use a prefix argument (C-u <number>) to skip between pages.  If you are
following tweets by using `twit-follow-recent-tweets', these might get overwritten.
 (let me know if that behavior bugs you, and I will make follow-tweets write into
a different buffer.)

Currently there is a bug where the first time you show tweets, it might come up
empty.  This is being worked on.

Patch version from Ben Atkin."
  (interactive "P")
  (when (or (null page)
			(<= page 1))
		(setq page 1))
  (let ((b (get-buffer-create "*Twit-recent*")))
    (display-buffer b)
    (with-current-buffer b
      (toggle-read-only 0)
      (twit-write-recent-tweets (twit-parse-xml (format twit-friend-timeline-file page) "GET"))
      ;; !set up some sensible mode and useful bindings
      (text-mode)
      (toggle-read-only 1)
	  (use-local-map twit-status-mode-map))))

;;* direct refactorme show interactive memoryleak
;; minor modes might be a cause of the memoryleak, see about removing them
;;;###autoload
(defun twit-show-direct-tweets (page)
   "Display a list of the most recent direct tweets.

With a numeric prefix argument, it will skip to that page like `twit-show-recent-tweets'."
   (interactive "P")
   (when (or (null page)
			 (<= page 1))
		 (setq page 1))
   (let ((b (get-buffer-create "*Twit-direct*")))
	 (display-buffer b)
	 (with-current-buffer b
	    (toggle-read-only 0)
		(buffer-disable-undo)
		(delete-region (point-min) (point-max))
		(twit-insert-with-overlay-attributes (format-time-string "Direct messages: %c\n") '((face . "twit-title-face")))

		(let ((times-through 0))
		  (dolist (msg-node (xml-get-children (cadr (twit-parse-xml (format twit-direct-msg-get-url page) "GET")) 'direct_message))
				  (twit-write-tweet msg-node nil times-through)
				  (setq times-through (+ 1 times-through))))

		(text-mode)
		(toggle-read-only 1)
		(use-local-map twit-status-mode-map))))

;;* mode
;;;###autoload
(define-minor-mode twit-mode 
  "Toggle twit-mode, a minor mode that binds some keys for posting.
Globally binds some keys to Twit's interactive functions.

With no argument, this command toggles the mode. 
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

\\{twit-mode-map}" nil
" Twit" 
'(("\C-c\C-tp" . twit-post)
  ("\C-c\C-tr" . twit-post-region)
  ("\C-c\C-tb" . twit-post-buffer)
  ("\C-c\C-tf" . twit-list-followers)
  ("\C-c\C-ts" . twit-show-recent-tweets))
 :global t
 :group 'twit
 :version twit-version-number)

(provide 'twit)

;;; twit.el ends here
