;;; abacus.el --- Abacus Calculator

;; Copyright (C) 2001 by Valvassori Moïse

;; Author: Valvassori Moïse <m...@valvassori.org>
;; Version: 1.0.2
;; Last modification: Tue Oct 16 00:25:02 CEST 2001
;; Keywords: games

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is an Abacus calculator


;;; Install:

;; To install Abacus put in your .emacs this kind of line:
;; (autoload 'abacus "abacus" "The Abacus Simulator from Hell" t)


;;; Usage:

;;  Invoke:
;;  M-x abacus


;; Bugs:

;; GNU Emacs represent integer on 28 bits, providing a bug in
;; \\[abacus-set-number]

;; Why keypad does not work on Xemacs ?


;;; History:

;; v1.0.0
;; First public release of this Abacus

;; v1.0.1
;; Correction of Bugs repported by Adam Duck
;; Now it works on Xemacs and GNU Emacs 21

;; v1.0.2
;; Adding Faces

;;; Code:


(defcustom abacus-number-of-column 13
  "Number of digit in this abacus."
  :type 'integer
  :group 'abacus)
(defvar abacus-next-column-step 3)
(setq abacus-fisrt-panel-col 3)
(setq abacus-fisrt-panel-line 3)
(setq abacus-second-panel-col 3)
(setq abacus-second-panel-line 7)
(setq abacus-third-panel-line 15)
(setq abacus-direction-panel-line 17)
(setq abacus-action-panel-line 18)
(setq abacus-numbers (make-vector abacus-number-of-column 0))
(setq abacus-cursor-pos 0)
(setq abacus-direction 'ltr)				    ; ltr: left to right
							    ; rtl: right to left
(setq abacus-action 'none)

(defface abacus-frame-face
  '((t (:bold t :background "tan" :foreground "white")))
  "Face for the Abacus frame")
(defface abacus-bead-face
  '((t (:bold t :foregroung "white")))
  "Face for Abacus bead")
(defface abacus-rod-face
  '((t (:bold nil :foregroung "burlywood")))
  "Face for Abacus rod")
(defface abacus-digit-face
  '((t (:bold t :background "tan" :foreground "black")))
  "Face for the Abacus digit")



(defvar abacus-mode-map nil ".")
(if abacus-mode-map
    ()
  (setq abacus-mode-map (make-sparse-keymap))
  (define-key abacus-mode-map "\C-c\C-s" 'abacus-set-number)
  (define-key abacus-mode-map "\C-l" 'abacus-display-abacus)
  (define-key abacus-mode-map "0" 'abacus-key-0)
  (define-key abacus-mode-map "1" 'abacus-key-1)
  (define-key abacus-mode-map "2" 'abacus-key-2)
  (define-key abacus-mode-map "3" 'abacus-key-3)
  (define-key abacus-mode-map "4" 'abacus-key-4)
  (define-key abacus-mode-map "5" 'abacus-key-5)
  (define-key abacus-mode-map "6" 'abacus-key-6)
  (define-key abacus-mode-map "7" 'abacus-key-7)
  (define-key abacus-mode-map "8" 'abacus-key-8)
  (define-key abacus-mode-map "9" 'abacus-key-9)
  (define-key abacus-mode-map " " 'abacus-toggle-direction)
  (define-key abacus-mode-map "+" 'abacus-toggle-action)
  (define-key abacus-mode-map [right] 'abacus-move-right)
  (define-key abacus-mode-map [left] 'abacus-move-left))



(defun abacus-move-right ()
  "Move the cursor on the next column."
  (interactive)
  (setq abacus-cursor-pos (% (1+ abacus-cursor-pos) abacus-number-of-column))
  (abacus-move-cursor-to-column abacus-cursor-pos))
(defun abacus-move-left ()
  "Move the cursor on the preceding column."
  (interactive)
  (setq abacus-cursor-pos (let ((x (1- abacus-cursor-pos)))
			    (if (< x 0)
				(1- abacus-number-of-column)
			      x)))
  (abacus-move-cursor-to-column abacus-cursor-pos))
(defun abacus-key-0 ()
  "You prees the 0 key."
  (interactive)
  (abacus-do-numbered-key 0))
(defun abacus-key-1 ()
  "You prees the 1 key."
  (interactive)
  (abacus-do-numbered-key 1))
(defun abacus-key-2 ()
  "You prees the 2 key."
  (interactive)
  (abacus-do-numbered-key 2))
(defun abacus-key-3 ()
  "You prees the 3 key."
  (interactive)
  (abacus-do-numbered-key 3))
(defun abacus-key-4 ()
  "You prees the 4 key."
  (interactive)
  (abacus-do-numbered-key 4))
(defun abacus-key-5 ()
  "You prees the 5 key."
  (interactive)
  (abacus-do-numbered-key 5))
(defun abacus-key-6 ()
  "You prees the 6 key."
  (interactive)
  (abacus-do-numbered-key 6))
(defun abacus-key-7 ()
  "You prees the 7 key."
  (interactive)
  (abacus-do-numbered-key 7))
(defun abacus-key-8 ()
  "You prees the 8 key."
  (interactive)
  (abacus-do-numbered-key 8))
(defun abacus-key-9 ()
  "You prees the 9 key."
  (interactive)
  (abacus-do-numbered-key 9))
(defun abacus-toggle-direction ()
  "Change the direction of the text: ltr or rtl."
  (interactive)
  (setq abacus-direction
       (if (eq abacus-direction 'ltr)
      'rtl
    'ltr))
  (abacus-display-direction)
  (abacus-move-cursor-to-column abacus-cursor-pos))
(defun abacus-toggle-action ()
  "Change the action: addition or edition."
  (interactive)
  (setq abacus-action
	(if (eq abacus-action 'addition)
	    'none
	  'addition))
  (abacus-display-action)
  (abacus-move-cursor-to-column abacus-cursor-pos))
(defun abacus-set-number (n)
  "Put a number on the abacus.
Argument N bloops."
  (interactive "nNumber: ")
  (abacus-display-abacus)
  (let ((i (1- abacus-number-of-column)))
    (while (>= i 0)
      (abacus-display-digit i (% n 10))
      (aset abacus-numbers i (% n 10))
      (setq n (/ n 10))
      (setq i (1- i))
      )))





(defun abacus-do-numbered-key (n)
  "Do the appropriate action when a numeric key is pressed.
Argument N the number of the key (0,1,...,9)."
  (if (eq abacus-action 'addition)
      (abacus-addition-action n abacus-cursor-pos)
    (abacus-none-action n abacus-cursor-pos))
  (if (eq abacus-direction 'ltr)
      (abacus-move-right)
    (abacus-move-left)))
(defun abacus-none-action (n col)
  "Set the number on the abacus.
Argument N the number to display.
Argument COL the colunm where the number is."
  (aset abacus-numbers col n)
  (abacus-display-nth-column col))
(defun abacus-addition-action (n col)
  "Add the number with the column.
Argument N the number to add.
Argument COL the column to add."
  (let* ((res (+ n (aref abacus-numbers col)))
	 (out (% res 10))
	 (carry (/ res 10)))
    (abacus-none-action out col)
    (if (and (> carry 0)
	     (> col 0))
	(abacus-addition-action carry (1- col)))))




(defun abacus-kill-line ()
  "Kill a line."
  (insert " ")
  (beginning-of-line)
  (kill-line))
(defun abacus-move-cursor-to-column (n)
  (goto-line abacus-third-panel-line)
  (move-to-column (abacus-get-text-column n)))

(defun abacus-get-text-column (n)
  "Get the real text column of an abacus column.
Argument N the column."
  (+ abacus-fisrt-panel-col (* n abacus-next-column-step)))
(defun abacus-repeat (n fct)
  "Repeat N time function.
Argument FCT the function to repeat."
  (while (> n 0)
    (progn
      (funcall fct)
      (setq n (1- n)))))





(defun abacus-display-external-frame ()
  (let ((pt (point)))
    (insert (concat "+"
		    (make-string (+ 2 (* abacus-next-column-step abacus-number-of-column)) ?-)
		    "+\n"))
    (add-text-properties pt (1- (point)) '(face abacus-frame-face))))
(defun abacus-display-internal-frame ()
  (let ((pt (point)))
    (insert (concat "|+"
		    (make-string (+ 0 (* abacus-next-column-step abacus-number-of-column)) ?-)
		    "+|\n"))
    (add-text-properties pt (1- (point)) '(face abacus-frame-face))))
(defun abacus-display-empty-line ()
  (let ((pt (point)))
    (insert (concat "||"
		    (make-string (+ 0 (* abacus-next-column-step abacus-number-of-column)) ? )
		    "||\n"))
    (add-text-properties pt (+ pt 2) '(face abacus-frame-face)))
    (add-text-properties (- (point) 3) (1- (point)) '(face abacus-frame-face)))
  
(defun abacus-display-direction ()
  (goto-line abacus-direction-panel-line)
  (abacus-kill-line)
  (insert "Direction: ")
  (if (eq abacus-direction 'ltr)
      (insert "-->")
    (insert "<--")))
(defun abacus-display-action ()
  (goto-line abacus-action-panel-line)
  (abacus-kill-line)
  (insert "Action: ")
  (insert (symbol-name abacus-action)))
(defun abacus-display-abacus ()
  "Display an empty abacus."
  (interactive)
  (switch-to-buffer "*Abacus*")
  (erase-buffer)
  (goto-char 0)
  (abacus-display-external-frame)
  (abacus-display-internal-frame)
  (abacus-repeat 3 'abacus-display-empty-line)
  (abacus-display-internal-frame)
  (abacus-repeat 7 'abacus-display-empty-line)
  (abacus-display-internal-frame)
  (abacus-display-internal-frame)
  (abacus-display-external-frame)
  (let ((i 0))
    (while (< i abacus-number-of-column)
      (abacus-display-nth-column i)
      (setq i (1+ i))
      ))
  (goto-line abacus-direction-panel-line)
  (insert "\n\n\n")
  (abacus-display-direction)
  (abacus-display-action)
  (abacus-move-cursor-to-column abacus-cursor-pos))



(defun abacus-display-nth-column (n)
  (abacus-display-digit n (aref abacus-numbers n)))
(defun abacus-display-digit-aux (gap n c)
  (delete-char 1)
  ; (insert
  ;  (if (and (>= n 0)
  ; 	    (< n gap))
  ;      ?|
  ;    ?o))
  (if (and (>= n 0)
	   (< n gap))
      (progn (insert ?|)
	     (add-text-properties (1- (point)) (point) '(face abacus-rod-face)))
    (progn (insert ?o)
	   (add-text-properties (1- (point)) (point) '(face abacus-bead-face))))
  (forward-line 1)
  (move-to-column c))

(defun abacus-display-digit (column digit)
  "Draw the DIGIT in the desired COLUMN."
  (let ((up-panel-value (- 0 (/ digit 5)))
	(down-panel-value (- 0 (% digit 5)))
	(column (abacus-get-text-column column)))
    (goto-line abacus-fisrt-panel-line)
    (move-to-column column)
    (let ((i 3))
      (while (> i 0)
	(abacus-display-digit-aux 1 (+ (setq i (1- i)) up-panel-value) column)))
    (goto-line abacus-second-panel-line)
    (move-to-column column)
    (let ((i -1))
      (while (< i 6)
	(abacus-display-digit-aux 2 (+ (setq i (1+ i)) down-panel-value) column)))
    (goto-line abacus-third-panel-line)
    (move-to-column column)
    (delete-char 1)
    (insert (number-to-string (% digit 10)))
    (add-text-properties (1- (point)) (point) '(face abacus-digit-face))
    ))



(defun abacus-mode ()
  "Abacus calculator.

\\[abacus] launch Abacus

\\{abacus-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map abacus-mode-map)
  
  (setq mode-name "Abacus")
  (setq major-mode 'abacus-mode)
  (run-hooks 'abacus-mode-hook))

;;;###autoload
(defun abacus ()
  "Abacus calculator."
  (interactive)
  (abacus-display-abacus)
  (abacus-mode))

(provide 'abacus)

;;; abacus.el ends here
