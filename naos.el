;;; naos.el --- Naos User Frontend for GNU Emacs
;; Copyright (C) 1999 Klaus Schilling

;; Author: Klaus Schilling <Klaus.Schilling@home.ivm.de>
;; Created: 2 Feb 1999
;; Version: 0.0.1
;; Keywords: games roleplay 

;; This file is not part of GNU Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
     
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
     
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This is a user frontend for the role play game Naos based on the Emacs
;; Widget package by P. Abrahamsen.

;;; Code:

(defvar gamedriver "/home/klaus/naos/naos.scm")

;; loads P. Abrahamsen's Widget Package
(require 'widget)

;; string functions from elib
(require 'string)
     
(eval-when-compile
  (require 'wid-edit))


;; submit button
(defun naos-submit-button ()
  "Create the button which triggers the result of the current page to be submitted to the gamedriver, as regular answer to the gamedriver's query."
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (let ((res (apply 'concat (append answer '("\n")))))
			     (process-send-string process res))
			   (catch 'done
			     (progn
			       (save-excursion
				 (set-buffer process-buffer)
				 (naos-parse-output
				  process
				  (naos-read-output process))))
			     ))
		 "Submit"))


;;Launching a new page in Naos
(defun naos-new-page ()
  "Let a new page show up"
  (switch-to-buffer "*naos-client*")
  (setq answerl '())
  (let ((inhibit-read-only t))
    (erase-buffer))
  (let ((all (overlay-lists)))
    ;; Delete all the overlays.
    (mapcar 'delete-overlay (car all))
    (mapcar 'delete-overlay (cdr all))))

;;Completes a Page and sets it up
(defun naos-page-setup ()
  (widget-insert "\n\n")
  (setq answer
	(let* ((l answerl)(ll (length l))(j ll)(ans (make-vector ll " ")))
	  (while (> j 0)
	    (aset ans (- j 1) (car l))
	    (setq l (cdr l))
	    (setq j (- j 1)))
	  ans))
  (naos-submit-button)
  (use-local-map widget-keymap)
  (widget-setup))

;;Info Link
(defun naos-info-link (addr)
  "Links to the indicated node in the Naos Online Help Facility."
  (widget-create
   'info-link
   :value (format ("(naos)%s" addr))))

;;Checkbox
(defun naos-checkbox (index val item)
  "Lets the player select or deselect some option."
  (setq answerl (cons (format " %s" val) answerl))
  (widget-create
   'checkbox
   :format "%[%v%] %d"
   :doc item
   :tag (format "%d" index)
   :value (not (string= val "0"))
   :notify (lambda (widget &rest ignore)
	     (aset
	      answer
	      (string-to-number (widget-get widget ':tag))
	      (if (widget-value widget) " 1" " 0")))))

;;Radiolist 
(defun naos-radiolist (index pre-chosen items)
  "Let's the player choose exactly one of the given options."
  (if (null items)
      (setq answerl (cons " -1"	 answerl))
      (progn
  (setq answerl (cons (format " %s" pre-chosen) answerl))
  (let ((i 0)
	(it items)
	(widget
	 (widget-create 'radio-button-choice
			:tag (format "%d" index)
			:value (format " %s" pre-chosen)
			:notify (lambda (widget &rest ignore)
				  (aset
				   answer
				   (string-to-number (widget-get widget ':tag))
				   (widget-value widget))))))
    (while (not (null it))
      (widget-radio-add-item
       widget
       (list
	'item
	:format "%d"
	:doc (car it)
	:value (format " %d" i)))
      (setq i (+ 1 i))
      (setq it (cdr it)))))))

(defun naos-read-output (process)
  (goto-char (point-min))
  (while (not (search-forward "\r\n" nil t))
    (accept-process-output process)
    (goto-char (point-min)))
  (goto-char (point-min))
  (buffer-substring (point) (- (point-max) 2)))

(defun naos-parse-output (proc string)
  (let* ((content string)(index 0))
    (erase-buffer)
    (naos-new-page)
    (while (not (string= content ""))
      (cond
       ((equal (string-match "/quit .*" content) 0) (throw 'done nil))
       ((equal (string-match "/info .*" content) 0)
	(setq arg (substring (match-string 0 content) 6))
	(naos-info-link arg))
       ((equal (string-match "/radio [0-9]+.*" content) 0)
	(setq args (cdr (string-split "[ \t]+" (match-string 0 content))))
	(naos-radiolist index (car args) (cdr args))
	(setq index (+ 1 index)))
       ((equal (string-match "/check .*" content) 0)
	(setq args (cdr (string-split "[ \t]+" (match-string 0 content))))
	(naos-checkbox index (car args) (car (cdr args)))
	(setq index (+ 1 index)))
       ((equal (string-match ".*\n" content) 0)
	(widget-insert (match-string 0 content)))
       (t (widget-insert content)))
      (setq content (substring content (+ 1 (or (string-match "\n" content) (- (length content) 1))))))
    (setq answer (make-vector index " "))
    (naos-page-setup)) )


(defun naos-sentinel (proc event)
  (message (format "%s received %s" proc event))
  (cond ((string= event "finished\n") (kill-buffer "*naos-client*"))
	((string-match "exited abnormally" event)
	 (kill-buffer "*naos-client*")
	 (switch-to-buffer "*naos*")
	 (message (buffer-string))
	 (kill-buffer "*naos*"))))
		
(defun naos ()
  "Start the Naos program."
  (interactive)
  (setq process-buffer (get-buffer-create "*naos*"))
  (setq process-connection-type t)
  (make-local-variable 'answer)
  (save-excursion
    (set-buffer process-buffer)
    (buffer-disable-undo process-buffer)
    (erase-buffer))
  (setq process (start-process "naos" process-buffer gamedriver))
  (process-kill-without-query process)
  (set-process-sentinel process 'naos-sentinel)
  (save-excursion
    (set-buffer process-buffer)
    (naos-parse-output process (naos-read-output process))))

(provide 'naos)
