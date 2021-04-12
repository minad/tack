;;; tack.el --- Modal keybindings -*- lexical-binding: t -*-

;; Author: Daniel Mendler
;; Created: 2020
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://github.com/minad/tack

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Modal keybindings

;;; Code:

(require 'seq)
(require 'subr-x)

(defvar-local tack--lighter nil)
(defvar tack--map-alist nil)
(push 'tack--map-alist emulation-mode-map-alists)

(defvar tack-base-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\e ?\e ?\e] #'tack-disable)
    (define-key map [?\C-u] #'tack--universal-argument)
    (define-key map [?u] #'tack--universal-argument)
    (define-key map [?-] #'tack--negative-argument)
    (define-key map [kp-subtract] #'tack--negative-argument)
    (dotimes (n 10)
     (define-key map (vector (intern (format "kp-%s" n))) #'tack--digit-argument)
     (define-key map (vector (+ ?0 n)) #'tack--digit-argument))
    map)
  "Keymap used as parent keymap for the Tack maps.")

;; The functions `universal-argument', `digit-argument' and `negative-argument' must be
;; replicated for Tack, since the Emacs functions push their own transient map.
;; This means that the tack keys like "u" do not work while the transient map is active.
(defun tack--universal-argument (arg)
  "Replacement for `universal-argument', to be used by Tack. Takes prefix ARG."
  (interactive "P")
  (prefix-command-preserve-state)
  (setq prefix-arg (cond
                    ((consp arg) (list (* 4 (car arg))))
                    ((eq arg '-) '(-4))
                    (t '(4)))))

(defun tack--digit-argument (arg)
  "Replacement for `digit-argument', to be used by Tack. Takes prefix ARG."
  (interactive "P")
  (prefix-command-preserve-state)
  (let* ((char (if (integerp last-command-event)
		   last-command-event
		 (get last-command-event 'ascii-character)))
	 (digit (- (logand char ?\177) ?0)))
    (setq prefix-arg (cond ((integerp arg)
                            (+ (* arg 10)
			       (if (< arg 0) (- digit) digit)))
                           ((eq arg '-)
                            (if (zerop digit) '- (- digit)))
                           (t
                            digit)))))

(defun tack--negative-argument (arg)
  "Replacement for `negative-argument', to be used by Tack. Takes prefix ARG."
  (interactive "P")
  (prefix-command-preserve-state)
  (setq prefix-arg (cond ((integerp arg) (- arg))
                         ((eq arg '-) nil)
                         (t '-))))

(defun tack--reject (keys map)
  "Remove all KEYS from property MAP."
  (let ((res))
    (while map
      (if (memq (car map) keys)
          (setq map (cddr map))
        (push (car map) res)
        (setq map (cdr map))))
    (nreverse res)))

(defun tack--define-keys (map prefix keys cmd &optional desc)
  "Bind a list of KEYS with PREFIX to (DESC . CMD) in the keymap MAP."
  (mapcar (lambda (k) `(define-key ,map ,(vconcat prefix (kbd k))
                         ,(if desc `(cons ,desc #',cmd) `#',cmd)))
          (if (listp keys) keys (list keys))))

(defun tack--translate ()
  "Translate tack keybinding."
  (interactive)
  (when-let (key (key-binding (vconcat [tack--translate] (this-single-command-keys))))
    (setq unread-command-events (append key unread-command-events))
    (prefix-command-preserve-state)))

(defun tack--cmd (name map keys cmd)
  "Bind KEYS to CMD in tack MAP of tack state NAME."
  (macroexp-progn
   (cond
    ((symbolp cmd)
     (tack--define-keys map nil keys cmd))
    ((stringp cmd)
     (append
      (tack--define-keys map [tack--translate] keys (kbd cmd))
      (tack--define-keys map nil keys 'tack--translate (key-description (kbd cmd)))))
    (t (let ((sym (intern (format "%s/%s" name cmd))))
         (cons
          `(defun ,sym () (interactive) ,cmd)
          (tack--define-keys map nil keys sym (format "%S" cmd))))))))

(defun tack--opt-hook (opts name)
  "Get hook option NAME from OPTS plist."
  (if-let (x (plist-get opts name))
      (if (symbolp x) `((,x)) `(,x))))

(defmacro tack-define (name &rest opts)
  "Tack state with NAME and OPTS."
  (declare (indent defun))
  (let* ((map (intern (format "%s/map" name)))
         (body (tack--reject '(:on :off :base-map :lighter) opts))
         (opt-on (tack--opt-hook opts :on))
         (opt-off (tack--opt-hook opts :off))
         (opt-base-map (or (plist-get opts :base-map) 'tack-base-map))
         (opt-lighter (or (plist-get opts :lighter) (symbol-name name))))
    `(progn
       (with-no-warnings (defvar-local ,name nil))
       (defvar ,map (make-composed-keymap (make-sparse-keymap) ,opt-base-map))
       (defun ,name ()
         (interactive)
         (if ,name
             (progn
               ,@opt-off
               (setq ,name nil
                     tack--lighter nil))
           ,@opt-on
           (setq ,name t
                 tack--lighter ,opt-lighter))
         (force-mode-line-update t))
       ,@(mapcar (pcase-lambda (`(,keys ,cmd)) (tack--cmd name map keys cmd)) body)
       (push (cons ',name ,map) tack--map-alist))))

;;;###autoload
(define-minor-mode tack-mode
  "Minor mode which shows the current tack state in the mode-line."
  :global t
  (setq mode-line-misc-info (assq-delete-all 'tack--lighter mode-line-misc-info)
        tack--lighter nil)
  (when tack-mode
    (push '(tack--lighter ("[" (:propertize tack--lighter face (:inherit error :weight normal)) "] ")) mode-line-misc-info)))

(provide 'tack)
;;; tack.el ends here
