;;; hop.el --- An easymotion-like plugin highly inspired from hop.nvim

;; Author: Animesh Sahu <animeshsahu19@yahoo.com>
;; URL: https://github.com/Animeshz/hop.el
;; Version: 0.1
;; Package-Requires: ((emacs "28.0"))


;;; Commentary:

;; Emacs Motion on Speed!
;; Move anywhere in your buffer with 1 or 2 characters.
;;
;; Defines following (TODO: tentative)
;; hop-word
;; hop-char
;; hop-line
;; hop-line-skip-whitespace
;; hop-pattern (maybe?)


;;; Code:

;; User Facing Options :: BEGIN
(defcustom hop-jump-keys "asdghklqwertyuiopzxcvbnmfj"
  "The keys to be permutated for hopping/jumping, rightmost are splitted first.")
(defcustom hop-uppercase-hints nil
  "A non-nil value means the keys will be displayed in uppercase.")
(defcustom hop-hints-position 'start
  "Where the hop hints be in a matched string.")

(defcustom hop-quit-key "<esc>"
  "The key to quit hop")
(defcustom hop-all-windows t
  "Apply hop in all opened windows in current frame or in just current window.")

(defface hop-face-dim-unmatched `((t (:foreground "#666666" :background ,(face-background 'default) :weight bold)))
  "Face used for all unmatched characters.")
(defface hop-face-single-char `((t (:foreground "#ff007c" :background ,(face-background 'default) :weight bold)))
  "Face used for all near-cursor single character hops/jumps.")
(defface hop-face-double-char-1 `((t (:foreground "#00dfff" :background ,(face-background 'default) :weight bold)))
  "Face used for first character of all double character hops/jumps.")
(defface hop-face-double-char-2 `((t (:foreground "#2b8db3" :background ,(face-background 'default) :weight bold)))
  "Face used for second character of all double character hops/jumps.")
;; User Facing Options :: END

;; Helper Definitions :: BEGIN
(defun hop-window-list ()
  "List of windows selected for hop/jump."
  (if (eq hop-all-windows t) (window-list) (list (selected-window))))
;; Helper Definitions :: END

;; Dimming Overlay Logic :: BEGIN
(defvar hop--dim-overlay-save nil
  "Hold overlays for cleanup later.")

(defun hop--dim-overlay (wnd-list)
  "Applies dim-overlay to all the windows in wnd-list."
  (setq hop--dim-overlay-save
        (mapcar (lambda (w)
                  (let ((ol (make-overlay
                             (window-start w)
                             (window-end w)
                             (window-buffer w))))
                    (overlay-put ol 'face 'hop-face-dim-unmatched)
                    (overlay-put ol 'window w)
                    ol))
                wnd-list)))

(defun hop--dim-overlay-done ()
  "Clean up overlays."
  (mapc #'delete-overlay hop--dim-overlay-save)
  (setq hop--dim-overlay-save nil))
;; Dimming Overlay Logic :: END

;; Pattern Matching Logic :: BEGIN

; PCRE Regex:
; https://github.com/illikainen/are
; https://github.com/syohex/emacs-pcre
;
; WORD: (?<=\s|^)\w
; LINE: ^
; LINE-SKIP-WHITESPACE: (?<=^\s+)\w
; CHAR: escape(<c>)
; PATTERN: <PATTERN>

;; Pattern Matching Logic :: END

;; Hop Character Overlay Logic :: BEGIN
;; Hop Character Overlay Logic :: END

;; Updation and Movement Logic :: BEGIN
;; Updation and Movement Logic :: END

;; Main Logic | API :: BEGIN
;; Main Logic | API :: END


(provide 'hop)
;;; my-package.el ends here
