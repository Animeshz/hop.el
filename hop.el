;;; hop.el --- An easymotion-like plugin highly inspired from hop.nvim

;; Author: Animesh Sahu <animeshsahu19@yahoo.com>
;; URL: https://github.com/Animeshz/hop.el
;; Version: 0.1
;; Package-Requires: ((emacs "28.0"))


;;; Commentary:

;; Emacs Motion on Speed!
;; Move anywhere in your buffer with 1 or 2 characters.
;;
;; It requires https://github.com/syohex/emacs-pcre
;; If you're using elpaca/straight as package manager, write the following package declaration:
;;
;; ```
;; ;; install pcre(-dev) package first from system package manager
;; (use-package pcre
;;   :elpaca (pcre :host github :repo "syohex/emacs-pcre"
;;                 :pre-build ("make" "all")
;;                 :files (:default "pcre.el" "pcre-core.so")))
;; (use-package hop
;;   :elpaca (hop :host github :repo "Animeshz/hop.el"))
;; ```
;;
;; Defines following (TODO: tentative)
;; hop-word
;; hop-char
;; hop-line
;; hop-line-skip-whitespace


;;; Code:

(require 'cl-lib)
(require 'pcre)

;; User Facing Options :: BEGIN
(defcustom hop-jump-keys "asdghklqwertyuiopzxcvbnmfj"
  "The keys to be permutated for hopping/jumping, rightmost are splitted first."
  :type 'string)

(defcustom hop-uppercase-hints nil
  "A non-nil value means the keys will be displayed in uppercase."
  :type 'boolean)

(defcustom hop-hints-position 'start
  "Where the hop hints be in a matched string."
  :type '(choice
          (const :tag "start" start)
          (const :tag "middle" middle)
          (const :tag "end" end)))

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

(defcustom hop-word-regex "((?:[A-Za-z0-9_]|(?<=\\w)-(?=\\w)(?![A-Z]))+)"
  "Regex to use when matching a word")
(defcustom hop-line-regex "(^[^\\S\\r\\n]$)"
  "Regex to use when matching a line")
(defcustom hop-line-skip-whitespace-regex "^[^\\S\\r\\n]*([\\S\\r\\n])"
  "Regex to use when matching a line skipping whitespace characters")
;; User Facing Options :: END


;; Dependent Variables :: BEGIN
(defvar hop--single-key-list '()
  "Set of all possible single-char combinations that can be made using `hop-jump-keys`")
(defvar hop--double-key-list '()
  "Set of all possible double-char combinations that can be made using `hop-jump-keys`")

(defun hop--jump-key-watcher (symbol newval op where)
  (setq hop--single-key-list (mapcar 'string newval)
        hop--double-key-list '())
  (let ((hop-jump-keys-len (length newval)))
    (dotimes (i hop-jump-keys-len)
      (dotimes (j hop-jump-keys-len)
        (push (concat
               (substring newval i (1+ i))
               (substring newval j (1+ j)))
              hop--double-key-list)))
    (setq hop--double-key-list (reverse hop--double-key-list))))

(add-variable-watcher 'hop-jump-keys #'hop--jump-key-watcher)
(hop--jump-key-watcher nil hop-jump-keys nil nil)
;; Dependent Variables :: END

;; Helper Definitions :: BEGIN
(defun hop-window-list ()
  "List of windows selected for hop/jump."
  (if (eq hop-all-windows t) (window-list) (list (selected-window))))

(defun hop-indices-with-prefix (prefix string-list)
  "Get the indices of all strings in STRING-LIST that start with PREFIX."
  (let ((indices '()))
    (dotimes (i (length string-list) indices)
      (when (string-prefix-p prefix (nth i string-list))
        (push i indices)))))

(defun hop-calculate-jump-char (match)
  "Evaulate jump position respecting `hop-hints-position`"
  (let* ((s (car (car match)))
         (e (cdr (car match)))
         (result (cond ((eq hop-hints-position 'start) s)
                       ((eq hop-hints-position 'middle) (truncate (+ s e) 2))
                       ((eq hop-hints-position 'end) e))))
    result))
;; Helper Definitions :: END

;; Dimming Overlay Logic :: BEGIN
(defvar hop--dim-overlay-save nil
  "Hold overlays for cleanup later.")

(defun hop--dim-overlay (windows)
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
                windows)))

(defun hop--dim-overlay-done ()
  "Clean up overlays."
  (mapc #'delete-overlay hop--dim-overlay-save)
  (setq hop--dim-overlay-save nil))
;; Dimming Overlay Logic :: END

;; Pattern Matching Logic :: BEGIN
(defun hop--regex-matches-in-windows (regex-pattern windows)
  "Return a list of matches ((BEG . END) . WND), sorted by distance from the cursor position."
  (let* ((matches ())
         (cursor-pos (point)))
    (dolist (window windows)
      (with-selected-window window
        (save-excursion
          (goto-char (window-start window))
          (setq default-case-fold-search case-fold-search
                case-fold-search nil)
          (while (and (< (point) (window-end window))
                      (pcre-re-search-forward regex-pattern (window-end window) t))
            (let ((match-pos (match-beginning 1)))
              (push (cons (cons match-pos (match-end 0)) window) matches)))
          (setq case-fold-search default-case-fold-search))))
    (sort matches (lambda (x y)
                    (< (abs (- (car (car x)) cursor-pos))
                       (abs (- (car (car y)) cursor-pos)))))))

(defun hop--generate-jump-keys (n)
  "Return all combinations of length LENGTH from the characters in STRING using hop-trie-backtrack-filling algorithm."
  (let* ((hop-jump-keys-len (length hop-jump-keys))
         (split-len (- hop-jump-keys-len 1))
         (num-splits (ceiling (truncate (- n 1) split-len)))
         (num-single-hop-jump-keys (min n (max 0 (- hop-jump-keys-len num-splits))))
         (num-double-hop-jump-keys (max 0 (- n num-single-hop-jump-keys))))
    (append (cl-subseq hop--single-key-list 0 num-single-hop-jump-keys) (last hop--double-key-list num-double-hop-jump-keys))))
;; Pattern Matching Logic :: END

;; Hop Character Overlay Logic :: BEGIN
(defun hop--jump-overlay (matches hop-key)
  "Setup hints for hopping/jumping"
  (setq hop--key-overlay-save
        (flatten-tree
         (cl-mapcar #'(lambda (m hk)
                        (let* ((start (car (car m)))
                               (end (cdr (car m)))
                               (window (cdr m))
                               (marker-len (length hk))
                               (marker-begin (hop-calculate-jump-char m)))
                          (if (= marker-len 1) (let ((ol1 (make-overlay
                                                           marker-begin
                                                           (1+ marker-begin)
                                                           (window-buffer window))))
                                                 (overlay-put ol1 'display hk)
                                                 (overlay-put ol1 'window window)
                                                 (overlay-put ol1 'face 'hop-face-single-char)
                                                 (list ol1))
                            (let ((ol1 (make-overlay
                                        marker-begin
                                        (1+ marker-begin)
                                        (window-buffer window)))
                                  (ol2 (make-overlay
                                        (1+ marker-begin)
                                        (+ marker-begin 2)
                                        (window-buffer window))))
                              (overlay-put ol1 'display (substring hk 0 1))
                              (overlay-put ol1 'window window)
                              (overlay-put ol1 'face 'hop-face-double-char-1)
                              (overlay-put ol2 'display (substring hk 1 2))
                              (overlay-put ol2 'window window)
                              (overlay-put ol2 'face 'hop-face-double-char-2)
                              (list ol1 ol2)))))
                    matches hop-key))))

(defun hop--jump-overlay-done ()
  "Clear hints for hopping/jumping"
  (mapc #'delete-overlay hop--key-overlay-save)
  (setq hop--key-overlay-save nil))
;; Hop Character Overlay Logic :: END

;; Main Logic (Updation and Movement) | API :: BEGIN
(defun hop-word ()
  (interactive)
  (let* ((windows (hop-window-list))
         (matches (hop--regex-matches-in-windows hop-word-regex windows))
         (keys (hop--generate-jump-keys (length matches))))
    (hop--dim-overlay windows)
    (hop--jump-overlay matches keys)
    (let ((key-indices (hop-indices-with-prefix (string (read-char)) keys)))
      (hop--jump-overlay-done)
      (if (= (length key-indices) 1)
          (let* ((key-index (car key-indices))
                 (match (nth key-index matches)))
            (hop--dim-overlay-done)
            (select-window (cdr match))
            (goto-char (hop-calculate-jump-char match)))
        (let* ((filtered-matches (cl-loop for index in key-indices collect (nth index matches)))
               (filtered-keys (cl-loop for index in key-indices collect (substring (nth index keys) 1 2))))
          (hop--jump-overlay filtered-matches filtered-keys)
          (let* ((key-index (car (hop-indices-with-prefix (string (read-char)) filtered-keys)))
                 (match (nth key-index filtered-matches)))
            (hop--jump-overlay-done)
            (hop--dim-overlay-done)
            (select-window (cdr match))
            (goto-char (hop-calculate-jump-char match))))))))


;; Main Logic (Updation and Movement) | API :: END


(provide 'hop)
;;; my-package.el ends here
