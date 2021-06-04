;;; toy-scheme.el --- Handle custom path schemes defined in `schemes.txt'

;; Author: toyboot4e <toyboot4e@gmail.com>
;; Version: 0.0.0
;; Homepage: https://github.com/toyboot4e/toy-scheme
;; Keywords: 
;; Package-Requires: ((emacs "26.1"))
;; License: MIT

;;; Commentary:

;; `toy-scheme.el' is a package for handling custom path schemes defined in `schemes.txt'.

;;; Code:

(defgroup toy-scheme nil
    "Custom scheme configuration."
    :group 'toy-scheme)

(defcustom toy-scheme-file-name
    "schemes.txt"
    "Custom scheme file name."
    :type 'string
    :group 'toy-scheme)

;;;###autoload
(define-minor-mode toy-scheme-mode
    "Minor mode to utilize schemed path (aliases)."
    :lighter " toy-scheme"
    ;; (setq ffap-file-finder #'toy-scheme--ffap-file-finder)
    (advice-add #'find-file-at-point :around #'toy-scheme-ffap-advice))

(define-global-minor-mode global-toy-scheme-mode toy-scheme-mode
    (lambda () (toy-scheme-mode 1)))

(defun toy-scheme--s2rel (scheme &optional scheme-file)
    "Converts scheme to relative path from a root directory."
    (when scheme-file (setq scheme-file (expand-file-name scheme-file)))
    (unless scheme-file (setq scheme-file (toy-scheme-locate)))

    (unless scheme-file (error "Unable to locate custom scheme file"))

    ;; grep "^$scheme" "$file" | grep ':' | head -n 1 | awk -F':' '{print $2}' | <trim>
    (when (file-exists-p scheme-file)
        ;; grep "^$scheme:"
        (let ((pattern (concat "^" scheme ":")))
            (with-temp-buffer
                (insert-file-contents scheme-file)
                (goto-char (point-min))
                (when (re-search-forward pattern nil 'no-error)
                    (let ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
                        ;; get content after `:`
                        (when line
                            (let* ((parts (s-split ":" line))
                                   (content (car (cdr parts))))
                                ;; then trim
                                (s-trim-left content)))))))))

(defun toy-scheme--s2abs (scheme &optional root-dir scheme-file)
    "Converts scheme to absolute path."
    (let ((rel (toy-scheme--s2rel scheme scheme-file)))
        (unless root-dir (setq root-dir (file-name-directory (toy-scheme-locate))))
        (concat root-dir rel)))

(defun toy-scheme--resolve-parsed (scheme content)
    (setq scheme (toy-scheme-resolve-scheme scheme))
    (concat (file-name-as-directory scheme) content))

(defun toy-scheme--find-parsed (scheme content)
    (let (path (toy-scheme--resolve-parsed scheme content))
        (when path
            (find-file path))))

;;;###autoload
(defun toy-scheme-locate (&optional dir)
    "Locates the scheme file going upwards the directory."
    (interactive)
    (unless dir (setq dir default-directory))
    (let* ((root-dir (locate-dominating-file dir toy-scheme-file-name)))
        (when root-dir
            (concat root-dir toy-scheme-file-name))))

;;;###autoload
(defun toy-scheme-resolve-scheme (scheme &optional scheme-file)
    "Returns a corresponding absolute path to the given scheme."
    (interactive)
    (toy-scheme--s2abs scheme scheme-file))

;;;###autoload
(defun toy-scheme-resolve (path)
    "Returns where the schemed path refers to."
    ;; parse `scheme:content'
    (let* ((parts (s-split ":" path))
           (scheme (car parts))
           (content (car (cdr parts))))
        (if (not content)
                ;; Relative/absolute path only
                scheme
            ;; Scheme + relative path
            (toy-scheme--resolve-parsed scheme content))))

;;;###autoload
(defun toy-scheme-find (&optional path)
    "Tries to resolve and open the schemed path."
    (interactive "sPath:")
    (if (not pass)
            (toy-scheme-ffap)
        (let ((resolved (toy-scheme-resolve path)))
            (when resolved
                (find-file resolved)))))

(defun toy-scheme-ffap (&optional filename)
    (interactive)
    (let* ((string (if filename filename (thing-at-point 'filename)))
           (resolved (toy-scheme-resolve string)))
        (if resolved
                (find-file resolved)
            (find-file-at-point filename))))

(defun toy-scheme-ffap-advice (original-func &optional filename)
    "Function for use in `add-advice :around'"
    (interactive)
    (if (not toy-scheme-mode)
            (funcall original-func filename)
        ;; almost same as `toy-scheme-ffap', but uses the `original-func` parameter
        (let* ((string (thing-at-point 'filename))
               (resolved (toy-scheme-resolve string)))
            (if resolved
                    (find-file resolved)
                (original-func filename)))))

;;; Local variables:      ***
;;; lisp-body-indent: 4   ***
;;; indent-tabs-mode: nil ***
;;; End:                  ***

(provide 'toy-scheme)

;;; toy-scheme.el ends here
