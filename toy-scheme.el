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

;; Local variables:
;; lisp-body-indent: 4
;; indent-tabs-mode: nil
;; END

(defgroup toy-scheme nil
    "Custom scheme configuration."
    :group 'toy-scheme)

(defcustom toy-scheme-file-name
    "schemes.txt"
    "Custom scheme file name."
    :type 'string
    :group 'toy-scheme)

;;;###autoload
(defun toy-scheme-locate (&optional dir)
    "Locates the scheme file going upwards the directory."
    (interactive)
    (unless dir (setq dir default-directory))
    (let ((root-dir (locate-dominating-file dir toy-scheme-file-name)))
        (when root-dir
            (concat root-dir toy-scheme-file-name))))

;;;###autoload
(defun toy-scheme-resolve-scheme (scheme &optional file)
    "Returns a corresponding absolute path to the given scheme."
    (unless file (setq file (toy-scheme-locate)))
    (interactive)
    ;; TODO: grep
    scheme)

;;;###autoload
(defun toy-scheme-resolve (path)
    "Figures out where the schemed path refers to."
    (interactive)
    (let ((root-file (toy-scheme-locate)))
        ;; (unless root-file (error "Unable to locate scheme file"))
        ;; parse `scheme:content'
        (let* ((parts (s-split ":" root-file))
               (scheme (car parts))
               (content (cdr parts)))
            (if (not content)
                    ;; Relative/absolute path only
                    scheme
                ;; Scheme + relative path
                (setq scheme (toy-scheme-resolve-scheme scheme))
                (setq content (s-trim-left content))
                ;; FIXME: don't create double slash
                (concat scheme "/" content)
                ))))

;;; toy-scheme.el ends here
