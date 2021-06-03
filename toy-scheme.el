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

;;; toy-scheme.el ends here
