;;; fluff.el --- Emacs integration for fluff Fortran linter

;;; Commentary:
;; This package provides Emacs integration for the fluff Fortran linter.

;;; Code:

(defgroup fluff nil
  "Interface to fluff Fortran linter."
  :group 'programming
  :prefix "fluff-")

(defcustom fluff-executable "fluff"
  "Path to fluff executable."
  :type 'string
  :group 'fluff)

(defcustom fluff-check-on-save t
  "Run fluff check when saving Fortran files."
  :type 'boolean
  :group 'fluff)

(defun fluff-check-buffer ()
  "Run fluff check on current buffer."
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
        (compilation-start
         (concat fluff-executable " check " (shell-quote-argument file))
         'compilation-mode "fluff")
      (message "Buffer not associated with a file"))))

(defun fluff-check-project ()
  "Run fluff check on entire project."
  (interactive)
  (compilation-start
   (concat fluff-executable " check .")
   'compilation-mode "fluff"))

;; Auto-run fluff on save if enabled
(defun fluff-maybe-check-on-save ()
  "Run fluff check on save if enabled."
  (when fluff-check-on-save
    (fluff-check-buffer)))

(add-hook 'fortran-mode-hook
          (lambda ()
            (add-hook 'after-save-hook 'fluff-maybe-check-on-save nil t)))

(provide 'fluff)
;;; fluff.el ends here