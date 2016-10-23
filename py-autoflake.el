;;; py-docformatter.el --- Use docformatter to beautify a Python buffer

;; Copyright (C) 2016, Manuel Kaufmann <humitos@gmail.com>

;; Author: Manuel Kaufmann <humitos@gmail.com>
;; URL: 
;; Version: 0.1

;;; Commentary:

;; Provides the `py-docformatter' command, which uses the external
;; "docformatter" tool to tidy up the current buffer according to
;; Python's PEP257.

;; To automatically apply when saving a python file, use the
;; following code:

;;   (add-hook 'python-mode-hook 'py-docformatter-enable-on-save)

;; To customize the behaviour of "docformatter" you can set the
;; py-docformatter-options e.g.

;;   (setq py-docformatter-options '("--wrap-summaries=100"))

;; This file is 99.9% based on py-autopep8.el
;; (https://github.com/paetzke/py-autopep8.el)

;;; Code:

(defgroup py-docformatter nil
  "Use docformatter to beautify a Python buffer."
  :group 'convenience
  :prefix "py-docformatter-")


(defcustom py-docformatter-options nil
  "Options used for docformatter.

Note that `--in-place' is used by default."
  :group 'py-docformatter
  :type '(repeat (string :tag "option")))


(defun py-docformatter--call-executable (errbuf file)
  (zerop (apply 'call-process "docformatter" nil errbuf nil
                (append py-docformatter-options `("--in-place", file)))))


;;;###autoload
(defun py-docformatter-buffer ()
  "Uses the \"docformatter\" tool to reformat the current buffer."
  (interactive)
  (py-docformatter-bf--apply-executable-to-buffer "docformatter"
						  'py-docformatter--call-executable
						  nil
						  "py"))


;;;###autoload
(defun py-docformatter-enable-on-save ()
  "Pre-save hook to be used before running docformatter."
  (interactive)
  (add-hook 'before-save-hook 'py-docformatter-buffer nil t))


;; BEGIN GENERATED -----------------
;; !!! This file is generated !!!
;; buftra.el
;; Copyright (C) 2015, Friedrich Paetzke <paetzke@fastmail.fm>
;; Author: Friedrich Paetzke <paetzke@fastmail.fm>
;; URL: https://github.com/paetzke/buftra.el
;; Version: 0.5

;; This code is initially copied from go-mode.el (copyright the go-mode authors).
;; See LICENSE or https://raw.githubusercontent.com/dominikh/go-mode.el/master/LICENSE


(defun py-docformatter-bf--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "invalid rcs patch or internal error in py-docformatter-bf--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (setq line-offset (- line-offset len))
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (goto-char (point-min))
                (forward-line (- from line-offset 1))
                (setq line-offset (+ line-offset len))
                (kill-whole-line len)
                (pop kill-ring)))
             (t
              (error "invalid rcs patch or internal error in py-docformatter-bf--apply-rcs-patch")))))))))


(defun py-docformatter-bf--replace-region (filename)
  (delete-region (region-beginning) (region-end))
  (insert-file-contents filename))


(defun py-docformatter-bf--apply-executable-to-buffer (executable-name
						       executable-call
						       only-on-region
						       file-extension)
  "Formats the current buffer according to the executable"
  (when (not (executable-find executable-name))
    (error (format "%s command not found." executable-name)))
  (let ((tmpfile (make-temp-file executable-name nil (concat "." file-extension)))
        (patchbuf (get-buffer-create (format "*%s patch*" executable-name)))
        (errbuf (get-buffer-create (format "*%s Errors*" executable-name)))
        (coding-system-for-read buffer-file-coding-system)
        (coding-system-for-write buffer-file-coding-system))
    (with-current-buffer errbuf
      (setq buffer-read-only nil)
      (erase-buffer))
    (with-current-buffer patchbuf
      (erase-buffer))

    (if (and only-on-region (use-region-p))
        (write-region (region-beginning) (region-end) tmpfile)
      (write-region nil nil tmpfile))

    (if (funcall executable-call errbuf tmpfile)
        (if (zerop (call-process-region (point-min) (point-max) "diff" nil
                                        patchbuf nil "-n" "-" tmpfile))
            (progn
              (kill-buffer errbuf)
              (pop kill-ring)
              (message (format "Buffer is already %sed" executable-name)))

          (if only-on-region
              (py-docformatter-bf--replace-region tmpfile)
            (py-docformatter-bf--apply-rcs-patch patchbuf))

          (kill-buffer errbuf)
          (pop kill-ring)
          (message (format "Applied %s" executable-name)))
      (error (format "Could not apply %s. Check *%s Errors* for details"
                     executable-name executable-name)))
    (kill-buffer patchbuf)
    (pop kill-ring)
    (delete-file tmpfile)))


;; py-docformatter-bf.el ends here
;; END GENERATED -------------------


(provide 'py-docformatter)


;;; py-docformatter.el ends here
