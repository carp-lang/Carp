(defvar carp-cli-file-path "carp-inferior")
(defvar carp-cli-arguments '())

(defun run-carp ()
  "Run an inferior instance of `carp-inferior' inside Emacs."
  (interactive)
  (let* ((carp-program carp-cli-file-path)
         (buffer (comint-check-proc "Carp")))
    ;; Pop to the "*Carp*" buffer if the process is dead, the buffer is missing or it's got the wrong mode.
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'carp-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer "*Carp*"))
       (current-buffer)))
    ;; Create the comint process if there is no buffer.
    (unless buffer
      (apply 'make-comint-in-buffer "Carp" buffer
             carp-program carp-cli-arguments)
      (carp-mode))))

(defun carp-test-1 ()
  (interactive)
  (comint-send-string "*Carp*" "(println (str 12345))\n"))

(defun carp-eval-buffer ()
  (interactive)
  (comint-send-string "*Carp*" (buffer-string)))

(defvar carp-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    (define-key map (kbd "C-c C-l") 'carp-eval-buffer)
    map)
  "Basic mode map for `carp-mode'")

(define-derived-mode carp-mode comint-mode "Carp"
  "Major mode for the Carp programming language.")
