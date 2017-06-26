(require 'comint)
(require 'thingatpt)

;; This code is blatantly stolen from clojure-inf by Bozhidar Batsov

(defgroup inf-carp nil
  "Run an external Carp process (REPL) in an Emacs buffer."
  :group 'carp)

(defcustom inf-carp-prompt-read-only t
  "If non-nil, the prompt will be read-only.

Also see the description of `ielm-prompt-read-only'."
  :type 'boolean
  :group 'inf-carp)

(defcustom inf-carp-filter-regexp
  "\\`\\s *\\(:\\(\\w\\|\\s_\\)\\)?\\s *\\'"
  "What not to save on inferior Carp's input history.
Input matching this regexp is not saved on the input history in Inferior Carp
mode.  Default is whitespace followed by 0 or 1 single-letter colon-keyword
\(as in :a, :c, etc.)"
  :type 'regexp
  :group 'inf-carp)

(defvar inf-carp-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    (define-key map "\C-x\C-e" #'inf-carp-eval-last-sexp)
    (define-key map "\C-c\C-l" #'inf-carp-load-file)
    (define-key map "\C-c\C-a" #'inf-carp-show-arglist)
    (define-key map "\C-c\C-v" #'inf-carp-show-var-documentation)
    (define-key map "\C-c\C-s" #'inf-carp-show-var-source)
    (define-key map "\C-c\M-o" #'inf-carp-clear-repl-buffer)
    (easy-menu-define inf-carp-mode-menu map
      "Inferior Carp REPL Menu"
      '("Inf-Carp REPL"
        ["Eval last sexp" inf-carp-eval-last-sexp t]
        "--"
        ["Load file" inf-carp-load-file t]
        "--"
        ["Show arglist" inf-carp-show-arglist t]
        ["Show documentation for var" inf-carp-show-var-documentation t]
        ["Show source for var" inf-carp-show-var-source t]
        "--"
        ["Clear REPL" inf-carp-clear-repl-buffer]))
    map))

(defvar inf-carp-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-\C-x"  #'inf-carp-eval-defun)     ; Gnu convention
    (define-key map "\C-x\C-e" #'inf-carp-eval-last-sexp) ; Gnu convention
    (define-key map "\C-c\C-e" #'inf-carp-eval-last-sexp)
    (define-key map "\C-c\C-c" #'inf-carp-bake)
    (define-key map (kbd "<s-return>") #'inf-carp-eval-defun)     ; Light Table style
    (define-key map "\C-c\C-b" #'inf-carp-eval-buffer)
    (define-key map "\C-c\C-r" #'inf-carp-eval-region)
    (define-key map "\C-c\C-n" #'inf-carp-eval-form-and-next)
    (define-key map "\C-c\C-z" #'inf-carp-switch-to-repl)
    (define-key map "\C-c\C-i" #'inf-carp-show-ns-vars)
    (define-key map "\C-c\C-A" #'inf-carp-apropos)
    (define-key map "\C-c\C-m" #'inf-carp-macroexpand)
    (define-key map "\C-c\C-l" #'inf-carp-load-file)
    (define-key map "\C-c\C-a" #'inf-carp-show-arglist)
    (define-key map "\C-c\C-v" #'inf-carp-show-var-documentation)
    (define-key map "\C-c\C-s" #'inf-carp-show-var-source)
    (define-key map "\C-c\M-n" #'inf-carp-set-ns)
    (easy-menu-define inf-carp-minor-mode-menu map
      "Inferior Carp Minor Mode Menu"
      '("Inf-Carp"
        ["Eval top-level sexp at point" inf-carp-eval-defun t]
        ["Eval last sexp" inf-carp-eval-last-sexp t]
        ["Eval region" inf-carp-eval-region t]
        ["Eval buffer" inf-carp-eval-buffer t]
        "--"
        ["Load file..." inf-carp-load-file t]
        "--"
        ["Switch to REPL" inf-carp-switch-to-repl t]
        ["Set REPL ns" inf-carp-set-ns t]
        "--"
        ["Show arglist" inf-carp-show-arglist t]
        ["Show documentation for var" inf-carp-show-var-documentation t]
        ["Show source for var" inf-carp-show-var-source t]
        ["Show vars in ns" inf-carp-show-ns-varst]
        ["Apropos" inf-carp-apropos t]
        ["Macroexpand" inf-carp-macroexpand t]))
    map))

;;;###autoload
(define-minor-mode inf-carp-minor-mode
  "Minor mode for interacting with the inferior Carp process buffer.

The following commands are available:

\\{inf-carp-minor-mode-map}"
  :lighter "" :keymap inf-carp-minor-mode-map)

(defcustom inf-carp-program "CarpHask-exe"
  "Program name for invoking an inferior Carp in Inferior Carp mode."
  :type 'string
  :group 'inf-carp)

(defcustom inf-carp-load-command "(load-lisp \"%s\")\n"
  "Format-string for building a Carp expression to load a file.
This format string should use `%s' to substitute a file name
and should result in a Carp expression that will command the inferior Carp
to load that file."
  :type 'string
  :group 'inf-carp)

(defcustom inf-carp-prompt "^[^鲮 \n]   *" ;; "^[^λ> \n]+λ> *"
  "Regexp to recognize prompts in the Inferior Carp mode."
  :type 'regexp
  :group 'inf-carp)

(defcustom inf-carp-subprompt " *_> *"
  "Regexp to recognize subprompts in the Inferior Carp mode."
  :type 'regexp
  :group 'inf-carp)

(defvar inf-carp-buffer nil
  "The current inf-carp process buffer.

MULTIPLE PROCESS SUPPORT
===========================================================================
To run multiple Carp processes, you start the first up
with \\[inf-carp].  It will be in a buffer named `*inf-carp*'.
Rename this buffer with \\[rename-buffer].  You may now start up a new
process with another \\[inf-carp].  It will be in a new buffer,
named `*inf-carp*'.  You can switch between the different process
buffers with \\[switch-to-buffer].

Commands that send text from source buffers to Carp processes --
like `inf-carp-eval-defun' or `inf-carp-show-arglist' -- have to choose a
process to send to, when you have more than one Carp process around.  This
is determined by the global variable `inf-carp-buffer'.  Suppose you
have three inferior Carps running:
    Buffer              Process
    foo                 inf-carp
    bar                 inf-carp<2>
    *inf-carp*     inf-carp<3>
If you do a \\[inf-carp-eval-defun] command on some Carp source code,
what process do you send it to?

- If you're in a process buffer (foo, bar, or *inf-carp*),
  you send it to that process.
- If you're in some other buffer (e.g., a source file), you
  send it to the process attached to buffer `inf-carp-buffer'.
This process selection is performed by function `inf-carp-proc'.

Whenever \\[inf-carp] fires up a new process, it resets
`inf-carp-buffer' to be the new process's buffer.  If you only run
one process, this does the right thing.  If you run multiple
processes, you might need to change `inf-carp-buffer' to
whichever process buffer you want to use.")

(defvar inf-carp-mode-hook '()
  "Hook for customizing Inferior Carp mode.")

(put 'inf-carp-mode 'mode-class 'special)

(define-derived-mode inf-carp-mode comint-mode "Inferior Carp"
  "Major mode for interacting with an inferior Carp process.
Runs a Carp interpreter as a subprocess of Emacs, with Carp I/O through an
Emacs buffer.  Variable `inf-carp-program' controls which Carp interpreter
is run.  Variables `inf-carp-prompt', `inf-carp-filter-regexp' and
`inf-carp-load-command' can customize this mode for different Carp
interpreters.

For information on running multiple processes in multiple buffers, see
documentation for variable `inf-carp-buffer'.

\\{inf-carp-mode-map}

Customization: Entry to this mode runs the hooks on `comint-mode-hook' and
`inf-carp-mode-hook' (in that order).

You can send text to the inferior Carp process from other buffers containing
Carp source.
    `inf-carp-switch-to-repl' switches the current buffer to the Carp process buffer.
    `inf-carp-eval-defun' sends the current defun to the Carp process.
    `inf-carp-eval-region' sends the current region to the Carp process.

    Prefixing the inf-carp-eval/defun/region commands with
    a \\[universal-argument] causes a switch to the Carp process buffer after sending
    the text.

Commands:\\<inf-carp-mode-map>
\\[comint-send-input] after the end of the process' output sends the text from the
    end of process to point.
\\[comint-send-input] before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
\\[comint-copy-old-input] copies the sexp ending at point to the end of the process' output,
    allowing you to edit it before sending it.
If `comint-use-prompt-regexp' is nil (the default), \\[comint-insert-input] on old input
   copies the entire old input to the end of the process' output, allowing
   you to edit it before sending it.  When not used on old input, or if
   `comint-use-prompt-regexp' is non-nil, \\[comint-insert-input] behaves according to
   its global binding.
\\[backward-delete-char-untabify] converts tabs to spaces as it moves back.
\\[carp-indent-line] indents for Carp; with argument, shifts rest
    of expression rigidly with the current line.
\\[indent-sexp] does \\[carp-indent-line] on each line starting within following expression.
Paragraphs are separated only by blank lines.  Semicolons start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it."
  (setq comint-prompt-regexp inf-carp-prompt)
  (setq mode-line-process '(":%s"))
  ;;(carp-mode-variables) ;; ???
  ;;(inf-carp-eldoc-setup)
  (setq comint-get-old-input #'inf-carp-get-old-input)
  (setq comint-input-filter #'inf-carp-input-filter)
  (set (make-local-variable 'comint-prompt-read-only) inf-carp-prompt-read-only)
  (add-hook 'comint-preoutput-filter-functions #'inf-carp-preoutput-filter nil t)
  (add-hook 'completion-at-point-functions #'inf-carp-completion-at-point nil t))

(add-hook 'carp-mode-hook (lambda () (inf-carp-minor-mode 1)))

(defun inf-carp-get-old-input ()
  "Return a string containing the sexp ending at point."
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (point) end))))

(defun inf-carp-input-filter (str)
  "Return t if STR does not match `inf-carp-filter-regexp'."
  (not (string-match inf-carp-filter-regexp str)))

(defun inf-carp-chomp (string)
  "Remove final newline from STRING."
  (if (string-match "[\n]\\'" string)
      (replace-match "" t t string)
    string))

(defun inf-carp-remove-subprompts (string)
  "Remove subprompts from STRING."
  (replace-regexp-in-string inf-carp-subprompt "" string))

(defun inf-carp-preoutput-filter (str)
  "Preprocess the output STR from interactive commands."
  (cond
   ((string-prefix-p "inf-carp-" (symbol-name (or this-command last-command)))
    ;; Remove subprompts and prepend a newline to the output string
    (inf-carp-chomp (concat "\n" (inf-carp-remove-subprompts str))))
   (t str)))

(defvar inf-carp-project-root-files
  '("project.clj" "build.boot" "project.carp")
  "A list of files that can be considered project markers.")

(defun inf-carp-project-root ()
  "Retrieve the root directory of a project if available.

Fallback to `default-directory.' if not within a project."
  (or (car (remove nil
                   (mapcar (lambda
                             (file)
                             (locate-dominating-file default-directory file))
                           inf-carp-project-root-files)))
      default-directory))

(defun inf-carp-clear-repl-buffer ()
  "Clear the REPL buffer."
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

;;;###autoload
(defun inf-carp (cmd)
  "Run an inferior Carp process, input and output via buffer `*inf-carp*'.
If there is a process already running in `*inf-carp*', just switch
to that buffer.
With argument, allows you to edit the command line (default is value
of `inf-carp-program').  Runs the hooks from
`inf-carp-mode-hook' (after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (interactive (list (if current-prefix-arg
                         (read-string "Run Carp: " inf-carp-program)
                       inf-carp-program)))
  (if (not (comint-check-proc "*inf-carp*"))
      ;; run the new process in the project's root when in a project folder
      (let ((default-directory (inf-carp-project-root))
            (cmdlist (split-string cmd)))
        (set-buffer (apply #'make-comint
                           "inf-carp" (car cmdlist) nil (cdr cmdlist)))
        (inf-carp-mode)))
  (setq inf-carp-buffer "*inf-carp*")
  ;;(split-window-below)
  ;;(pop-to-buffer-same-window "*inf-carp*")
  (pop-to-buffer "*inf-carp*")
  )

;;;###autoload
(defalias 'run-carp 'inf-carp)

(defun inf-carp-eval-region (start end &optional and-go)
  "Send the current region to the inferior Carp process.
Prefix argument AND-GO means switch to the Carp buffer afterwards."
  (interactive "r\nP")
  ;; replace multiple newlines at the end of the region by a single one
  ;; or add one if there was no newline
  (let ((str (replace-regexp-in-string
              "[\n]*\\'" "\n"
              (buffer-substring-no-properties start end))))
    (comint-send-string (inf-carp-proc) str))
  (if and-go (inf-carp-switch-to-repl t)))

(defun inf-carp-eval-string (code)
  "Send the string CODE to the inferior Carp process to be executed."
  (comint-send-string (inf-carp-proc) (concat code "\n")))

(defun inf-carp-eval-defun (&optional and-go)
  "Send the current defun to the inferior Carp process.
Prefix argument AND-GO means switch to the Carp buffer afterwards."
  (interactive "P")
  (save-excursion
    (end-of-defun)
    (let ((end (point)) (case-fold-search t))
      (beginning-of-defun)
      (inf-carp-eval-region (point) end and-go))))

(defun inf-carp-eval-buffer (&optional and-go)
  "Send the current buffer to the inferior Carp process.
Prefix argument AND-GO means switch to the Carp buffer afterwards."
  (interactive "P")
  (save-excursion
    (widen)
    (let ((case-fold-search t))
      (inf-carp-eval-region (point-min) (point-max) and-go))))

(defun inf-carp-eval-last-sexp (&optional and-go)
  "Send the previous sexp to the inferior Carp process.
Prefix argument AND-GO means switch to the Carp buffer afterwards."
  (interactive "P")
  (inf-carp-eval-region (save-excursion (backward-sexp) (point)) (point) and-go))

(defun inf-carp-eval-form-and-next ()
  "Send the previous sexp to the inferior Carp process and move to the next one."
  (interactive "")
  (while (not (zerop (car (syntax-ppss))))
    (up-list))
  (inf-carp-eval-last-sexp)
  (forward-sexp))

(defun inf-carp-switch-to-repl (eob-p)
  "Switch to the inferior Carp process buffer.
With prefix argument EOB-P, positions cursor at end of buffer."
  (interactive "P")
  (if (get-buffer-process inf-carp-buffer)
      (let ((pop-up-frames
             ;; Be willing to use another frame
             ;; that already has the window in it.
             (or pop-up-frames
                 (get-buffer-window inf-carp-buffer t))))
        (pop-to-buffer inf-carp-buffer))
    (run-carp inf-carp-program))
  (when eob-p
    (push-mark)
    (goto-char (point-max))))


;;; Now that inf-carp-eval-/defun/region takes an optional prefix arg,
;;; these commands are redundant. But they are kept around for the user
;;; to bind if he wishes, for backwards functionality, and because it's
;;; easier to type C-c e than C-u C-c C-e.

(defun inf-carp-eval-region-and-go (start end)
  "Send the current region to the inferior Carp, and switch to its buffer."
  (interactive "r")
  (inf-carp-eval-region start end t))

(defun inf-carp-eval-defun-and-go ()
  "Send the current defun to the inferior Carp, and switch to its buffer."
  (interactive)
  (inf-carp-eval-defun t))

(defvar inf-carp-prev-l/c-dir/file nil
  "Record last directory and file used in loading or compiling.
This holds a cons cell of the form `(DIRECTORY . FILE)'
describing the last `inf-carp-load-file' command.")

(defcustom inf-carp-source-modes '(carp-mode)
  "Used to determine if a buffer contains Carp source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a Carp source file by `inf-carp-load-file'.
Used by this command to determine defaults."
  :type '(repeat symbol)
  :group 'inf-carp)

(defun inf-carp-load-file (file-name)
  "Load a Carp file FILE-NAME into the inferior Carp process."
  (interactive (comint-get-source "Load Carp file: " inf-carp-prev-l/c-dir/file
                                  inf-carp-source-modes nil)) ; nil because LOAD
                                        ; doesn't need an exact name
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq inf-carp-prev-l/c-dir/file (cons (file-name-directory    file-name)
                                            (file-name-nondirectory file-name)))
  (comint-send-string (inf-carp-proc)
                      (format inf-carp-load-command file-name))
  (inf-carp-switch-to-repl t))

(defun inf-carp-connected-p ()
  "Return t if inferior Carp is currently connected, nil otherwise."
  (not (null inf-carp-buffer)))


;;; Documentation functions: function doc, var doc, arglist, and
;;; describe symbol.
;;; ===========================================================================

;;; Command strings
;;; ===============

(defcustom inf-carp-var-bake-command
  "(bake %s)\n"
  "Command to bake a form."
  :type 'string
  :group 'inf-carp)

(defcustom inf-carp-var-doc-command
  "(carp.repl/doc %s)\n"
  "Command to query inferior Carp for a var's documentation."
  :type 'string
  :group 'inf-carp)

(defcustom inf-carp-var-source-command
  "(carp.repl/source %s)\n"
  "Command to query inferior Carp for a var's source."
  :type 'string
  :group 'inf-carp)

(defcustom inf-carp-arglist-command
  "(try
     (:arglists
      (carp.core/meta
       (carp.core/resolve
        (carp.core/read-string \"%s\"))))
     (catch Throwable t nil))\n"
  "Command to query inferior Carp for a function's arglist."
  :type 'string
  :group 'inf-carp)

(defcustom inf-carp-completion-command
  "(complete.core/completions \"%s\")\n"
  "Command to query inferior Carp for completion candidates."
  :type 'string
  :group 'inf-carp)

(defcustom inf-carp-ns-vars-command
  "(carp.repl/dir %s)\n"
  "Command to show the public vars in a namespace."
  :type 'string
  :group 'inf-carp)

(defcustom inf-carp-set-ns-command
  "(carp.core/in-ns '%s)\n"
  "Command to set the namespace of the inferior Carp process."
  :type 'string
  :group 'inf-carp)

(defcustom inf-carp-apropos-command
  "(doseq [var (sort (carp.repl/apropos \"%s\"))]
     (println (str var)))\n"
  "Command to invoke apropos."
  :type 'string
  :group 'inf-carp)

(defcustom inf-carp-macroexpand-command
  "(carp.core/macroexpand '%s)\n"
  "Command to invoke macroexpand."
  :type 'string
  :group 'inf-carp)

(defcustom inf-carp-macroexpand-1-command
  "(carp.core/macroexpand-1 '%s)\n"
  "Command to invoke macroexpand-1."
  :type 'string
  :group 'inf-carp)

;;; Ancillary functions
;;; ===================

;;; Reads a string from the user.
(defun inf-carp-symprompt (prompt default)
  (list (let* ((prompt (if default
                           (format "%s (default %s): " prompt default)
                         (concat prompt ": ")))
               (ans (read-string prompt)))
          (if (zerop (length ans)) default ans))))


;;; Adapted from function-called-at-point in help.el.
(defun inf-carp-fn-called-at-pt ()
  "Return the name of the function called in the current call.
The value is nil if it can't find one."
  (condition-case nil
      (save-excursion
        (save-restriction
          (narrow-to-region (max (point-min) (- (point) 1000)) (point-max))
          (backward-up-list 1)
          (forward-char 1)
          (let ((obj (read (current-buffer))))
            (and (symbolp obj) obj))))
    (error nil)))


;;; Adapted from variable-at-point in help.el.
(defun inf-carp-var-at-pt ()
  (condition-case ()
      (save-excursion
        (forward-sexp -1)
        (skip-chars-forward "'")
        (let ((obj (read (current-buffer))))
          (and (symbolp obj) obj)))
    (error nil)))

(defun inf-carp-symbol-at-point ()
  "Return the name of the symbol at point, otherwise nil."
  (or (thing-at-point 'symbol) ""))

(defun inf-carp-bake (var)
  "Send a command to the inferior Carp to bake a form.
See variable `inf-carp-var-bake-command'."
  (interactive (inf-carp-symprompt "Var bake" (inf-carp-var-at-pt)))
  (comint-proc-query (inf-carp-proc) (format inf-carp-var-bake-command var)))

;;; Documentation functions: var doc and arglist.
;;; ======================================================================

(defun inf-carp-show-var-documentation (var)
  "Send a command to the inferior Carp to give documentation for VAR.
See variable `inf-carp-var-doc-command'."
  (interactive (inf-carp-symprompt "Var doc" (inf-carp-var-at-pt)))
  (comint-proc-query (inf-carp-proc) (format inf-carp-var-doc-command var)))

(defun inf-carp-show-var-source (var)
  "Send a command to the inferior Carp to give source for VAR.
See variable `inf-carp-var-source-command'."
  (interactive (inf-carp-symprompt "Var source" (inf-carp-var-at-pt)))
  (comint-proc-query (inf-carp-proc) (format inf-carp-var-source-command var)))

(defun inf-carp-arglist (fn)
  "Send a query to the inferior Carp for the arglist for function FN.
See variable `inf-carp-arglist-command'."
  (interactive (inf-carp-symprompt "Arglist" (inf-carp-fn-called-at-pt)))
  (let* ((proc (inf-carp-proc))
         (comint-filt (process-filter proc))
         (kept "")
         eldoc)
    (set-process-filter proc (lambda (_proc string) (setq kept (concat kept string))))
    (unwind-protect
        (let ((eldoc-snippet (format inf-carp-arglist-command fn)))
          (process-send-string proc eldoc-snippet)
          (while (and (not (string-match inf-carp-prompt kept))
                      (accept-process-output proc 2)))
          (setq eldoc (and (string-match "(.+)" kept) (match-string 0 kept)))
          )
      (set-process-filter proc comint-filt))
    eldoc))

(defun inf-carp-show-arglist (fn)
  "Show the arglist for function FN in the mini-buffer."
  (interactive (inf-carp-symprompt "Arglist" (inf-carp-fn-called-at-pt)))
  (let ((eldoc (inf-carp-arglist fn)))
    (when eldoc
      (message "%s: %s" fn eldoc))))

(defun inf-carp-show-ns-vars (ns)
  "Send a query to the inferior Carp for the public vars in NS.
See variable `inf-carp-ns-vars-command'."
  (interactive (inf-carp-symprompt "Ns vars" (carp-find-ns)))
  (comint-proc-query (inf-carp-proc) (format inf-carp-ns-vars-command ns)))

(defun inf-carp-set-ns (ns)
  "Set the ns of the inferior Carp process to NS.
Defaults to the ns of the current buffer."
  (interactive (inf-carp-symprompt "Set ns to" (carp-find-ns)))
  (comint-proc-query (inf-carp-proc) (format inf-carp-set-ns-command ns)))

(defun inf-carp-apropos (var)
  "Send a command to the inferior Carp to give apropos for VAR.
See variable `inf-carp-apropos-command'."
  (interactive (inf-carp-symprompt "Var apropos" (inf-carp-var-at-pt)))
  (comint-proc-query (inf-carp-proc) (format inf-carp-apropos-command var)))

(defun inf-carp-macroexpand (&optional macro-1)
  "Send a command to the inferior Carp to give apropos for VAR.
See variable `inf-carp-macroexpand-command'.
With a prefix arg MACRO-1 uses `inf-carp-macroexpand-1-command'."
  (interactive "P")
  (let ((last-sexp (buffer-substring-no-properties (save-excursion (backward-sexp) (point)) (point))))
    (comint-send-string
     (inf-carp-proc)
     (format (if macro-1
                 inf-carp-macroexpand-1-command
               inf-carp-macroexpand-command)
             last-sexp))))


(defun inf-carp-proc ()
  "Return the current inferior Carp process.
See variable `inf-carp-buffer'."
  (let ((proc (get-buffer-process (if (derived-mode-p 'inf-carp-mode)
                                      (current-buffer)
                                    inf-carp-buffer))))
    (or proc
        (error "No Carp subprocess; see variable `inf-carp-buffer'"))))

(defun inf-carp-completions (expr)
  "Return a list of completions for the Carp expression starting with EXPR."
  (let* ((proc (inf-carp-proc))
         (comint-filt (process-filter proc))
         (kept "")
         completions)
    (set-process-filter proc (lambda (_proc string) (setq kept (concat kept string))))
    (unwind-protect
        (let ((completion-snippet
               (format
                inf-carp-completion-command (substring-no-properties expr))))
          (process-send-string proc completion-snippet)
          (while (and (not (string-match inf-carp-prompt kept))
                      (accept-process-output proc 2)))
          (setq completions (read kept))
          ;; Subprocess echoes output on Windows and OS X.
          (when (and completions (string= (concat (car completions) "\n") completion-snippet))
            (setq completions (cdr completions))))
      (set-process-filter proc comint-filt))
    completions))

(defconst inf-carp-carp-expr-break-chars " \t\n\"\'`><,;|&{(")

(defun inf-carp-completion-bounds-of-expr-at-point ()
  "Return bounds of expression at point to complete."
  (when (not (memq (char-syntax (following-char)) '(?w ?_)))
    (save-excursion
      (let ((end (point)))
        (skip-chars-backward (concat "^" inf-carp-carp-expr-break-chars))
        (cons (point) end)))))

(defun inf-carp-completion-expr-at-point ()
  "Return expression at point to complete."
  (let ((bounds (inf-carp-completion-bounds-of-expr-at-point)))
    (and bounds
         (buffer-substring (car bounds) (cdr bounds)))))

(defun inf-carp-completion-at-point ()
  "Retrieve the list of completions and prompt the user.
Returns the selected completion or nil."
  (let ((bounds (inf-carp-completion-bounds-of-expr-at-point)))
    (when bounds
      (list (car bounds) (cdr bounds)
            (if (fboundp 'completion-table-with-cache)
                (completion-table-with-cache #'inf-carp-completions)
              (completion-table-dynamic #'inf-carp-completions))))))

(provide 'inf-carp-mode)
