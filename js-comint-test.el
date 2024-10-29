;; -*- lexical-binding: t -*-

(require 'js-comint)
(require 'ert)
(require 'el-mock)
(require 'company) ;; for should-complete

(defun js-comint-test-output-matches (input regex)
  "Verify that sending INPUT yields output that matches REGEX."
  (with-new-js-comint-buffer
    (js-comint-send-string input)
    (sit-for 1)
    (let ((output (buffer-substring-no-properties
                   comint-last-input-end
                   (car comint-last-prompt))))
      (should (string-match-p regex output)))))

(defmacro with-new-js-comint-buffer (&rest body)
  "Run BODY with a fresh js-comint as current buffer and exit after."
  (declare (indent 0) (debug t))
  `(progn
     (when (js-comint-get-process)
       (kill-process (js-comint-get-process)))
     (sleep-for 0.2)
     (kill-matching-buffers (js-comint-get-buffer-name) nil t)
     (run-js)
     (unwind-protect
         (with-current-buffer (js-comint-get-buffer)
           (font-lock-mode -1)
           (sit-for 1) ;; prevent race condition on start
           ,@body)
       (when (js-comint-get-process)
         (kill-process (js-comint-get-process)))
       (sleep-for 0.2)
       (kill-matching-buffers (js-comint-get-buffer-name) nil t))))

(ert-deftest js-comint-test-multiline-dotchain-line-start ()
  "Test multiline statement with dots at beginning of lines."
  (js-comint-test-output-matches
   "[1, 2, 3]
  .map((it) => it + 1)
  .filter((it) => it > 0)
  .reduce((prev, curr) => prev + curr, 0);"
   ;; output
   "^9$"))

(ert-deftest js-comint-test-multiline-dotchain-line-start-dos ()
  "Test multiline statement with dots at beginning of lines, with
DOS line separators."
  (js-comint-test-output-matches
   "[1, 2, 3]\r
  .map((it) => it + 1)\r
  .filter((it) => it > 0)\r
  .reduce((prev, curr) => prev + curr, 0);\r
"
   ;; output
   "^9$"))

(ert-deftest js-comint-test-multiline-dotchain-line-end ()
  "Test multiline statement with dots at end of lines."
  (js-comint-test-output-matches
   "[1, 2, 3].
map((it) => it + 1).
filter((it) => it > 0).
reduce((prev, curr) => prev + curr, 0);"
   ;; output
   "^9$"))

(ert-deftest js-comint-start-or-switch-to-repl/test-no-modules ()
  "Should preserve node_path when nothing is set."
  (let ((original js-comint-module-paths)
        (original-set-env js-comint-set-env-when-startup)
        (original-env (getenv "NODE_PATH")))
    (unwind-protect
        (progn
          (setq js-comint-module-paths nil
                js-comint-set-env-when-startup nil)
          (setenv "NODE_PATH" "/foo/bar")
          (js-comint-test-output-matches "process.env['NODE_PATH'];"
                                         "/foo/bar"))
      (setq js-comint-module-paths original
            js-comint-set-env-when-startup original-set-env)
      (setenv "NODE_PATH" original-env))))

(ert-deftest js-comint-start-or-switch-to-repl/test-global-set ()
  "Should include the value of `js-comint-node-modules' if set."
  (let ((original js-comint-module-paths)
        (original-set-env js-comint-set-env-when-startup)
        (original-env (getenv "NODE_PATH")))
    (unwind-protect
        (progn
          (setq js-comint-module-paths '("/baz/xyz")
                js-comint-set-env-when-startup nil)
          (setenv "NODE_PATH" "/foo/bar")
          (js-comint-test-output-matches "process.env['NODE_PATH'];"
                                         (concat "/foo/bar" (js-comint--path-sep) "/baz/xyz")))
      (setq js-comint-module-paths original
            js-comint-set-env-when-startup original-set-env)
      (setenv "NODE_PATH" original-env))))

(ert-deftest js-comint-start-or-switch-to-repl/test-local ()
  "Should include the optional node-modules-path."
  (let ((original js-comint-module-paths)
        (original-set-env js-comint-set-env-when-startup)
        (original-env (getenv "NODE_PATH"))
        (original-suggest (symbol-function 'js-comint--suggest-module-path)))
    (unwind-protect
        (progn
          (fset 'js-comint--suggest-module-path (lambda () "/baz/xyz"))
          (setq js-comint-module-paths '()
                js-comint-set-env-when-startup 't)
          (setenv "NODE_PATH" "/foo/bar")
          (js-comint-test-output-matches "process.env['NODE_PATH'];"
                                         (concat "/foo/bar" (js-comint--path-sep) "/baz/xyz")))
      (setq js-comint-module-paths original
            js-comint-set-env-when-startup original-set-env)
      (setenv "NODE_PATH" original-env)
      (fset 'js-comint--suggest-module-path original-suggest))))

(ert-deftest js-comint/test-strict-mode ()
  "When NODE_REPL_MODE=strict should use strict mode."
  (with-environment-variables (("NODE_REPL_MODE" "strict"))
    ;; global variables are not allowed in strict mode
    (js-comint-test-output-matches "foo = 5;" "Uncaught ReferenceError.*")))

(ert-deftest js-comint-select-node-version/test-no-nvm ()
  "Should error if nvm is missing."
  (let ((original-command-value js-comint-program-command))
    (with-mock
     (mock (require 'nvm) => (error "Cannot open nvm"))
     (should-error (js-comint-select-node-version))
     (should-not js-use-nvm)
     (should (equal js-comint-program-command
                    original-command-value)))))

(ert-deftest js-comint-select-node-version/test-with-arg ()
  "Should set program-command when called non-interactively."
  (let ((original-command-value js-comint-program-command)
        (original-use-jvm-value js-use-nvm)
        (original-nvm-version js-nvm-current-version))
    (unwind-protect
        (with-mock
          (mock (require 'nvm))
          (mock (nvm--find-exact-version-for "foo") => '("foo-1.2" "some_path"))
          (js-comint-select-node-version "foo")
          (should js-use-nvm)
          (should (equal js-comint-program-command
                         "some_path/bin/node"))
          (should (equal js-nvm-current-version
                         '("foo-1.2" "some_path"))))
      (setq js-comint-program-command original-command-value
            js-use-nvm original-use-jvm-value
            js-nvm-current-version original-nvm-version))))

(ert-deftest js-comint-select-node-version/test-optional-arg ()
  "Should set program-command when called with no arg."
  (let ((original-command-value js-comint-program-command)
        (original-use-jvm-value js-use-nvm)
        (original-nvm-version js-nvm-current-version))
    (unwind-protect
        (with-mock
          (mock (require 'nvm))
          (mock (js-comint-list-nvm-versions *) => "foo")
          (mock (nvm--find-exact-version-for "foo") => '("foo-1.2" "some_path"))
          (js-comint-select-node-version)
          (should (equal js-comint-program-command
                         "some_path/bin/node")))
      (setq js-comint-program-command original-command-value
            js-use-nvm original-use-jvm-value
            js-nvm-current-version original-nvm-version))))

(ert-deftest js-comint--current-input/test ()
  "Tests default behavior."
  (with-new-js-comint-buffer
   (insert "Array")
   (should (equal (js-comint--current-input) "Array"))
   (comint-send-input)
   (should (equal (js-comint--current-input) ""))
   (goto-char (point-min))
   (should (equal (js-comint--current-input) nil))))

(ert-deftest js-comint--complete-substring/test ()
  "Tests normal behavior."
  (should (equal (js-comint--complete-substring "foo; bar")
                 "bar"))
  (should (equal (js-comint--complete-substring "if(tru")
                 "tru"))
  (should (equal (js-comint--complete-substring "for (let i of myObject.pro")
                 "myObject.pro")))

(ert-deftest js-comint--should-complete/test ()
  "Tests default behavior."
(with-new-js-comint-buffer
  (insert "Arr")
  (should (js-comint--should-complete))
  (comint-kill-input)

  (insert "Array.")
  (should (js-comint--should-complete))
  (comint-kill-input)

  ;; empty line
  (should (js-comint--should-complete))

  (insert "// a comment")
  (should-not (js-comint--should-complete))
  (comint-kill-input)

  (insert "\"foo")
  (should-not (js-comint--should-complete))
  (comint-kill-input)

  (insert "[1,2,")
  (should-not (js-comint--should-complete))
  (comint-kill-input)

  (insert "foo() ")
  (should-not (js-comint--should-complete))
  (comint-kill-input)

  ;; (insert "let foo")
  ;; (should-not (js-comint--should-complete))
  ;; (comint-kill-input)
  ))

(ert-deftest js-comint--process-completion-output/test-globals ()
  "Completing an empty string."
  (should
   (equal
    (js-comint--process-completion-output
     " 
AbortController                   AbortSignal                       AggregateError                    Array

constructor

[1G[0J> 	[9G"
     "")
    '("AbortController"
      "AbortSignal"
      "AggregateError"
      "Array"
      "constructor"))))

(ert-deftest js-comint--process-completion-output/test-single-completion ()
  "Completion of \"Arr\" yields a single result and type info."
  (should (equal (js-comint--process-completion-output
                  "Array
// [Function: Array][8G[1A"
                  "Arr")
                 '("Array"))))

(ert-deftest js-comint--process-completion-output/test-method-completion ()
  "Completion of object properties should give list of properties prefixed with name."
  (should
   (equal
    (js-comint--process-completion-output
     "Array.
Array.__proto__             Array.hasOwnProperty        Array.isPrototypeOf         Array.propertyIsEnumerable  Array.toLocaleString
Array.valueOf

[1G[0J> Array.[9G"
     "Array.")
    '("Array.__proto__"
      "Array.hasOwnProperty"
      "Array.isPrototypeOf"
      "Array.propertyIsEnumerable"
      "Array.toLocaleString"
      "Array.valueOf"))))

(ert-deftest js-comint--process-completion-output/test-multiline-prefix ()
  "Completion when there is a '...' prefix."
  (should
   (equal
    (js-comint--process-completion-output
     "Array.
Array.__proto__             Array.hasOwnProperty        Array.isPrototypeOf         Array.propertyIsEnumerable  Array.toLocaleString
Array.valueOf

[1G[0J... Array.[11G"
     "Array.")
    '("Array.__proto__"
      "Array.hasOwnProperty"
      "Array.isPrototypeOf"
      "Array.propertyIsEnumerable"
      "Array.toLocaleString"
      "Array.valueOf"))))

(ert-deftest js-comint--async-output-filter/test-no-callbacks ()
  "Output should be kept when no callbacks are active."
  (with-temp-buffer
    (should (equal (js-comint--async-output-filter "foo")
                   "foo"))
    ;; should be the same when old callbacks are used
    (with-mock
      (mock (js-comint--callback-active-p *) => nil)
      (setq js-comint--completion-buffer nil
            js-comint--completion-callbacks (list ()))
      (should (equal (js-comint--async-output-filter "foo")
                     "foo"))
      ;; callbacks should be cleared too
      (should-not js-comint--completion-callbacks))))

(ert-deftest js-comint--async-output-filter/test-discard ()
  "Output should be discarded when completion callback is active."
  (with-temp-buffer
    ;; function ignore always returns nil, so it is never cleared
    (setq js-comint--completion-callbacks (list '(:function ignore)))
    (with-mock
      (stub js-comint--callback-active-p => 't)
      (dolist (output '("foo" "bar" "[1G"))
        (should (string-empty-p (js-comint--async-output-filter output)))))
    ;; text should be in completion buffer
    (should (equal (with-current-buffer js-comint--completion-buffer (buffer-string))
                   "foobar[1G"))
    (should (equal js-comint--completion-callbacks
                   (list '(:function ignore))))))

(ert-deftest js-comint--async-output-filter/test-callback-error ()
  "Callback should be removed if it signals an error."
  (with-temp-buffer
    (setq js-comint--completion-callbacks
          (list (list :function (lambda () (error "This should be caught!")))))
    (with-mock
      (stub js-comint--callback-active-p => 't)
      (dolist (output '("foo" "bar" "[1G"))
        (js-comint--async-output-filter output)))
    ;; callback should be removed
    (should-not js-comint--completion-callbacks)))

(ert-deftest js-comint--async-output-filter/test-callback-chain ()
  "Callbacks should be able to add further callbacks."
  (with-temp-buffer
    (setq js-comint--completion-callbacks
          (list (list :function (lambda ()
                                  (push '(:function foo) js-comint--completion-callbacks)
                                  't))))
    (with-mock
      (stub js-comint--callback-active-p => 't)
      (should (string-empty-p (js-comint--async-output-filter "foo"))))
    ;; new callback should be added
    (should (equal js-comint--completion-callbacks
                   (list '(:function foo))))))

(ert-deftest js-comint--clear-input-async/test ()
  "Should send correct clear command and complete on expected response."
  (with-temp-buffer
    (setq js-comint--completion-callbacks
          (list '(:function always)))
    (with-mock
      (stub js-comint--callback-active-p => 't)
      (mock (process-send-string * ""))
      (js-comint--async-output-filter "foo")
      ;; node input is "foo"
      ;; completion buffer is empty as there are no active callbacks
      (js-comint--clear-input-async)
      ;; term sends a prompt
      (should (string-empty-p (js-comint--async-output-filter "[1G[0J> [3G")))
      ;; buffer should be empty and no active callbacks
      (should (js-comint--completion-looking-back-p "^$"))
      (should-not js-comint--completion-callbacks))))

(ert-deftest js-comint--async-output-filter/test-no-completion ()
  "Output should be saved until string match, then fail."
  (with-mock
    (stub js-comint--callback-active-p => 't)
    ;; 1 - complete foo
    ;; 2 - finished test " \b"
    ;; 3 - clear ""
    (mock (process-send-string * *) :times 3)
    (with-temp-buffer
      ;; callback should be called with nil
      (js-comint--get-completion-async "foo" (lambda (arg) (should-not arg)))
      (dolist (output '("f" "oo"               ;; output in chunks
                        "[1G[0J> foo[3G" ;; response to " \b"
                        ))
        (should (string-empty-p (js-comint--async-output-filter output))))
      ;; clear should be called
      (should (equal (plist-get (car js-comint--completion-callbacks) :type)
                     'clear)))))

(ert-deftest js-comint--get-completion-async/test-prop-completion-fail ()
  "When completion fails on something that looks like an object don't hang."
  (with-mock
    (stub js-comint--callback-active-p => 't)
    ;; 1 - complete
    ;; 2 - send another \t
    ;; 3 - finished test " \b"
    ;; 4 - clear
    (mock (process-send-string * *) :times 4)
    (with-temp-buffer
      ;; callback should be called with nil
      (js-comint--get-completion-async "scrog." (lambda (arg) (should-not arg)))
      (dolist (output '("s" "crog." ;; output in chunks
                        "scrog."    ;; response to repeat \t
                        "[1G[0J> scrog.[3G" ;; response to " \b"
                        ))
        (should (string-empty-p (js-comint--async-output-filter output))))
      ;; clear should be called
      (should (equal (plist-get (car js-comint--completion-callbacks) :type)
                     'clear)))))

(ert-deftest js-comint--get-completion-async/test-user-callback-error ()
  "Should clear even if supplied callback errors."
  (with-mock
    (stub js-comint--callback-active-p => 't)
    (stub process-send-string)
    (with-temp-buffer
      ;; callback errors
      (js-comint--get-completion-async "foo"
                                       (lambda (arg) (error "Broken user callback")))
      ;; after output the erroring callback is called with nil
      (dolist (output '("f" "oo"
                        "[1G[0J> foo[3G" ;; response to " \b"
                        ))
        (should (string-empty-p (js-comint--async-output-filter output))))
      ;; clear should be called
      (should (equal (plist-get (car js-comint--completion-callbacks) :type)
                     'clear)))))

(ert-deftest js-comint--get-completion-async/test-user-callback-error-2 ()
  "Should clear even if supplied callback errors (multiple completions)."
  (with-mock
    (stub js-comint--callback-active-p => 't)
    (stub process-send-string)
    (with-temp-buffer
      ;; callback errors
      (js-comint--get-completion-async "foo"
                                       (lambda (arg) (error "Broken user callback")))
      ;; after output the erroring callback is called with nil
      (dolist (output '("foo bar baz\n[1G[0J> foo[3G"))
        (should (string-empty-p (js-comint--async-output-filter output))))
      ;; clear should be called
      (should (equal (plist-get (car js-comint--completion-callbacks) :type)
                     'clear)))))

(ert-deftest js-comint--get-completion-async/test-prop-completion ()
  "When completing object properties, send another tab to get completion."
  (with-mock
    (stub js-comint--callback-active-p => 't)
    (mock (process-send-string * *) :times 3)
    (with-temp-buffer
      ;; callback should be called with ("foo" "bar" "baz")
      (js-comint--get-completion-async "Array."
                                       (lambda (arg)
                                         (should (equal arg '("foo" "bar" "baz")))))
      ;; after the second tab get completion suggestions
      (dolist (output '("A" "rray." "Array. foo bar baz\n[1G[0J> foo[3G"))
        (should (string-empty-p (js-comint--async-output-filter output))))
      ;; clear should be called
      (should (equal (plist-get (car js-comint--completion-callbacks) :type)
                     'clear)))))

;;; Company Integration Tests

;; sanity check: node should interpret ^U
(ert-deftest js-comint--clear-input-async/test-integration ()
  "Tests whether clear works in a live comint."
  (with-new-js-comint-buffer
    (process-send-string (get-buffer-process (current-buffer)) "5")
    (js-comint--clear-input-async)
    (comint-send-input)
    (let ((output (buffer-substring-no-properties
                   comint-last-input-end
                   (car comint-last-prompt))))
      ;; if it fails, node will see 5^U and fail, or see just 5 and echo it
      (should (string-empty-p output)))))

(ert-deftest js-comint/test-dumb-term ()
  "TERM env var should not be dumb."
  (js-comint-test-output-matches "process.env['TERM']" "emacs"))

(ert-deftest js-comint/test-company-global ()
  "Tests completion with an empty prompt."
  (with-new-js-comint-buffer
    (company-mode)
    (sit-for 1)
    (company-manual-begin)
    ;; register callback to see globals
    (company-complete-selection)
    (should (looking-back "AbortController"))))

(ert-deftest js-comint/test-company-unique-result ()
  "Completing with a unique result.
E.g. Arr => Array, or conso => console."
  (with-new-js-comint-buffer
    (company-mode)
    (setq company-async-timeout 5)
    (sit-for 1)
    (insert "Arra")
    (company-complete)
    (should (looking-back "Array"))))

(ert-deftest js-comint/test-company-complete-props ()
  "Completing props of an object.
E.g. should complete \"Array.\" to all properties."
  (with-new-js-comint-buffer
    (company-mode)
    (sit-for 1)
    (insert "Array.")
    (company-manual-begin)
    (company-complete-selection)
    (should (looking-back "Array.__proto__"))))

;; TODO also test that input is correctly cleared here, i.e. can send a string
(ert-deftest js-comint/test-company-complete-long-line ()
  "Completing part of a line.
E.g. 'if (true) { console.'"
  (with-new-js-comint-buffer
    (company-mode)
    (sit-for 1)
    (insert "if (true) { console.")
    (company-manual-begin)
    (company-complete-selection)
    (should (looking-back "console.__proto__"))))
