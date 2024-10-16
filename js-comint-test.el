;; -*- lexical-binding: t -*-

(require 'js-comint)
(require 'ert)
(require 'el-mock)

(defun js-comint-test-buffer-matches (regex)
  "Search the js-comint buffer for the given regular expression.
Return 't if a match is found, nil otherwise."
  (with-current-buffer (js-comint-get-buffer)
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward regex nil t) t nil))))

(defun js-comint-test-output-matches (input regex)
  "Verify that sending INPUT yields output that matches REGEX."

  ;; Start an instance to run tests on.
  (js-comint-reset-repl)

  (sit-for 1)

  (js-comint-send-string input)

  (sit-for 1)

  (js-comint-test-buffer-matches regex))

(defun js-comint-test-exit-comint ()
  "Finish process."
  (when (js-comint-get-process)
    (process-send-string (js-comint-get-process) ".exit\n")
    (sit-for 1)))

(ert-deftest js-comint-test-multiline-dotchain-line-start ()
  "Test multiline statement with dots at beginning of lines."
  (should (js-comint-test-output-matches "[1, 2, 3]
  .map((it) => it + 1)
  .filter((it) => it > 0)
  .reduce((prev, curr) => prev + curr, 0);" "^9$")))

(ert-deftest js-comint-test-multiline-dotchain-line-start-dos ()
  "Test multiline statement with dots at beginning of lines, with
DOS line separators."
  (should (js-comint-test-output-matches "[1, 2, 3]\r
  .map((it) => it + 1)\r
  .filter((it) => it > 0)\r
  .reduce((prev, curr) => prev + curr, 0);\r
" "^9$")))

(ert-deftest js-comint-test-multiline-dotchain-line-end ()
  "Test multiline statement with dots at end of lines."
  (should (js-comint-test-output-matches "[1, 2, 3].
map((it) => it + 1).
filter((it) => it > 0).
reduce((prev, curr) => prev + curr, 0);" "^9$")))

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
          (js-comint-test-exit-comint)
          (js-comint-start-or-switch-to-repl)
          (sit-for 1)
          (js-comint-send-string "process.env['NODE_PATH'];")
          (js-comint-test-buffer-matches "/foo/bar"))
      (setq js-comint-module-paths original
            js-comint-set-env-when-startup original-set-env)
      (setenv "NODE_PATH" original-env)
      (js-comint-test-exit-comint))))

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
          (js-comint-test-exit-comint)
          (js-comint-start-or-switch-to-repl)
          (sit-for 1)
          (js-comint-send-string "process.env['NODE_PATH'];")
          (js-comint-test-buffer-matches (concat "/foo/bar" (js-comint--path-sep) "/baz/xyz")))
      (setq js-comint-module-paths original
            js-comint-set-env-when-startup original-set-env)
      (setenv "NODE_PATH" original-env)
      (js-comint-test-exit-comint))))

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
          (js-comint-test-exit-comint)
          (js-comint-start-or-switch-to-repl)
          (sit-for 1)
          (js-comint-send-string "process.env['NODE_PATH'];")
          (js-comint-test-buffer-matches (concat "/foo/bar" (js-comint--path-sep) "/baz/xyz")))
      (setq js-comint-module-paths original
            js-comint-set-env-when-startup original-set-env)
      (setenv "NODE_PATH" original-env)
      (fset 'js-comint--suggest-module-path original-suggest)
      (js-comint-test-exit-comint))))

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

(ert-deftest js-comint--completion-filter/test-discard ()
  "Output should be discarded."
  (with-temp-buffer
    (js-comint--reset-completion-state)
    (setq js-comint--discard-output 't)
    ;; each should be empty
    (dolist (res (list (js-comint--completion-filter "foo")
                       (js-comint--completion-filter "bar")
                       (js-comint--completion-filter "[1G")))
     (should (string-empty-p res)))
    ;; then the the flag should be cleared
    (should-not js-comint--discard-output)))

(ert-deftest js-comint--completion-filter/test-discard-with-completion ()
  "Output should be discarded even when completion callback is set."
  (with-temp-buffer
    (js-comint--reset-completion-state)
    (setq js-comint--discard-output 't)
    (setq js-comint--post-completion-cb #'ignore)
    ;; each should be empty
    (dolist (res (list (js-comint--completion-filter "foo")
                       (js-comint--completion-filter "bar")
                       (js-comint--completion-filter "[1G")))
      (should (string-empty-p res)))
    ;; the output should not be accumulated
    (should (string-empty-p js-comint--completion-output))))
