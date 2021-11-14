;;; elisp-utils-test.el --- Tests for elisp-utils

(require 'elisp-utils)

(ert-deftest better-insert/add-space ()
  "Should add whitespace before inserting if no space is present."
  (with-temp-buffer
    (insert "Text that doesn't end with white space.")
    (elisp-utils/better-insert "Hello!")
    (should (equal (thing-at-point 'line t) "Text that doesn't end with white space. Hello!")))
   )

(ert-deftest better-insert/no-space ()
  "Should not add whitespace before inserting if space is already present."
  (with-temp-buffer
    (insert "Text that end with white space. ")
    (elisp-utils/better-insert "Hello!")
    (should (equal (thing-at-point 'line t) "Text that end with white space. Hello!")))
  )


(ert-deftest get-associated-function-name-for-test/inside-test()
  "Should return associated function name when inside a test."
  (with-temp-buffer
    (insert "(ert-deftest get-associated-function-name-for-test/inside-test()
  \"Should return associated function name when inside a test.\"
  (with-temp-buffer
    (insert ""))
  )")
    (should (equal (elisp-utils//get-associated-function-name-for-test) "get-associated-function-name-for-test"))
    )
  )

;;; elisp-utils-test.el ends here

(ert-deftest get-associated-function-name-for-test/outside-test()
  "Should return nil when outside a test"
  (with-temp-buffer
    (insert "(defun get-associated-function-name-for-test/inside-test()
  \"Should return associated function name when inside a test.\"
  (with-temp-buffer
    (insert ""))
  )")
    (should (equal (elisp-utils//get-associated-function-name-for-test) nil))
    )
  )
