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



;;; elisp-utils-test.el ends here
