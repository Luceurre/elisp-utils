(require 'f)

(defvar elisp-utils-support-path
  (f-dirname load-file-name))

(defvar elisp-utils-features-path
  (f-parent elisp-utils-support-path))

(defvar elisp-utils-root-path
  (f-parent elisp-utils-features-path))

(add-to-list 'load-path elisp-utils-root-path)

;; Ensure that we don't load old byte-compiled versions
(let ((load-prefer-newer t))
  (require 'elisp-utils)
  (require 'espuds)
  (require 'ert))

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
