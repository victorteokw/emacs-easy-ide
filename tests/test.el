;; Setup test

(require 'ert)
(require 'init "../init.el")

(ert-deftest test-truth ()
  (should (equal t t)))
