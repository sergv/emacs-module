;; haskell-emacs-module-test.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 28 May 2018
;; Description:

(require 'ert)

(load-library "libemacs-module-test")

(ert-deftest haskell-emacs-module-test/apply-function-twice-1 ()
  (should (fboundp #'haskell-emacs-module-tests-apply2)))

(ert-deftest haskell-emacs-module-test/apply-function-twice-2 ()
  (should (equal (haskell-emacs-module-tests-apply2 (lambda (x) (* x 2)) 10)
                40)))

(ert-deftest haskell-emacs-module-test/apply-function-twice-3 ()
  (let ((acc 0))
    (should (equal (haskell-emacs-module-tests-apply2
                   (lambda (x) (setf acc (+ acc 1)) (+ x acc))
                   10)
                  13))
    (should (equal (haskell-emacs-module-tests-apply2
                   (lambda (x) (setf acc (+ acc 1)) (+ x acc))
                   10)
                  17))))

(ert-deftest haskell-emacs-module-test/apply-function-twice-4 ()
  (let* ((acc 0))
    (should (equal (catch 'wat
                    (progn
                      (haskell-emacs-module-tests-apply2
                       (lambda (x)
                         (setf acc (+ acc 1))
                         (if (<= 2 acc)
                             (throw 'wat x)
                           (+ x acc)))
                       10)
                      0))
                  11))))

(ert-deftest haskell-emacs-module-test/apply-function-twice-5 ()
  (let* ((acc 0))
    (should-error
     (haskell-emacs-module-tests-apply2
      (lambda (x)
        (setf acc (+ acc 1))
        (if (<= 2 acc)
            (error "I'm done with you")
          (+ x acc)))
      10)
     :type 'error)))

(ert-deftest haskell-emacs-module-test/add-1 ()
  (should (fboundp #'haskell-emacs-module-tests-add)))

(ert-deftest haskell-emacs-module-test/add-2 ()
  (should (equal (haskell-emacs-module-tests-add 1 2)
                 3)))

(ert-deftest haskell-emacs-module-test/add-3 ()
  (should (equal (haskell-emacs-module-tests-add -1 -2)
                 -3)))

(ert-deftest haskell-emacs-module-test/add-4 ()
  (should-error
   (haskell-emacs-module-tests-add 1 "oops")
   :type 'error))

;; Test passing and interpretation of the &rest argument.

(ert-deftest haskell-emacs-module-test/get-rest-1 ()
  (should (fboundp #'haskell-emacs-module-tests-get-rest)))

(ert-deftest haskell-emacs-module-test/get-rest-2 ()
  (should (equal (haskell-emacs-module-tests-get-rest t) []))
  (should
   (equal
    (haskell-emacs-module-tests-get-rest 1 2 3 "foo" t 5)
    [2 3 "foo" t 5])))

(provide 'haskell-emacs-module-test)

;; Local Variables:
;; End:

;; haskell-emacs-module-test.el ends here
