;; haskell-emacs-module-test.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 28 May 2018
;; Description:

(require 'ert)

;; (setq-default garbage-collection-messages t)

(load-library "libemacs-module-test")

(defun custom-replicate (n x)
  (let ((res nil))
    (dotimes (i n)
      (push x res))
    res))

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


(ert-deftest haskell-emacs-module-test/append-lots-of-vectors-01 ()
  (should (equal (haskell-emacs-module-tests-append-lots-of-vectors 0) [])))

(ert-deftest haskell-emacs-module-test/append-lots-of-vectors-02 ()
  (should (equal (haskell-emacs-module-tests-append-lots-of-vectors 1) [1 2 3])))

(ert-deftest haskell-emacs-module-test/append-lots-of-vectors-03 ()
  (should (equal (haskell-emacs-module-tests-append-lots-of-vectors 2) [1 2 3 1 2 3])))

(ert-deftest haskell-emacs-module-test/append-lots-of-vectors-04 ()
  (should (equal (haskell-emacs-module-tests-append-lots-of-vectors 3) [1 2 3 1 2 3 1 2 3])))

(ert-deftest haskell-emacs-module-test/append-lots-of-vectors-05 ()
  (should (equal (haskell-emacs-module-tests-append-lots-of-vectors 10)
                 (apply #'vconcat (custom-replicate 10 [1 2 3])))))

(ert-deftest haskell-emacs-module-test/append-lots-of-vectors-06 ()
  (should (equal (haskell-emacs-module-tests-append-lots-of-vectors 100)
                 (apply #'vconcat (custom-replicate 100 [1 2 3])))))

(ert-deftest haskell-emacs-module-test/append-lots-of-vectors-07 ()
  (should (equal (haskell-emacs-module-tests-append-lots-of-vectors 1000)
                 (apply #'vconcat (custom-replicate 1000 [1 2 3])))))

(ert-deftest haskell-emacs-module-test/append-lots-of-vectors-08 ()
  (should (equal (haskell-emacs-module-tests-append-lots-of-vectors 2000)
                 (apply #'vconcat (custom-replicate 2000 [1 2 3])))))


(ert-deftest haskell-emacs-module-test/append-lots-of-strings-01 ()
  (should (equal (haskell-emacs-module-tests-append-lots-of-strings 0) "")))

(ert-deftest haskell-emacs-module-test/append-lots-of-strings-02 ()
  (should (equal (haskell-emacs-module-tests-append-lots-of-strings 1) "foo")))

(ert-deftest haskell-emacs-module-test/append-lots-of-strings-03 ()
  (should (equal (haskell-emacs-module-tests-append-lots-of-strings 2) "foofoo")))

(ert-deftest haskell-emacs-module-test/append-lots-of-strings-04 ()
  (should (equal (haskell-emacs-module-tests-append-lots-of-strings 3) "foofoofoo")))

(ert-deftest haskell-emacs-module-test/append-lots-of-strings-05 ()
  (should (equal (haskell-emacs-module-tests-append-lots-of-strings 10)
                 (apply #'concat (custom-replicate 10 "foo")))))

(ert-deftest haskell-emacs-module-test/append-lots-of-strings-06 ()
  (should (equal (haskell-emacs-module-tests-append-lots-of-strings 100)
                 (apply #'concat (custom-replicate 100 "foo")))))

(ert-deftest haskell-emacs-module-test/append-lots-of-strings-07 ()
  (should (equal (haskell-emacs-module-tests-append-lots-of-strings 1000)
                 (apply #'concat (custom-replicate 1000 "foo")))))

(ert-deftest haskell-emacs-module-test/append-lots-of-strings-08 ()
  (should (equal (haskell-emacs-module-tests-append-lots-of-strings 2000)
                 (apply #'concat (custom-replicate 2000 "foo")))))

(ert-deftest haskell-emacs-module-tests-replicate-01 ()
  (should (equal (haskell-emacs-module-tests-replicate 0 '(a b c))
                 nil)))

(ert-deftest haskell-emacs-module-tests-replicate-02 ()
  (should (equal (haskell-emacs-module-tests-replicate 1 '(a b c))
                 '((a b c)))))

(ert-deftest haskell-emacs-module-tests-replicate-03 ()
  (should (equal (haskell-emacs-module-tests-replicate 3 '(a b c))
                 '((a b c) (a b c) (a b c)))))


(provide 'haskell-emacs-module-test)

;; Local Variables:
;; End:

;; haskell-emacs-module-test.el ends here
