(defmodule lbx-tests
 (behaviour ltest-unit)
 (export all)
 (import
  (from ltest
   (check-failed-is 2)
   (check-wrong-is-exception 2))))


(include-lib "lbx/include/lbx.lfe")
(include-lib "ltest/include/ltest-macros.lfe")

(defun fmt-var-data () "^r^hi^b^~s")

(defun set-up () (doors:start_link))

(defun tear-down (set-up-result) (gen_server:stop #(global doors)))

(deftest fmt-no-color
 (is-equal "hiho" (fmt "hi~s" (list "ho"))))

(deftest fmt-color
 (is-equal "\e[0;31mhi\e[0;34mho\e[0m" (fmt "^r^hi^b^~s" (list "ho"))))

(deftest fmt-fun
 (is-equal "\e[0;31mhi\e[0;34mho\e[0m" (fmt (fmt-var-data) (list "ho"))))

;(deftest exec-echo
; (is-equal "hello" (exec "echo hello")))
