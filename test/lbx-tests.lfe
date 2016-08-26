(defmodule lbx-tests
 (behaviour ltest-unit)
 (export all)
 (import
  (from ltest
   (check-failed-is 2)
   (check-wrong-is-exception 2))))


(include-lib "../include/lbx.lfe") ;needed for -> like macros
(include-lib "ltest/include/ltest-macros.lfe")

(defun fmt-var-data () "^r^hi^b^~s")

(defun list-data () '(one 2 "three" "four" 5 6 7 8 9 ten))

(defun tuple-data () #(one 2 "three" "four" 5 6 7 8 9 ten))

(defun set-up () (doors:start_link))

(defun tear-down (set-up-result) (gen_server:stop #(global doors)))

;---------------- List and tuple tests ---------------
(deftest list-1st
  (is-equal 'one (lbx:1st (list-data))))

(deftest list-2nd
  (is-equal 2 (lbx:2nd (list-data))))

(deftest list-3rd
  (is-equal "three" (lbx:3rd (list-data))))

(deftest list-4th
  (is-equal "four" (lbx:4th (list-data))))

(deftest list-5th
  (is-equal 5 (lbx:5th (list-data))))

(deftest list-6th
  (is-equal 6 (lbx:6th (list-data))))

(deftest list-7th
  (is-equal 7 (lbx:7th (list-data))))

(deftest list-8th
  (is-equal 8 (lbx:8th (list-data))))

(deftest list-9th
  (is-equal 9 (lbx:9th (list-data))))

(deftest list-10th
  (is-equal 'ten (lbx:10th (list-data))))

(deftest tuple-1st
  (is-equal 'one (lbx:1st (tuple-data))))

(deftest tuple-2nd
  (is-equal 2 (lbx:2nd (tuple-data))))

(deftest tuple-3rd
  (is-equal "three" (lbx:3rd (tuple-data))))

(deftest tuple-4th
  (is-equal "four" (lbx:4th (tuple-data))))

(deftest tuple-5th
  (is-equal 5 (lbx:5th (tuple-data))))

(deftest tuple-6th
  (is-equal 6 (lbx:6th (tuple-data))))

(deftest tuple-7th
  (is-equal 7 (lbx:7th (tuple-data))))

(deftest tuple-8th
  (is-equal 8 (lbx:8th (tuple-data))))

(deftest tuple-9th
  (is-equal 9 (lbx:9th (tuple-data))))

(deftest tuple-10th
  (is-equal 'ten (lbx:10th (tuple-data))))

(deftest list-take
  (is-equal '(one 2 "three" "four" 5) (lbx:take 5 (list-data))))

(deftest list-last
  (is-equal 'ten (lbx:last (list-data))))

(deftest tuple-last
  (is-equal 'ten (lbx:last (tuple-data))))


;---------------- threading tests ---------------
;These need (include-lib "lbx.lfe"), recursive
;macros don't work in modules yet

(deftest simple-thread->
  (is-equal 2
    (lbx:-> 1 (+ 1) )))

(deftest three-step-thread->
  (is-equal 3
    (lbx:-> 1 (+ 1) (+ 1))))

(deftest thread->
  (is-equal 'yeah
    (lbx:-> (list-data) (lbx:5th) (+ 2)
            (lists:member '(1 7 9))
            (if (== 'true @) 'yeah 'hmmm))))

(deftest thread-m>
  (is-equal 9
    (lbx:m> (+ 2) (1 (+ 1) (+ 1)))))

(defun cat (x)
  (string:concat x "."))

(deftest thread-m->
  (is-equal "abc.d"
    (lbx:m-> cat ("abc" (++ "d")))))

;---------------- exec tests ------------------------
(deftest os-exec-one-line
  (is-equal #(0 "270\n") (lbx:exec "expr 9 '*' 30")))

(deftest os-exec-shell
  (is-equal #(0 "one\ntwo\n") (lbx:exec "echo one; echo two" 'shell)))

(deftest os-exec-binary-shell
  (is-equal #(0 #"one\ntwo\n")
    (lbx:exec "echo one; sleep 0.1; echo two" '(shell binary))))

(deftest os-exec-exit-status
  (is-equal #(5 ()) (lbx:exec "exit 5" 'shell)))

(deftest os-exec-bang
  (is-equal "10" (lbx:exec! "expr 9 '+' 1")))

(deftest os-exec-bang-non-zero-exit
  (is-exception 'error
    (tuple 'error (tuple 'non_zero_exit_status 5 str))
    (lbx:exec! "exit 5" 'shell)))


;---------------- format/output tests ---------------
(deftest fmt-no-color
 (is-equal "hiho" (lbx:fmt "hi~s" (list "ho"))))

(deftest fmt-fun
 (is-equal "^r^hi^b^ho" (lbx:fmt (fmt-var-data) (list "ho"))))

(deftest cfmt-no-color
 (is-equal "hiho" (lbx:cfmt "hi~s" (list "ho"))))

(deftest cfmt-color
 (is-equal "\e[0;31mhi\e[0;34mho\e[0m" (lbx:cfmt "^r^hi^b^~s" (list "ho"))))

(deftest cfmt-fun
 (is-equal "\e[0;31mhi\e[0;34mho\e[0m" (lbx:cfmt (fmt-var-data) (list "ho"))))

