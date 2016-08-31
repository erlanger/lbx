(include-lib "../include/lbx.lfe")
(include-lib "../include/mkapp.lfe")

(application myapp mysup (print (description "Test app")))

(supervisor mysup
            (;(worker wrk1 restart temporary)
             (supervisor mysup1)
             (supervisor (mysup2 intensity 15 period 70))
             (supervisor mysup3
               ((worker wrk31)
                (worker wrk32))))
            (strategy one_for_one)
            (print))

(genserver wrk31
  ((call who () #(reply "I am wrk31\n" State)))
  (print global)
)

(genserver wrk32
  ((call who () #(reply "I am wrk32\n" State))
   (call die () `#(reply ,(error 'died) State)))
  (print local)
)




;;------------------------------------
;; Tests
;;------------------------------------
(defmodule app-tests
 (behaviour ltest-system)
 (export all)
 (import
  (from ltest
   (check-failed-is 2)
   (check-wrong-is-exception 2))))

(include-lib "ltest/include/ltest-macros.lfe")

(defun app-set-up ()
  (is-match (tuple 'ok _) (application:ensure_all_started 'myapp)))

(defun app-tear-down (sres)
  (application:stop 'myapp))

(defun app_call_wrk31 (sres)
  (is-equal "I am wrk31\n" (wrk31_api:who)))

(defun app_call_wrk32 (sres)
  (is-equal "I am wrk32\n" (wrk32_api:who)))

(defun restart_wrk32 (sres)
  (is-exit _ (wrk32_api:die))
  (? 100 'should-restart-by-now)
  (is-equal "I am wrk32\n" (wrk32_api:who)))

(defun app_test_maker (sres)
  "Generate a list of tests for the app, noglobal genservers."
  `(
     ,(tuple "Call worker 31" (lambda()    (app_call_wrk31 sres)))
     ,(tuple "Call worker 32" (lambda()    (app_call_wrk32 sres)))
     ,(tuple "Restart worker 32" (lambda() (restart_wrk32 sres)))
     ))

(deftestgen app-run
  `#(setup
      ,(defsetup app-set-up)
      ,(defteardown app-tear-down)
		,#'app_test_maker/1
      ))
