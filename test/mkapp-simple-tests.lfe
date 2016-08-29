(include-lib "../include/lbx.lfe")
(include-lib "../include/mkapp.lfe")

;An exmaple of a gen_server and its api generated bu mk-genserver
;This creates the info module and the info_api module
(genserver info
  ((info (tuple 'store val)
     (progn
       (ets:insert 'testdb `#(info ,val))
       (let ((`#(info ,val) (hd (ets:lookup 'testdb 'info)))) val)
       `#(noreply ,(state))))
  ) ;end of api
  (print local)
)

(genserver initargs
  ((state-match (tuple bucket value))
   (call store (val)
     `#(reply ok ,(tuple bucket val)))
   (init (server-bucket)
     `#(ok ,(tuple server-bucket 'undefined)))
  ) ;end of api
  (print)
)


;;Anything here goes in the info module


;Tests
(defmodule mkapp-simple-tests
 (behaviour ltest-unit)
 (export all)
 (import
  (from ltest
   (check-failed-is 2)
   (check-wrong-is-exception 2))))

(include-lib "ltest/include/ltest-macros.lfe")

;;------------------------------------
;; Simple info tests
;;------------------------------------
(defun info-set-up ()
  (progn
    (ets:new 'testdb '(public named_table))
    (info:start_link)))

(defun info-tear-down (set-up-result)
  (progn
    (gen_server:stop 'info)
    (ets:delete 'testdb)))

(deftest start-info-server
 (is-match (tuple 'ok _) (info-set-up)))

(deftest stop-info-server
 (is-equal 'true (info-tear-down 'ok)))

(deftestcase info-msg (sres)
  (tuple "local info api"
    (is-equal 'test_msg
      (progn
        (! 'info #(store test_msg))
        (? 200 'ok) ;wait for insert
        (2nd (hd (ets:lookup 'testdb 'info) ))))))

(deftestcase default-initial-state (sres)
  (tuple "empty default initial state" (is-equal '() (sys:get_state 'info))))

(deftestgen info-cases
  `#(foreach
     ,(defsetup info-set-up)
     ,(defteardown info-tear-down)
     ,(deftestcases
         info-msg
         default-initial-state
         )))

;;------------------------------------
;; Tests for init with arguments
;; - run several gen_servers
;;   of the same type
;;------------------------------------
(defun initargs-set-up ()
  `(,(2nd (initargs:start_link 'bucket-one))
    ,(2nd (initargs:start_link 'bucket-two))))

(defun initargs-tear-down ([(list pid1 pid2)]
  (gen_server:stop pid1)
  (gen_server:stop pid2)))

(defun initargs_initial_state_1 (sres)
  (is-equal #(bucket-one undefined)
             (sys:get_state (car sres))))

(defun initargs_initial_state_2 (sres)
  (is-equal #(bucket-two undefined)
            (sys:get_state (cadr sres))))

(defun initargs_not_registered (sres)
  (is-not (lists:member 'initargs (global:registered_names)))
  (is-not (lists:member 'initargs1 (registered))))

(defun initargs_call_1 (sres)
  (is-equal #(bucket-one 555)
    (progn
      (initargs_api:store (car sres) 555)
      (sys:get_state (car sres)))))

(defun initargs_call_2 (sres)
  (is-equal #(bucket-two 777)
    (progn
      (initargs_api:store (cadr sres) 777)
      (sys:get_state (cadr sres)))))

;;Function names here need to use _ instead of - because
;;otherwise eunit breaks
;;see https://github.com/erlang/otp/blob/maint/lib/eunit/src/eunit_lib.erl#L383
(defun initargs_test_maker (sres)
  "Generate a list of tests for the initargs genserver."
  `(
     ,(tuple "Initial state server 1" (lambda() (initargs_initial_state_1 sres)))
     ,(tuple "Initial state server 2" (lambda() (initargs_initial_state_2 sres)))
     ,(tuple "initargs genserver not registered" (lambda() (initargs_not_registered sres)))
     ,(tuple "Server 1 call" (lambda() (initargs_call_1 sres)))
     ,(tuple "Server 2 call" (lambda() (initargs_call_2 sres)))
	))

(deftestgen initargs-run
  `#(setup
      ,(defsetup initargs-set-up)
      ,(defteardown initargs-tear-down)
		,#'initargs_test_maker/1
      ))
