(include-lib "../include/lbx.lfe")
(include-lib "../include/mkapp.lfe")

;An exmaple of a gen_server and its api generated bu mk-genserver
;This creates the doors module and the doors_api module
(genserver doors 
  ((state-match (tuple roomstate roomkeys))
  (call open (door) 
   "Open the specified door, without a key."
    (open door () roomstate roomkeys (state)))
  (call state (door) `#(reply ,(maps:get door roomstate) ,(state)))
  (call open (door key) (open door key roomstate roomkeys (state)))
  (call-match-3 (tuple 'close door) pid 'one_closed 
      `#(reply ok ,(upd-door (state) door 'closed)))
  (call close (door) 
      `#(reply ok ,(upd-door (state) door 'closed)))
  (cast kick (door) 
    (progn 
      (timer:apply_after 5000 'doors_api 'repair (list door))
      `#(noreply ,(upd-door (state) door 'kicked))))
  (cast repair (door) 
      `#(noreply ,(upd-door (state) door 'open)))
  ;(info (tuple 'close door) (close door))
  (init `#(ok ,(tuple 
                #M(1 open 2 closed 3 open 4 open) 
                #M(1 345))))
  ;trigger prev-state-match curr-state-match tigger-body
  ;trigger prev-state-match curr-state-match guard tigger-body
  ;trigger api-name-match prev-state-match curr-state-match tigger-body
  ;trigger api-name-match prev-state-match curr-state-match guard tigger-body
  ;(trigger-all olds 'one_closed 'true (format "^r^one reached: ~p~n" (list (get-reply))))
  (trigger 'close olds news (!= olds news) 
    (format "^g^door closed: ~p~n" (list (get-args))))
  (trigger 'open olds news (!= olds news) 
    (format "^g^door opened ~p~n" (list (get-args))))
  (trigger 'repair olds news (!= olds news) 
    (format "^g^door repaired ~p~n" (list (get-args))))
  ;(terminate 'ok)
  ) ;end of api
  ;(print #(gproc #(n l #(tier1 1))))
  ;(print local)
  (global print trigger_debug)
  ;(global print )
  ;(print #(api-module myapi_module))
  ;(print)
) 

;These will go in the doors module
(defun upd-door (((tuple roomstate roomkeys) door newstate)
  (let 
   ((newrs (map-update roomstate door newstate)))
    (tuple newrs roomkeys))))

(defun open (door key rs rk State)
  (if (== 'error (maps:find door rk))
   ;no need for key
   (case (maps:get door rs)
     ('open (tuple 'reply 'already_open State))
     ('closed (tuple 'reply 'ok (upd-door State door 'open))))
   ;door needs key
   (case (maps:get door rs)
     ('open (tuple 'reply 'already_open State))
     ('closed
      (if (== key (maps:get door rk 'none))
        (tuple 'reply 'ok (upd-door State door 'open))
        (tuple 'reply 'needs_key State))))))

(defmodule mkapp-doors-tests
 (behaviour ltest-unit)
 (export all)
 (import
  (from ltest
   (check-failed-is 2)
   (check-wrong-is-exception 2))))

(include-lib "ltest/include/ltest-macros.lfe")

(defun set-up () (doors:start_link))

(defun tear-down (set-up-result) (gen_server:stop #(global doors)))

(deftest start-genserver 
 (is-match (tuple 'ok _) (set-up)))

(deftest stop-genserver
 (is-match 'ok (tear-down 'ok)))

(deftestcase open-door-api (sres)
 (tuple "open3" (is-equal 'already_open (doors_api:open 3)))
 (tuple "open2" (is-equal 'ok (doors_api:open 2))))

(deftestcase open-door-w-key-api (sres)
 (is-equal 'already_open  (doors_api:open 1))
 (is-equal 'ok  (doors_api:close 1))
 (is-equal 'needs_key  (doors_api:open 1 000))
 (is-equal 'ok  (doors_api:open 1 345)))

(deftestcase kick-api (sres)
 (is-equal 'ok  (doors_api:kick 2))
 (is-equal 'kicked (doors_api:state 2))
 ;wait until door is repaired by trigger
 (is-equal 'closed (? 5100 (doors_api:state 2)))
 (is-equal 'dummy (? 1100 'dummy)))

(deftestcase close-api (sres)
 (is-equal 'ok  (doors_api:close 2))
 (is-equal 'ok (doors_api:close 1)))

(deftestgen setup-setup-cleanup
  `#(foreach
     ,(defsetup set-up)
     ,(defteardown tear-down)
     ,(deftestcases 
         open-door-api 
         open-door-w-key-api
         kick-api
         close-api)))

