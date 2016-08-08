(include-lib "../include/lfebox.lfe")
(include-lib "../include/mkapp.lfe")

;An exmaple of a gen_server and its api generated bu mk-genserver
;This creates the doors module and the doors_api module
(genserver doors 
  ((state-match (tuple roomstate roomkeys))
  (call open (door) (open door () roomstate roomkeys State))
  (call open (door key) (open door key roomstate roomkeys State))
  ;(call-match (tuple 'close door) pid 'one_closed (close (+ 1 door)))
  (call close (door) 
      `#(reply ok ,(upd-door State door 'closed)))
  (cast kick (door) 
    (progn 
      (timer:apply_after 5000 'doors_api 'repair (list door))
      `#(noreply ,(upd-door State door 'kicked))))
  (cast repair (door) 
      `#(noreply ,(upd-door State door 'open)))
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
     ('open (tuple 'reply 'alreay_open State))
     ('closed (tuple 'reply 'ok (upd-door State door 'open))))
   ;door needs key
   (case (maps:get door rs)
     ('open (tuple 'reply 'alreay_open State))
     ('closed
      (if (== key (maps:get door rk 'none))
        (tuple 'reply 'ok (upd-door State door 'open))
        (tuple 'reply 'needs_key State))))))

