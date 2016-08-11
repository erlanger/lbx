(defmodule lbx
 (export all))

(include-lib "../include/lbx.lfe")

;if x is a #(ok Result) tuple return Result,
;otherwise error out of the process with x
(defun ifok! (x)
  (case x
    ([tuple 'ok res] res)
    (err (error err))))

; Print result so far and return the result to be used in the rest
; of the thread.
;
; Example:
; lfe>  (-> 1 (+ 2) (pr "result:~p~n") (+ 5))
; result:3
; 8
(defun pr 
  ([x y]
    (progn 
      (io:format y (list x))
      x)))

; Log the resul so far and return the result to be used in the rest
; of the thread. error_logger:info_msg is used to log the message.
;
; Example:
; lfe>  (-> 1 (+ 2) (lgi "result:~p~n") (+ 5))
; 8
;
; =INFO REPORT==== 17-Jul-2016::11:36:52 ===
; result:3
(defun lgi 
  ([x y]
    (progn 
      (error_logger:info_msg y (list x))
      x)))

; Log the resul so far and return the result to be used in the rest
; of the thread. error_logger:error_msg is used to log the message.
;
; Example:
; lfe>  (-> 1 (+ 2) (case (3 (lge "error, got three!:~p~n")) (r r)) (+ 5))
; 8
;
; =INFO REPORT==== 17-Jul-2016::11:36:52 ===
; result:3
(defmacro lge 
  ([x tst msg]
    `(progn 
      (if ,tst
        (error_logger:error_msg ,msg (list ,x)))
      ,x)))

;;---------------------------------------------------------------------
;; Utility functions
;; chop spnl
;;---------------------------------------------------------------------
(defun chop 
  "Delete newline at end of string."
  ((str) (when (is_list str))
    (-> str
        ;We reverse to delete only the last new line
        lists:reverse
        (-- (io_lib:nl))
        lists:reverse))
  ((str) (when (is_binary str))
        (-> str binary_to_list chop list_to_binary)))

(defun spnl
  "Split string at every newline; returning a list of strings 
   without including the line termination characters."
  ([s] (when (is_binary s)) 
    (binary:split s (list_to_binary (io_lib:nl)) '(global trim_all)))
  ([s] (when (is_list s)) 
    (-> (list_to_binary s) 
        (binary:split (list_to_binary (io_lib:nl)) '(global trim_all))
        (lists:map (lambda (e)
                     (binary_to_list e)) @))))
;*************************************************************************
;*************************************************************************
; Pool setup/nodes
;*************************************************************************
;*************************************************************************
(defun tonodes-unused (module)
  (let (((tuple mod1 bin file ) 
          ;; Find object code for module Mod
          (code:get_object_code module)))
    ;; and load it on all nodes if not already loaded 
    (lists:foldl (lambda (n res)
                   (spawn n
                          (lambda ()
                            (case (code:is_loaded module)
                              ('false
                                (progn
                                  (io:format "Loading ~p in ~p~n" (list module n))
                                  (code:load_binary mod1 file bin)))
                              (_ 'already_loaded)))))
                   ()
                   (nodes))))

(defun spraycode ()
  "Send all loaded modules to all connected nodes."
  (lists:map (lambda (e) (tonodes (element 1 e))) (code:all_loaded)))

(defun startpool (nodename)
  "Start a pool of nodes on every host listed in the .hosts.erlang file; 
   the name of the node is specified by <nodename>."
  (pool:start nodename (++ "-loader inet -hosts "
                        "'" (getifaddr) "'"
                        " -pz /opt/lfe/ebin")))

(defun startpool()
  (startpool 'n1))

(defun getifaddr ()
  (-> (inet:getifaddrs)
      (case 
        ((tuple 'ok T) T)
        ((tuple 'error E) (error E)))
      (lists:map (lambda (e) 
                   (-> e 2nd))
                 @)
      (lists:map (lambda (e)
                       (case e
                         ((tuple 'addr (tuple A B C D)) (when (!= A 127))
                           (++ (integer_to_list A) "."
                               (integer_to_list B) "."
                               (integer_to_list C) "."
                               (integer_to_list D)))
                         (_ ())))
                 (lists:flatten @))
      (lists:flatten)))
