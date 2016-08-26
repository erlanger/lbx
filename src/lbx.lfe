(defmodule lbx
 (export
         (ifok! 1)
         (pr 2)
         (lgi 2)
         (chop 1)
         (spnl 1)
         (getifaddr 0)
         (startpool 1)
         (cfmt 1)
         (cfmt 2)
         (exec 1)
         (exec 2)
         (sh 1)
         (format 1)
         (format 2)
         (nocolor 1)
         (last 1)
         )
 (export-macro
         1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th take
         -> m> m-> lge
         exec!
         fmt
         color-aux__
         tonodes spraycode)
 )

(include-lib "../include/lbx.lfe")

(defun ifok! (x)
  "if x is a #(ok Result) tuple return Result,
  otherwise error out of the process with x
  "
  (case x
    ([tuple 'ok res] res)
    (err (error err))))

(defun pr
  "Print result so far and return the result to be used in the rest
  of the thread.

  Example:
  lfe>  (-> 1 (+ 2) (pr \"result:~p~n\") (+ 5))
  result:3
  8"
  ([x y]
    (progn
      (io:format y (list x))
      x)))

(defun lgi
  "info_msg() the resul so far and return the result to be used in the rest
  of the thread. error_logger:info_msg is used to log the message.
  ~p specifies where to print the result so far.

  Example:
  lfe>  (-> 1 (+ 2) (lgi \"result:~p~n\") (+ 5))
  8

  =INFO REPORT==== 17-Jul-2016::11:36:52 ===
  result:3
  "
  ([x y]
    (progn
      (error_logger:info_msg y (list x))
      x)))

(defmacro lge
  "error_msg() the resul so far and return the result to be used in the rest
  of the thread. error_logger:error_msg is used to log the message.
  ~p specifies where to print the result so far.

  Example:
  lfe>  (-> 1 (+ 2) (case (3 (lge \"error, got three!:~p~n\")) (r r)) (+ 5))
  8

  =INFO REPORT==== 17-Jul-2016::11:36:52 ===
  result:3
  "
  ([list x tst msg]
    `(progn
      (if ,tst
        (error_logger:error_msg ,msg (list ,x)))
      ,x)))

;;---------------------------------------------------------------------
;; Utility functions
;; chop spnl
;;---------------------------------------------------------------------
(defun chop
  "Delete newline at end of string/binary."
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
  "Send all loaded modules to all connected nodes.
   Sticky modules are skipped.
  "
  (lists:map (lambda (e) (tonodes (element 1 e))) (code:all_loaded)))

(defun startpool (nodename)
  "Start a pool of nodes on every host listed in the .hosts.erlang file;
   the name of the node is specified by <nodename>."
  (pool:start nodename (++ "-loader inet -hosts "
                        "'" (getifaddr) "'"
                        " -pz /opt/lfe/ebin")))

(defun startpool()
  (startpool 'n1))

;Get first non-local running ip address
(defun getifaddr ()
   (-> (lc ((<- `#(,if ,opts) (element 2 (inet:getifaddrs)))
            (<- `#(flags ,flgs) opts)
            (lists:member 'running flgs) ;only running interfaces
            (<- `#(addr ,addr) opts)
            (=/= "lo" if)                ;skip local interfaces
            (== 4 (size addr)))          ;ip4 address only
            addr)
       hd                                ;first address
       tuple_to_list                     ;convert to string separated by dots
       (lc ((<- num @))  (integer_to_list num))
       (string:join "."))
)

(defun nocolor (str)
  (-> str
      (re:replace "\e\\[.*?m" "" '(global #(return list)))
  ))
