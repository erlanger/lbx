
;*************************************************************************
;*************************************************************************
;List utility macros
;*************************************************************************
;*************************************************************************
;;---------------------------------------------------------------------
;; List utility macros
;; most workfor for tuples too
;;
;; 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th
;; take last
;;---------------------------------------------------------------------


(eval-when-compile
 (defun list-num-aux__ (num x)
   `(case (is_list ,x)
      ('true (lists:nth ,num ,x))
      ('false
        (case (is_tuple ,x)
          ('true (element ,num ,x))
          ('false #(error expected_tuple_or_list))))))
)

(defmacro 1st
  ([list x] (list-num-aux__ 1 x)))

(defmacro 2nd
  ([list x] (list-num-aux__ 2 x)))

(defmacro 3rd
  ([list x] (list-num-aux__ 3 x)))

(defmacro 4th
  ([list x] (list-num-aux__ 4 x)))

(defmacro 5th
  ([list x] (list-num-aux__ 5 x)))

(defmacro 6th
  ([list x] (list-num-aux__ 6 x)))

(defmacro 7th
  ([list x] (list-num-aux__ 7 x)))

(defmacro 8th
  ([list x] (list-num-aux__ 8 x)))

(defmacro 9th
  ([list x] (list-num-aux__ 9 x)))

(defmacro 10th
  ([list x] (list-num-aux__ 10 x)))

(defsyntax take
  ([l x]
    (lists:sublist x l)))

(defun last
  ([x] (when (is_list x))
    (lists:last x))
  ([x] (when (is_tuple x))
    (element (size x) x)))

;*************************************************************************
;*************************************************************************
;Function threading macros
;*************************************************************************
;*************************************************************************
;;---------------------------------------------------------------------
;; Function threading macros
;; and useful functions that can be used with them
;;
;; -> pr lgi lge
;;---------------------------------------------------------------------

; Execute functions threading the value from the previous step as the
; first argument. Or use @ to set the argument position.
;
; lfe> ( -> 2 math:sqrt math:sin)
; 0.9877659459927356
;
; lfe> ( -> (list "2") (io:format "received: ~p" @))
; received: "2"ok"
;
; You can also use @+ to set the argument position, in addition
; to placing the argument in the first position.
; lfe> (-> 2 (lge (== 2 @+) "error, got ~p"))
; 2
;
; =ERROR REPORT==== 19-Jul-2016::14:39:24 ===
; error, got 2
;
; It is the same as:
; lfe> (-> 2 (lge @ (== 2 @) "error, got ~p"))
;
(defmacro ->
   ([list x]
     x)
   ([list x l] (when (is_list l))
     (if (or (>= (cnt@__ '@ l) 2)
             (lists:member '@+ (lists:flatten l)))
       ;Calculate the previous result only one time
       ;if we have more than one @  substitution
       `(let ((r__ ,x))
          (,(car l) ,@(subst-1 'r__ (cdr l))))
       ;Since there are no multiple @ substitutions
       ;we don't need to store result in a variable
       `( ,(car l) ,@(subst-1 x (cdr l)))))
   ([list x y]
     `(,y ,x))
   ([list* x y  l ]
     `(-> (-> ,x ,y ) ,@l)))


(defmacro tee>
  ([list x l]
    `(let* ((r ,x)
           (r1 ,(cl:subst 'r '% l)))
      r)))

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
  ([list x tst msg]
    `(progn
      (if ,tst
        (error_logger:error_msg ,msg (list ,x)))
      ,x)))
;
; Run an intermediate function before each function call and at the end
; for example:
;
; (->m sqrt 1 f1 f2 f3)
;   becomes:
; (-> 1 sqrt f1 sqrt f2 sqrt f3 sqrt)
(defmacro m>
  ([cons m x] (when (is_list x))
    (cons '->
      (lists:append
        (lists:map (lambda (e)
                     `(,e ,m))
                   (1st x))))))

;
; Run an intermediate function before each function call but NOT at the end
; for example:
;
; (->m sqrt 1 f1 f2 f3)
;   becomes:
; (-> 1 sqrt f1 sqrt f2 sqrt f3 )
(defmacro m->
  ([cons m x] (when (is_list x))
    (cons '->
      (lists:droplast
        (lists:append
          (lists:map (lambda (e)
                       `(,e ,m))
                     (1st x)))))))

(defsyntax ->>
  ([x] x)
  ( [x ( ss ...)]
     (ss ... x))
  ( [x y]
     (y x))
  ( [ x y z ... ]
     (->> (->> x y ) z ...)))

(eval-when-compile
  (defun subst-1 (arg lst)
    (let ((r arg)
         (flst (lists:flatten lst)))
      (if (lists:member '@ flst)
        ;Substitute @ with arg, and nothing else
        (cl:subst r '@ lst)
        (if (lists:member '@+ flst)
          ;if there is a @+ (and no @) then substitute them with arg,
          ;AND add arg as the first argument also.
          (cl:subst r '@+ (cons r lst))
          ;If there is no @ or @+ simply put arg as the first argument
          (cons  arg lst)))))
  ;Count number of 'sysms' in 'lst'
  (defun cnt@__ (sym lst)
    (lists:foldl (lambda (x a)
                   (if (== x sym) (+ a 1) a))
                 0
                 (lists:flatten lst)))
)

;  Same as -> but stop processing if a function
;  returns an #(error e) tuple.
;
;  Returns: #(error e) if any function in the chain
;           returns an error tuple.
;
;           The return value of the chain if no function
;           returns an error tuple.
(defsyntax i>
   ( (( s ...) ee)
      (case (-> s ...)
         ([tuple 'error e]
            (error_logger:info_msg "   ~p~n      (~p)" (list ee e)))
         (r r))))


;*************************************************************************
;*************************************************************************
; Run external commands
;*************************************************************************
;*************************************************************************
;;---------------------------------------------------------------------
;; exec exec!
;;---------------------------------------------------------------------
(defun exec (cmd)
  "Execute external command. See exec/2 for examples and more information."
    (exec-aux__ cmd
                 '( stream exit_status use_stdio
                    stderr_to_stdout in eof binary)))

(defun exec (cmd opts)
  "Execute external command. Consider the 'shell' option if
   you want to run the source for a mini-script on the 'sh' shell.

  Example:
  > (exec \"if [ -z $DISPLAY ]; then echo underx; else echo console; fi\" '(shell))
  #(0 #\"console\n\")

  Arguments:
    cmd      A string specifying the command to execute.
             If the 'shell' option is not specified the word before
             the first whitespaces is the command to execute,
             the rest are the arguments for the command.

             if the 'shell' option is specified the whole of 'cmd'
             is sent to the shell as 'sh -c \"<cmd>\"'

    opts     See below for available options.

  Returns:
    A tuple '#(status output)', where status is an integer indicating
    the exit status and 'output' is a binary with the output of the
    command (stderr is redirected to stdio). If you want a list instead
    use the 'list' option.

  Options:
    Any of the options for open_port({spawn,...})
    can be used. In addition the following options are supported:

    shell    run command under sh -c \"<command>\"
    nocolor  strip ANSI color escape codes from output
    list     return a list instead of a binary

  See also:
    The 'sh' function intended to be used in the lfe
    shell to execute shell commands easily.
  "
    (exec-aux__ cmd
                (lists:flatten
                  (cons opts '(stream exit_status use_stdio
                     stderr_to_stdout in eof binary)))))


(defun exec-aux__ (cmd opts)
  (-> (lists:member 'shell opts)
      (if (++ "sh -c \"" cmd "\"") cmd) ;use "sh -c <cmd>" if shell option

      ;execute external command
      (tuple 'spawn @)
      (open_port (-- opts '(shell nocolor list)))
      (exec-get-data__ ())

      ;turn iolist into binary
      (tuple (element 1 @)
             (iolist_to_binary (element 2 @)))

      ;convert to list if list option
      (if (lists:member 'list opts)
        (tuple (element 1 @)
               (binary_to_list (element 2 @)))
        @)

      ;strip color escape codes if nocolor option
      (if (lists:member 'nocolor opts)
        (tuple (element 1 @)
               (lbx:nocolor (element 2 @)))
        @)))

(defun exec-get-data__ (p d)
  (receive
    ((tuple p (tuple 'data d1))
      (exec-get-data__ p (cons d1 d )))
    ((tuple p 'eof)
      (progn
        (port_close p)
        (receive
          ((tuple p (tuple 'exit_status n))
            `#(,n ,(lists:reverse d))))))))

(eval-when-compile
(defun exec!-aux__ (res)
  `(case ,res
    ([tuple 0 l]
      l)
    ([tuple n l]
      (error (tuple 'non_zero_exit_status (tuple n l))))))
)

(defmacro exec! args
  "Execute external command, calling error(...) if it fails.

   Example:
   > (exec \"if [ -z $DISPLAY ]; then echo underx; else echo console; fi\" '(shell))
   #\"console\"

   Arguments, options:
     Same as exec.

   Returns:
     A binary with the output from the command. If the exit status is non-zero it
     errors (terminating the calling process) with a tuple:
     #(non_zero_exit_status #(<exit_status> <output>))
  "
  `(lbx:chop ,(exec!-aux__ `(lbx:exec ,@args))))

(defun sh (cmd)
  "Execute shell command, print the output
   to stdout and return the exit status."
  (let ((r (exec cmd 'shell)))
    (format (element 2 r))
    (element 1 r)))

;*************************************************************************
;*************************************************************************
; Color macros
;*************************************************************************
;*************************************************************************
; Text Reset
(defmacro Crst () "\e[0m")

;# Regular colors
(defmacro Cl () "\e[0;30m")
(defmacro Cr () "\e[0;31m")
(defmacro Cg () "\e[0;32m")
(defmacro Cy () "\e[0;33m")
(defmacro Cb () "\e[0;34m")
(defmacro Cp () "\e[0;35m")
(defmacro Cc () "\e[0;36m")
(defmacro Cw () "\e[0;37m")
;
;# Bold
(defmacro CL () "\e[1;30m")
(defmacro CR () "\e[1;31m")
(defmacro CG () "\e[1;32m")
(defmacro CY () "\e[1;33m")
(defmacro CB () "\e[1;34m")
(defmacro CP () "\e[1;35m")
(defmacro CC () "\e[1;36m")
(defmacro CW () "\e[1;37m")

;# Underline
(defmacro CUl () "\e[4;30m")
(defmacro CUr () "\e[4;31m")
(defmacro CUg () "\e[4;32m")
(defmacro CUy () "\e[4;33m")
(defmacro CUb () "\e[4;34m")
(defmacro CUp () "\e[4;35m")
(defmacro CUc () "\e[4;36m")
(defmacro CUw () "\e[4;37m")

;# High Intensity
(defmacro CIl () "\e[0;90m")
(defmacro CIr () "\e[0;91m")
(defmacro CIg () "\e[0;92m")
(defmacro CIy () "\e[0;93m")
(defmacro CIb () "\e[0;94m")
(defmacro CIp () "\e[0;95m")
(defmacro CIc () "\e[0;96m")
(defmacro CIw () "\e[0;97m")

;# BoldHigh Intens
(defmacro CIL () "\e[1;90m")
(defmacro CIR () "\e[1;91m")
(defmacro CIG () "\e[1;92m")
(defmacro CIY () "\e[1;93m")
(defmacro CIB () "\e[1;94m")
(defmacro CIP () "\e[1;95m")
(defmacro CIC () "\e[1;96m")
(defmacro CIW () "\e[1;97m")
;
;# Background
(defmacro CBl () "\e[40m")
(defmacro CBr () "\e[41m")
(defmacro CBg () "\e[42m")
(defmacro CBy () "\e[43m")
(defmacro CBb () "\e[44m")
(defmacro CBp () "\e[45m")
(defmacro CBc () "\e[46m")
(defmacro CBw () "\e[47m")
;
;# High Intensity Backgrounds
(defmacro CBIl () "\e[0;100m")
(defmacro CBIr () "\e[0;101m")
(defmacro CBIg () "\e[0;102m")
(defmacro CBIy () "\e[0;103m")
(defmacro CBIb () "\e[0;104m")
(defmacro CBIp () "\e[0;105m")
(defmacro CBIc () "\e[0;106m")
(defmacro CBIw () "\e[0;107m")

(defmacro color-aux__ (string)
   `(-> ,string

       (re:replace "\\^l\\^" (Cl) '(global #(return list)))
       (re:replace "\\^r\\^" (Cr) '(global #(return list)))
       (re:replace "\\^g\\^" (Cg) '(global #(return list)))
       (re:replace "\\^y\\^" (Cy) '(global #(return list)))
       (re:replace "\\^b\\^" (Cb) '(global #(return list)))
       (re:replace "\\^p\\^" (Cp) '(global #(return list)))
       (re:replace "\\^c\\^" (Cc) '(global #(return list)))
       (re:replace "\\^w\\^" (Cw) '(global #(return list)))

       (re:replace "\\^L\\^" (CL) '(global #(return list)))
       (re:replace "\\^R\\^" (CR) '(global #(return list)))
       (re:replace "\\^G\\^" (CG) '(global #(return list)))
       (re:replace "\\^Y\\^" (CY) '(global #(return list)))
       (re:replace "\\^B\\^" (CB) '(global #(return list)))
       (re:replace "\\^P\\^" (CP) '(global #(return list)))
       (re:replace "\\^C\\^" (CC) '(global #(return list)))
       (re:replace "\\^W\\^" (CW) '(global #(return list)))

       (re:replace "\\^Ul\\^" (CUl) '(global #(return list)))
       (re:replace "\\^Ur\\^" (CUr) '(global #(return list)))
       (re:replace "\\^Ug\\^" (CUg) '(global #(return list)))
       (re:replace "\\^Uy\\^" (CUy) '(global #(return list)))
       (re:replace "\\^Ub\\^" (CUb) '(global #(return list)))
       (re:replace "\\^Up\\^" (CUp) '(global #(return list)))
       (re:replace "\\^Uc\\^" (CUc) '(global #(return list)))
       (re:replace "\\^Uw\\^" (CUw) '(global #(return list)))

       (re:replace "\\^Il\\^" (CIl) '(global #(return list)))
       (re:replace "\\^Ir\\^" (CIr) '(global #(return list)))
       (re:replace "\\^Ig\\^" (CIg) '(global #(return list)))
       (re:replace "\\^Iy\\^" (CIy) '(global #(return list)))
       (re:replace "\\^Ib\\^" (CIb) '(global #(return list)))
       (re:replace "\\^Ip\\^" (CIp) '(global #(return list)))
       (re:replace "\\^Ic\\^" (CIc) '(global #(return list)))
       (re:replace "\\^Iw\\^" (CIw) '(global #(return list)))

       (re:replace "\\^Bl\\^" (CBl) '(global #(return list)))
       (re:replace "\\^Br\\^" (CBr) '(global #(return list)))
       (re:replace "\\^Bg\\^" (CBg) '(global #(return list)))
       (re:replace "\\^By\\^" (CBy) '(global #(return list)))
       (re:replace "\\^Bb\\^" (CBb) '(global #(return list)))
       (re:replace "\\^Bp\\^" (CBp) '(global #(return list)))
       (re:replace "\\^Bc\\^" (CBc) '(global #(return list)))
       (re:replace "\\^Bw\\^" (CBw) '(global #(return list)))

       (re:replace "\\^BIl\\^" (CBIl) '(global #(return list)))
       (re:replace "\\^BIr\\^" (CBIr) '(global #(return list)))
       (re:replace "\\^BIg\\^" (CBIg) '(global #(return list)))
       (re:replace "\\^BIy\\^" (CBIy) '(global #(return list)))
       (re:replace "\\^BIb\\^" (CBIb) '(global #(return list)))
       (re:replace "\\^BIp\\^" (CBIp) '(global #(return list)))
       (re:replace "\\^BIc\\^" (CBIc) '(global #(return list)))
       (re:replace "\\^BIw\\^" (CBIw) '(global #(return list)))

       (re:replace "\\^rst\\^" (Crst) '(global #(return list)))

       ;Cursor movement up, down, left, right
       (re:replace "\\^(\\d+)up\\^" "\e[\\1A" '(global #(return list)))
       (re:replace "\\^(\\d+)dn\\^" "\e[\\1B" '(global #(return list)))
       (re:replace "\\^(\\d+)rt\\^" "\e[\\1C" '(global #(return list)))
       (re:replace "\\^(\\d+)lt\\^" "\e[\\1D" '(global #(return list)))

       ;Erase line from cursor position (included) onward
       (re:replace "\\^era\\^" "\e[K" '(global #(return list)))

       ;Clear screen
       (re:replace "\\^clr\\^" "\e[2J" '(global #(return list)))

       ;Save cursor position
       (re:replace "\\^sav\\^" "\e[s" '(global #(return list)))

       ;Load cursor poition
       (re:replace "\\^ld\\^"  "\e[u" '(global #(return list)))

       ;Set cursor position: line, column
       (re:replace "\\^(\\d+),(\\d+)pos\\^" "\e[\\1;\\2H" '(global #(return list)))

       ;Reset attributes at end of string
       (if (== 'nomatch (re:run ,string "\\^.{1,3}\\^"))
         @
         (++ @ (Crst)))
   ))

(defmacro fmt
  ([list str args]
      `(lists:flatten (io_lib:format ,str ,args)))
  ([list str]
      `(lists:flatten (io_lib:format ,str () ))))

(defun cfmt (fstr args)
  (-> (lfe_io:format1 (color-aux__ fstr) args)
      (lists:flatten)))

(defun cfmt (fstr)
  (cfmt fstr ()))

(defun format (fstr args)
  (lfe_io:format (color-aux__ fstr) args))

(defun format (fstr)
  (format "~s" (list (color-aux__ fstr))))

;*************************************************************************
;*************************************************************************
; Node/network macros
;*************************************************************************
;*************************************************************************

;This is defined as a macro so that when called there are no dependencies
;on external modules
(defmacro tonodes (module)
  "Send module to all the connected nodes."
  `(if (not (code:is_sticky ,module))
    (let (((tuple mod1 bin file)
            ;; Find object code for module Mod
            (code:get_object_code ,module)))
      ;; and load it on all nodes if not already loaded
      (rpc:multicall (nodes) 'code 'load_binary (list mod1 file bin)))))

