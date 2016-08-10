 
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
 (defun list-num-aux__ 
   ([num x] (when (is_list x)) 
     (lists:nth num x))
   ([num x] (when (is_tuple x))
     (element num x)))

(defsyntax 1st 
  ([x] (list-num-aux__ 1 x)))

(defsyntax 2nd 
  ([x] (list-num-aux__ 2 x)))

(defsyntax 3rd 
  ([x] (list-num-aux__ 3 x)))

(defsyntax 4th 
  ([x] (list-num-aux__ 4 x)))

(defsyntax 5th 
  ([x] (list-num-aux__ 5 x)))

(defsyntax 6th 
  ([x] (list-num-aux__ 6 x)))

(defsyntax 7th 
  ([x] (list-num-aux__ 7 x)))

(defsyntax 8th 
  ([x] (list-num-aux__ 8 x)))

(defsyntax 9th 
  ([x] (list-num-aux__ 9 x)))

(defsyntax 10th 
  ([x] (list-num-aux__ 10 x)))

(defsyntax take 
  ([l x]
    (lists:sublist x l)))

(defun last 
  ([x] (when (is_list x)) 
    (lists:last x))
  ([x] (when (is_tuple x))
    (element (size x)
              x)))

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
   ([x] 
     x)                               
   ([x l] (when (is_list l))
     (if (or (>= (cnt@__ '@ l) 2)
             (lists:member '@+ (lists:flatten l)))
       ;Calculate the previous result only one time
       ;if we have more than one @  substitution
       `(let ((r__ ,x)) 
          (,(car l) ,@(subst-1 'r__ (cdr l))))  
       ;Since there are no multiple @ substitutions 
       ;we don't need to store result in a variable
       `( ,(car l) ,@(subst-1 x (cdr l)))))
   ([x y] 
     `(,y ,x))
   ([ x y . l ]  
     `(-> (-> ,x ,y ) ,@l)))


(defmacro tee>
  ([list x l] 
    `(let* ((r ,x)
           (r1 ,(subst 'r '% l)))
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
  ([x tst msg]
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
        (subst r '@ lst)
        (if (lists:member '@+ flst)
          ;if there is a @+ (and no @) then substitute them with arg, 
          ;AND add arg as the first argument also.
          (subst r '@+ (cons r lst))
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
(defmacro exec
  "Execute external command."
  ([cmd] 
    `(exec-aux__ ,cmd
                 '( stream exit_status use_stdio
                    stderr_to_stdout in eof)))
  ([cmd opts] (when (is_list opts))
    `(exec-aux__ ,cmd
                 '( stream exit_status use_stdio
                    stderr_to_stdout in eof ,@opts)))
  ([cmd opt] 
    `(exec-aux__ ,cmd
                 '( stream exit_status use_stdio
                    stderr_to_stdout in eof ,opt))))

(defmacro exec!-aux__
  ([res]
    `(case ,res
      ([tuple 0 l] (when (> (length l) 1)) 
        l) 
      ([tuple 0 l] (when (== (length l) 1)) 
        (hd l))
      ([tuple n l] 
        (error (tuple 'error (tuple 'non_zero_exit_status n l))))
        )))

(defmacro exec-aux__
  ([cmd opts]
    `(-> #(spawn ,cmd) 
        (open_port ,opts) 
        (exec-get-data__ ()))))

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

(defmacro str-chop (str)
  ;We reverse to delete only the last new line
  `(lists:reverse (-- (lists:reverse ,str) (io_lib:nl))))

(defmacro exec!
  "Execute external command, calling error(...) if it fails."
  ([cmd]
    `(exec!-aux__ (exec ,cmd)))
  ([cmd opts]
    `(exec!-aux__ (exec ,cmd ,opts))))

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

(defun color-aux__ (string)
   (-> string 

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
       (++ (Crst))
   ))

(defmacro C (string)
  (color-aux__ string))

(defun format (fstr args)
  (lfe_io:format (color-aux__ fstr) args))

(defun format (fstr)
  (io:format (color-aux__ fstr) () ))
