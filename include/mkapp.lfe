;;---------------------------------------------------------------------
;; Application  macros
;; 
;;---------------------------------------------------------------------

(defmacro application (appname topsupervisor opts)
  "(application <name> <topsup> <opts>)"
        (let ([r (application-aux appname topsupervisor)])
         (progn 
          (if (proplists:get_bool 'print opts)
           (lfe_io:format "~p~n" (list r)))
          (mk-app-file (atom_to_list appname) topsupervisor)
          r)))

(eval-when-compile
  (defun application-aux (appname topsup)
    (++ '(progn)
      ;Produce application module
      (list `(defmodule ,appname
              (behaviour application)
              (export (start 2) (stop 1))))

      (list `(defun start (StartType StartArgs)
              (: ,topsup start_link))

            `(defun stop (State)
              'ok))))

   ; make app file
   ; .app file format:
   ;"{application, Application,
   ;    [{description,  \"Description\"},
   ;     {id,           \"Id\"},
   ;     {vsn,          Vsn},
   ;     {modules,      Modules},
   ;     {maxP,         MaxP},
   ;     {maxT,         MaxT},
   ;     {registered,   Names},
   ;     {included_applications, Apps},
   ;     {applications, [kernel, stdlib, sasl]},
   ;     {env,          Env},
   ;     {mod,          {mod, Application, []},
   ;     {start_phases, Phases},
   ;     {runtime_dependencies, RTDeps}]}."
  (defun mk-app-file (appname-str topsup)
    (-> "{application, Application,
         [{description,  \"Description\"},
          {id,           \"Id\"},
          {vsn,          \"Vsn\"},
          {mod,          {Application, []}}
          ]}."

        (re:replace "Application" appname-str '(global #(return list)))
        (re:replace "Vsn" 
		    (str-chop (os:cmd "git describe --tags --always")) '(global #(return list)))

        (file:write_file (++ appname-str ".app") (list @))))
)

; (include-lib "lbebox.lfe") is needed before including this file
;;---------------------------------------------------------------------
;; Supervisor  macros
;; 
;;---------------------------------------------------------------------

;init returns {ok,  #{strategy  => one_for_one|one_for_all*|rest_for_one|simple_for_one,
;                     intensity => 5* integer() >= 0,
;                        period => 10* integer() >= 1} , Children}

;Children= [  
;#{id       := term(),
;  start    := {M,F,A},
;  restart  => permanent*|transient|temporary,
;  shutdown => brutal_kill | 5000* if worker, infinity* if supervisor Timeout:(integer or infinity),
;  type     => worker*|supervisor,
;  modules  => [module() M*]|dynamic }
;

(defmacro supervisor args 
  "(supervisor <name> 
               (;worker child 
                (worker <child-name> start #(<child-name> start_link ()) 
                  restart permanent shutdown 5000 modules (<child-name>))

                ;supervisor child with default parameters
                (supervisor <child-name> <strategy> <children>)

                ;supervisor child with parameters
                (supervisor (<child-name> start #(<child-name> start_link ()) 
                  restart permanent shutdown infinity modules (<child-name>)) <strategy> <children>))

                (strategy one_for_all intensity 5 period 10) ;supervisor config
                (<options>: [print])
               )"

  (case (mk-four args)
    ;supname is an atom
    ([supname children supconfig opts] 
      (when (progn (is_list supconfig) (is_list children) (is_atom supname))) 
        (let ([r (supervisor-aux supname children supconfig opts)])
         (progn 
          (if (proplists:get_bool 'print opts)
           (lfe_io:format "~p~n" (list r)))
          r)))))

(eval-when-compile
  ;return list with all elements quotes
  (defun quote-list (lst)
    (lists:map (lambda (e)
                 `(quote ,e))
               lst))

  ;build strategy, intensity, period map
  (defun get-supconfig (supconfig)
    `(map ,@(quote-list supconfig)))

  ;make child entry for children array
  (defun mk-child (type name spec)
    (if (lists:member 'start spec)
     ;start key already present
     `(map 'type ',type 'id ,name ,@(quote-list spec))
     ;build start key
     `(map 'type ',type 'id ',name
        'start ,(tuple name 'start_link ()) 
        ,@(quote-list spec))))

  ;make children array for init()
  (defun get-children (children)
    `(list ,@(lists:map (match-lambda
                 ;worker child
                 ([(cons 'worker rest)]
                   (mk-child 'worker (hd rest) (tl rest)))

                 ;supervisor child without child spec
                 ([(cons 'supervisor (cons name _))]
                  (when (is_atom name))
                   (mk-child 'supervisor name ()))

                 ;supervisor child with child spec
                 ([(cons 'supervisor (cons spec-and-name _))]
                  (when (is_list spec-and-name))
                   (mk-child 'supervisor (hd spec-and-name) (tl spec-and-name)))
               )
               children)))

  ;Make supervisor modules for children supervisors
  (defun mk-children (children opts0)
    `,(-> (lists:foldl (match-lambda
                 ;worker child does not generate modules
                 ([(cons 'worker rest) acc]
                   acc)

                 ;supervisor child without child spec - generate module
                 ([(list 'supervisor name children supconfig opts) acc]
                  (when (is_atom name))
                   (cons 
                     `(supervisor ,name ,children ,supconfig ,opts0)
                     acc))

                 ;supervisor child with child spec - generate module
                 ([(list 'supervisor spec-and-name children supconfig opts) acc]
                  (when (is_list spec-and-name))
                   (cons 
                     `(supervisor ,(hd spec-and-name) ,children ,supconfig ,opts0)
                     acc)))
               ()
               (lists:map (lambda (e)
                            (mk-five e))
                          children))
          (lists:reverse)))
  

  (defun supervisor-aux (supname children supconfig opts0)
    (++ '(progn)
      ;Produce supervisor module
      (list `(defmodule ,supname
              (behaviour supervisor)
              (export (start_link 0) (init 1))))

      (list `(defun start_link ()
               (supervisor:start_link #(local ,supname) ',supname ()))

            `(defun init (Args)
               (tuple 'ok (tuple ,(get-supconfig supconfig) ,(get-children children)))))

      (mk-children children opts0)))
)

;;---------------------------------------------------------------------
;; Genserver  macros
;; 
;;---------------------------------------------------------------------

(defmacro genserver args
  "(genserver <srvname> <api> [opts] [rst])
   <srvname> is a symbol. the server name
   <api> is a list of call,cast,cast-match,call-match,trigger, trigger-all and maybe one state-match
   <opts> list of global|local|gproc|#(gproc <gprockey>)
          and/or print,trigger_debug,#(api-module <modname>)
   <rst> optional. Additional function definitions"
      (let* ([(list srvname api opts rst) (mk-four args)]
             (r   (genserver-aux__ srvname api opts rst)))
        (progn 
          (if (proplists:get_bool 'print opts)
            (lfe_io:format "~p~n" (list r)))
          r)))

(eval-when-compile
  ;Take elements from a list-of-lists if the first
  ;item of the inner list matches <match>
  (defun filter-on-1st (match lst)
    (lists:filter (lambda (e)
                    (== (hd e) match))
                  lst))

  ;return true if the first element inside a lists-of-lists
  ;matches <match>
  (defun any (match lst)
    (lists:any (lambda (e)
                 (== (hd e) match))
               lst))

  ;return true if the first element inside a lists-of-lists
  ;matches <first> and the second element matches <second>
  (defun any-two (first second lst)
    (lists:any (lambda (e)
                 (and (== (hd e) first)
                      (== (lists:nth 2 e) second)))
               lst))

  ;return true if the list-of-lists has
  ;at least one defun in it: (... (defun <name> ...) ...)
  (defun any-fun (name lst)
    (any-two 'defun name lst))

  ;produce a list with 4 elements even if less than
  ;four are provided, pad with empty lists.
  ;used in the genserver macro to 
  ;prevent using a large case statement.
  (defun mk-four
    ([(list one two three four)] (list one two three four))
    ([(list one two three)]      (list one two three () ))
    ([(list one two)]            (list one two () () )))

  (defun mk-five 
    ([(list one two three four five)] (list one two three four five))
    ([(list one two three four)]      (list one two three four ()))
    ([(list one two three)]           (list one two three () ()))
    ([(list one two)]                 (list one two () () ())))

  ;make the necessary body code for the handle_* functions
  ;including trigger code if necessary.
  (defun get-body (api type apiname body)
    (if (or (any-two 'trigger apiname api)
            (any 'trigger-all api))
      (mk-trigger-code type apiname body)
      body))

  ;make the trigger code, which spawns a process
  ;which calls the run-triggers function at run-time
  ;before returning from the handle_* function
  ;<body> is executed only once
  (defun mk-trigger-code (type apiname body)
    ;execute body only once and store in variable
    `(let ((r-aux__ ,body))
      ;run trigger body in a separate process to prevent blocking
      (spawn_link (lambda ()
                   (run-triggers__
                    (tuple 
                     ',type 
                     ,apiname 
                     State 
                     (get-state-from-result ',type r-aux__) 
                     r-aux__
                     request))))
      ;return the result from executing body
      r-aux__))

  ;make the clauses and the body for the handle_call functions
  (defun mk-hca-clause 
    ;match-state from 'state-match 
    ;api element (in the main genserver macro call)
    ([(list 'call name args body) match-state api] (when (is_list args))
      `([(= request (tuple ',name ,@args)) From ,match-state] 
           ,(get-body api 'call `',name body)))
    ;match-state from 'call element
    ([(list 'call name args match-state body) _ api] (when (is_list args))
      `([(= request (tuple ',name ,@args)) From (= State ,match-state)] 
           ,(get-body api 'call `',name body)))
    ;match-msg, match-from and match-state from 'call element
    ([(list 'call-match match-msg match-from match-state body) _ api] 
      `([(= request ,match-msg) ,match-from (= State ,match-state)] 
           ,(get-body api 'call `,match-msg body))))

  ;Default clause for unkown call, return
  ;#(reply #(badcall Request) State)
  (defun mk-hca-def-clause ()
    `([Request From State] 
        (tuple 'reply 
          (tuple 'badcall Request)
          State)))

  ;make the clauses and the body for the handle_cast functions
  (defun mk-hct-clause 
    ;match-state from 'state-match 
    ;api element (in the main genserver macro call)
    ([(list 'cast name args body) match-state api] (when (is_list args))
      `([(= request (tuple ',name ,@args)) ,match-state] 
           ,(get-body api 'cast `',name body)))
    ;match-state from 'cast element
    ([(list 'cast name args match-state body) _ api] (when (is_list args))
      `([(= request (tuple ',name ,@args)) ,match-state] 
           ,(get-body api 'cast `',name body)))
    ;match-request and match-state from 'cast element
    ([(list 'cast-match match-request match-state body) _ api] 
        (when (and (is_list args) (is_list match-state)))
      `([(= request ,match-request) ,match-state] 
           ,(get-body api 'cast `,match-request body))))

  ;Default clause for unkown cast
  ;return #(noreply State)
  ;and log warning
  (defun mk-hct-def-clause (srvname)
    `([Request State] 
        (progn
          (error_logger:warning_msg 
            "     ~s: Unknown cast '~p'" 
            (list ',srvname Request))
          (tuple 'noreply State))))

  ;make the clauses and the body for the handle_call functions
  (defun mk-hci-clause 
    ;match-state from 'state-match 
    ;api element (in the main genserver macro call)
    ;match-msg from 'info element
    ([(list 'info msg body) match-state api] 
      `([(= request ,msg) ,match-state] 
           ,(get-body api 'info `,msg body)))
    ;match-state from 'info element
    ([(list 'infomatch match-msg match-state body) _ api] 
      `([(= request ,match-msg) ,match-state] 
           ,(get-body api 'info `,match-msg body))))

  ;Default clause for unkown info msg
  ;return #(noreply State)
  ;and log warning
  (defun mk-hci-def-clause (srvname)
    `([Msg State] 
        (progn
          (error_logger:warning_msg 
            "     ~s: Unknown msg '~p'" 
            (list ',srvname Msg))
          (tuple 'noreply State))))

  ; produce all necessary handle_call functions, e.g.:
  ;(mk-handle_calls '((call open (door key) (+ door key))
  ;                    (1 2 3) 
  ;                    (call close (door key) (- door key))))
  ; produces:
  ; (defun handle_call
  ;    (((tuple 'open door key) From State) (+ door key))
  ;    (((tuple 'close door key) From State) (- door key)))
  (defun mk-handle_calls (api)
    (if (or (any 'call api) 
            (any 'call-match api))
      (list (cons 'defun (cons 'handle_call 
        (++ (lists:map (lambda (e)
                         (mk-hca-clause e (get-match-state-aux__ api) api))
                       (++ (filter-on-1st 'call-match api)
                           (filter-on-1st 'call api)))
            (list (mk-hca-def-clause))))))
      (list (cons 'defun (cons 'handle_call 
        (list (mk-hct-def-clause srvname)))))
      ))
  
  ; produce all necessary handle_cast functions, e.g.:
  (defun mk-handle_casts (srvname lst)
    (if (any 'cast lst)
      (list (cons 'defun (cons 'handle_cast 
        (++ (lists:map (lambda (e)
                         (mk-hct-clause e (get-match-state-aux__ api) api))
                       (++ (filter-on-1st 'cast-match api)
                           (filter-on-1st 'cast api)))
            (list (mk-hct-def-clause srvname))))))
      (list (cons 'defun (cons 'handle_cast 
        (list (mk-hct-def-clause srvname)))))
      ))

  ; produce all necessary handle_info functions, e.g.:
  (defun mk-handle_infos (srvname lst)
    (if (any 'info lst)
      (list (cons 'defun (cons 'handle_info 
        (++ (lists:map (lambda (e)
                         (mk-hci-clause e (get-match-state-aux__ api) api))
                       (filter-on-1st 'info lst))
            (list (mk-hci-def-clause srvname))))))
      (list (cons 'defun (cons 'handle_info 
        (list (mk-hci-def-clause srvname)))))
      ))

  ; produce gen_server init function
  (defun mk-init (api)
    (list 
      (if (any 'init api)
        (list 'defun 'init '(Args)
          (lists:nth 2 (hd (filter-on-1st 'init api))))
        (list 'defun 'init '(Args)
          '(progn 
            (spray_api)
            #(ok ()))))))

  ; produce gen_server terminate function
  (defun mk-terminate (api)
    (list 
      (if (any 'terminate api)
        (list 'defun 'terminate `([reason ,(get-match-state-aux__ api)]
          ,(lists:nth 2 (hd (filter-on-1st 'terminate api)))))
        (list 'defun 'terminate '(Reason State)
          #(ok)))))

  ;Group calls by name and number of arguments in the call
  ;type is 'call or 'cast; returns an orddict
  (defun grp-apicalls (type api)
    (lists:foldl (match-lambda 
                   ([(list type name args body) dict]
                     (orddict:append (tuple name (length args))
                                     (list type name args body)
                                     dict)))
                 (orddict:new)
                 (filter-on-1st type api)))

  ;produce clauses and body for the functions that
  ;go into the separate api module
  (defun mk-api-clauses (srvname type name arity opts dict)
    (lists:map 
      (match-lambda 
        ([(list type name args body)]
          (list args 
            `(: gen_server ,type ,(get-reg-name srvname opts) (tuple ',name ,@args)))))
      (orddict:fetch 
        (tuple name arity)
        dict))) 

  ;produce the functions that
  ;go into the separate api module
  (defun mk-apimod-funs (srvname type api opts)
    (if (any type api) 
      (lists:map 
        (match-lambda
          ([(tuple name noargs)]
            `(defun ,name 
              ,@(mk-api-clauses srvname type name noargs opts (grp-apicalls type api)))))
        (orddict:fetch_keys 
          (grp-apicalls type api)))
      ()))

  ;produce the gen_server start_link function
  ;honoring the gproc|public|local options
  (defun mk-start-link (api opts)
      (if (any-fun 'start_link rst)
        ()
        (if (proplists:get_bool 'global opts)
          (list `(defun start_link ()
                  (gen_server:start_link #(global ,srvname) ',srvname () ())))
          (if (proplists:get_bool 'gproc opts)
            (list `(defun start_link ()
                    (gen_server:start_link #(via gproc ,srvname) ',srvname () ())))
            (if (!= 'undefined (proplists:get_value 'gproc opts 'undefined))
              (list `(defun start_link ()
                      (gen_server:start_link 
                        #(via gproc ,(proplists:get_value 'gproc opts)) 
                        ',srvname () ())))
              (list `(defun start_link ()
                      (gen_server:start_link #(local ,srvname) ',srvname () ()))))))))

  ;Get the pattern match for the state from
  ;the 'state-match ;element in the api description
  (defun get-match-state-aux__ (api)
    (if (any 'state-match api)
      `(= State ,(lists:nth 2 (hd (filter-on-1st 'state-match api))))
      'State))

  ;Get api module name from #(api-module <name>) in opts or 
  ; return <srvname>_api
  (defun get-api-modname (srvname opts)
    (let ([n (proplists:get_value 'api-module opts 'undefined)])
      (if (== 'undefined n)
        (list_to_atom (++ 
                        (atom_to_list srvname) 
                        "_api"))
        n)))

  ;Get the gen_server registered name to use 
  ;in gen_server:call or cast
  (defun get-reg-name (srvname opts)
    (if (proplists:get_bool 'global opts)
      `#(global ,srvname)
      (if (proplists:get_bool 'gproc opts)
        `#(via gproc ,srvname)
        (if (!= 'undefined (proplists:get_value 'gproc opts 'undefined))
          `#(via gproc ,(proplists:get_value 'gproc opts)) 
          `',srvname))))

  ;extract the state from the result of executing 
  ;the user's code for each api call (i.e. from #(reply Reply State)..)
  (defun mk-get-state-from-result (srvname api opts)
    (if (any 'trigger api)
      (list `(defun get-state-from-result 
        (('call reply)
         ;  {reply,Reply,NewState} {reply,Reply,NewState,Timeout}
         ;  {reply,Reply,NewState,hibernate}
         ;  {noreply,NewState} 
         ;  {noreply,newstate,timeout}
         ;  {noreply,newstate,hibernate}
         ;  {stop,reason,reply,newstate}
         ;  {stop,reason,newstate}
          (case reply
            ((tuple 'reply _ state) state)
            ((tuple 'reply _ state _) state)
            ((tuple 'noreply state) state)
            ((tuple 'noreply state _) state)
            ((tuple 'stop _ state) state)
            ((tuple 'stop _ _ state) state)))
        ((type reply) (when (or (== 'info type) (== 'cast type)))
         ;  {noreply,newstate} | {noreply,newstate,timeout}
         ;  {noreply,newstate,hibernate} | {stop,reason,newstate}
          (case reply
            ((tuple 'noreply state) state)
            ((tuple 'noreply state _) state)
            ((tuple 'stop _ state) state)
            ))))
      ()))

  ;produce the run-triggers function; needed at run-time to process triggers.
  ;Also generates the following utility macros for the user:
  ; - get_reply: to get the result of executing the body for 
  ;              the api call that caused the trigger
  ; - get-args: to get a list of the arguments passed to the api call
  ;             that caused the trigger
  ;Produces trigger debugging printrouts if trigger_debug is specified in the opts
  ;for the genserver macro call
  ;(defun run-triggers
  ;  ((type old-state-match new-state-match body-result call-args) {{(when guard)}}
  ;    ...)
  ;  ((type apiname old-state-match1 new-state-match1 body-result call-args) {{(when guard)}} 
  ;    ...)
  ;)
  (defun mk-trigger-aux (srvname api opts)
    (if (any 'trigger api) 
      (list 
        `(defmacro print-if-trigger-debug__ (a) 
           ,(if (proplists:get_bool 'trigger_debug opts)
              ``(format (++ "^Ur^triggered:~n^rst^"
                           "  ^r^type       : ~p~n"
                           "  apiname    : ~p~n"
                           "  prev. state: ~p~n"
                           "  new   state: ~p~n"
                           "       result: ~p~n"
                           "      request: ~p~n")
                 (list (element 1 ,a) 
                       (element 2 ,a) 
                       (element 3 ,a) 
                       (element 4 ,a) 
                       (element 5 ,a) 
                       (element 6 ,a)))
              ()))

        '(defmacro get-reply () 'body-result__)
        `(defmacro get-args ()
           '(case type__
             ('call (tl (tuple_to_list call-args__)))
             ('cast (tl (tuple_to_list call-args__)))
             (_  call-args__)))

        `(defun run-triggers__ 
          ,@(lists:map 
              (match-lambda
                ([(list 'trigger-all prev-state-match new-state-match trigger-body)]
                  `(((= (tuple type__ apiname ,prev-state-match ,new-state-match body-result__ call-args__) all))
                    (progn (print-if-trigger-debug__ all) ,trigger-body)))

                ([(list 'trigger-all prev-state-match new-state-match guard trigger-body)]
                  `(((= (tuple type__ apiname ,prev-state-match ,new-state-match body-result__ call-args__) all)) 
                      (when ,guard)
                    (progn (print-if-trigger-debug__ all) ,trigger-body)))

                ([(list 'trigger apiname-match prev-state-match new-state-match trigger-body)]
                  `(((= (tuple type__ ,apiname-match ,prev-state-match ,new-state-match body-result__ call-args__) all))
                    (progn (print-if-trigger-debug__ all) ,trigger-body)))

                ([(list 'trigger apiname-match prev-state-match new-state-match guard trigger-body)]
                  `(((= (tuple type__ ,apiname-match ,prev-state-match ,new-state-match body-result__ call-args__) all)) 
                      (when ,guard)
                    (progn (print-if-trigger-debug__ all) ,trigger-body))))
              (++ (filter-on-1st 'trigger-all api) 
                  (filter-on-1st 'trigger api)))
          
          (((=(tuple a b c d e f) all))
            ,(if (proplists:get_bool 'trigger_debug opts)
              `(format (++ "^Ub^not triggered:~n^rst^"
                           "  ^b^type       : ~p~n"
                           "  apiname    : ~p~n"
                           "  prev. state: ~p~n"
                           "  new   state: ~p~n"
                           "       result: ~p~n"
                           "      request: ~p~n")
                       (list a b c d e f))
              ())))
      )
      ()))
  
  (defun mk-export (api opts rst)
    (if (or (any 'call api) 
            (any 'cast api))
      `(export
        ,@(lists:map 
          (match-lambda
           ([(tuple name noargs)]
            `(,name ,noargs)))
          (++ (orddict:fetch_keys (grp-apicalls 'call api))
              (orddict:fetch_keys (grp-apicalls 'cast api)))))
      ()))

  ;generates the gen_server module, its corresponding api module
  ;and all the necessary functions.
  (defun genserver-aux__ (srvname api opts rst)
    (++ '(progn)
      ;Produce api module
      (list `(defmodule ,(get-api-modname srvname opts) 
              ,(mk-export api opts rst)
              ;(export all)
              ))
      (mk-apimod-funs srvname 'call api opts)
      (mk-apimod-funs srvname 'cast api opts)

      ;Produce genserver module
      (list `(defmodule ,srvname 
              (behaviour gen_server)
              (export (code_change 3)
                      (handle_call 3)
                      (handle_cast 2)
                      (handle_info 2)
                      (init 1)
                      (terminate 2)
                      (start_link 0)
                      (spray_api 0)
              )))
 
      (mk-start-link api opts)
 
      ;We use ++ so that if any of these functions return an empty
      ;list they will dissapear in the final result
      ;This also requires that the previous elements be wrapped in a
      ;list
      (mk-init api)
      (mk-terminate api)
      (mk-handle_calls api)
      (mk-handle_casts srvname api)
      (mk-handle_infos srvname api)
  
      ;User-defined functions, etc.
      rst

      ;code_change fun if it has not been specified by user in rst
      (if (any-fun 'code_change rst)
        ()
        (list `(defun code_change (oldvsn State Extra)
                (tuple 'ok State))))

      (if (any-fun 'spray_api rst)
        ()
        (list `(defun spray_api ()
                (lbx:tonodes ',(get-api-modname srvname opts)))))

      ;Used by triggers - auxiliary function
      (mk-get-state-from-result srvname api opts)
      (mk-trigger-aux srvname api opts)
      
      ))
)

;;---------------------------------------------------------------------
;; supervisor  macros
;; 
;;---------------------------------------------------------------------
;(defmacro supervisor arg
;  ([]
;  ))
;
;(eval-when-compile
;  (defun genserver-aux__ (srvname api rst)
;    (++ '(progn)
;      (list `(defmodule ,srvname 
;              (behaviour gen_server)
;              (export all)))))
;              
;              
;              
;)