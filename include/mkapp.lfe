;;---------------------------------------------------------------------
;; Application  macros
;;
;;---------------------------------------------------------------------

(defmacro application
  "(application <name> <topsup> <opts>)
   (application <name> <topsup>)"
  ([list appname topsupervisor opts]
    (let ([r (application-aux appname topsupervisor opts)])
     (progn
      (if (proplists:get_bool 'print opts)
       (lfe_io:format "~p~n" (list r)))
      ;(mk-app-file (atom_to_list appname) topsupervisor)
      r)))

   ([list appname topsupervisor]
     (application-aux appname topsupervisor ())))

(eval-when-compile
  (defun application-aux (appname topsup opts)
    (++ '(progn)
      ;;Produce application module
      (list `(defmodule ,appname
              ,(proplists:get_value
                'doc
                opts
                (fmt
                  "The `~s` application was generated by lbx.
                   It has a top level supervisor named `~s`,
                   and can be stopped and started just like any
                   other application:
                   `application:ensure_all_started(~s).`"
                   (list appname topsup appname)))
              (behaviour application)
              (export (start 2) (stop 1))))

      (list `(defun start (StartType StartArgs)
              (: ,topsup start_link))

            `(defun stop (State)
              'ok))))

   ; make app file - UNUSED
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

;;init returns {ok,  #{strategy  => one_for_one|one_for_all*|rest_for_one|simple_for_one,
;;                     intensity => 5* integer() >= 0,
;;                        period => 10* integer() >= 1} , Children}
;;
;;Children= [
;;#{id       := term(),
;;  start    := {M,F,A},
;;  restart  => permanent*|transient|temporary,
;;  shutdown => brutal_kill | 5000* if worker, infinity* if supervisor Timeout:(integer or infinity),
;;  type     => worker*|supervisor,
;;  modules  => [module() M*]|dynamic }
;;

(defmacro supervisor args
  "(supervisor <name>
               (;worker child
                (worker <child-name> start #(<child-name> start_link ())
                  restart permanent shutdown 5000 modules (<child-name>))

                ;;supervisor child with default parameters
                (supervisor <child-name> <strategy> <children>)

                ;;supervisor child with parameters
                (supervisor (<child-name> start #(<child-name> start_link ())
                  restart permanent shutdown infinity modules (<child-name>)) <strategy> <children>))

                (strategy one_for_all intensity 5 period 10) ;supervisor config
                (<options>: [print])
               )"

  (case (mk-four args)
    ;;supname is an atom
    ([list supname children supconfig opts]
      (when (progn (is_list supconfig) (is_list children) (is_atom supname)))
        (let ([r (supervisor-aux supname children supconfig opts)])
         (progn
          (if (proplists:get_bool 'print opts)
           (lfe_io:format "~p~n" (list r)))
          r)))))

(eval-when-compile
  ;;return list with all elements quotes
  (defun quote-list (lst)
    (lc ((<- e lst)) `(quote ,e)))

  ;;build strategy, intensity, period map
  (defun get-supconfig (supconfig)
    `(map ,@(quote-list supconfig)))

  ;;make child entry for children array
  (defun mk-child (type name spec)
    (if (lists:member 'start spec)
     ;;start key already present
     `(map 'type ',type 'id ',name ,@(quote-list spec))
     ;;build start key
     `(map 'type ',type 'id ',name
        'start ,(tuple name 'start_link ())
        ,@(quote-list spec))))

  ;;make children array for init()
  (defun get-children (children)
    `(list ,@(lists:map (match-lambda
                 ;;worker child
                 ([(cons 'worker rest)]
                   (mk-child 'worker (hd rest) (tl rest)))

                 ;;supervisor child without child spec
                 ([(cons 'supervisor (cons name _))]
                  (when (is_atom name))
                   (mk-child 'supervisor name ()))

                 ;;supervisor child with child spec
                 ([(cons 'supervisor (cons spec-and-name _))]
                  (when (is_list spec-and-name))
                   (mk-child 'supervisor (hd spec-and-name) (tl spec-and-name)))
               )
               children)))

  ;;Make supervisor modules for children supervisors
  (defun mk-children (children opts0)
    `,(-> (lists:foldl (match-lambda
                 ;;worker child does not generate supervisor modules
                 ([(cons 'worker rest) acc]
                   acc)

                 ;;supervisor child without child spec - generate supervisor module
                 ([(list 'supervisor name children supconfig opts) acc]
                  (when (is_atom name))
                   (cons
                     `(supervisor ,name ,children ,supconfig ,opts0)
                     acc))

                 ;;supervisor child with child spec - generate module
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
      ;;Produce supervisor module
      (list `(defmodule ,supname
              ,(proplists:get_value
                'doc
                opts0
                (fmt
                  "*For internal use*. `~p`  was generated by lbx.
                   Configuration: `~p`"
                   (list supname supconfig)))
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
   The arguments in brackets are optional.
   <srvname> is a symbol. the server name
   <api> is a list of call,cast,cast-match,call-match,trigger,
         trigger-all,init and maybe one state-match
   <opts> list of global|local|gproc|#(gproc <gprockey>)
          and/or print,trigger_debug,#(api-module <modname>),noreg,multi,no_send_api
   <rst> optional. Additional function definitions"
      (let* ([(list srvname api opts rst) (mk-four (macroexpand-all args $ENV))]
             (r   (genserver-aux__ srvname api opts rst)))
        (progn
          (if (proplists:get_bool 'print opts)
            (lfe_io:format "~p~n" (list r)))
          r)))

(eval-when-compile
  ;;Take elements from a list-of-lists if the first
  ;;item of the inner list matches <match>
  (defun filter-on-1st
    ((match lst) (when (is_atom match))
      (lists:filter (lambda (e)
                      (== (hd e) match))
                    lst))
    ((match lst) (when (is_list match))
      (lists:append
        (lc ((<- m match)) (filter-on-1st m lst)))))

  ;;return true if the first element inside a lists-of-lists
  ;;matches <match>
  (defun any
    ((match lst) (when (is_atom match))
      (lists:any (lambda (e)
                   (== (hd e) match))
                 lst))
    ((match lst) (when (is_list match))
      (lists:any (lambda (e)
                   (any  e lst))
                 match)))

  ;;return true if the first element inside a lists-of-lists
  ;;matches <first> and the second element matches <second>
  (defun any-two (first second lst)
    (lists:any (lambda (e)
                 (and (== (hd e) first)
                      (== (lists:nth 2 e) second)))
               lst))

  ;;return true if the list-of-lists has
  ;;at least one defun in it: (... (defun <name> ...) ...)
  (defun any-fun (name lst)
    (any-two 'defun name lst))

  ;;produce a list with 4 elements even if less than
  ;;four are provided, pad with empty lists.
  ;;used in the genserver macro to
  ;;prevent using a large case statement.
  (defun mk-four
    ([(list one two three four)] (list one two three four))
    ([(list one two three)]      (list one two three () ))
    ([(list one two)]            (list one two () () )))

  (defun mk-five
    ([(list one two three four five)] (list one two three four five))
    ([(list one two three four)]      (list one two three four ()))
    ([(list one two three)]           (list one two three () ()))
    ([(list one two)]                 (list one two () () ())))

  ;;make the necessary body code for the handle_* functions
  ;;including trigger code if necessary.
  (defun get-body (apilst type apiname body opts)
      (if (or (any-two 'trigger apiname apilst)
              (any 'trigger-all apilst))
        (mk-trigger-code type apiname body opts)
        body))

  ;;make the trigger code, which spawns a process
  ;;which calls the run-triggers function at run-time
  ;;before returning from the handle_* function
  ;<body> is executed only once
  (defun mk-trigger-code (type apiname body opts)
    ;;execute body only once and store in variable
    `(let ((r-aux__ ,body))
      ,(if (proplists:get_bool 'spawn_trigger opts)
         ;;run trigger body in a separate process to prevent blocking
         `(spawn_link (lambda ()
                      (run-triggers__
                       (tuple
                        ',type
                        ,apiname
                        State__
                        (get-state-from-result ',type r-aux__)
                        r-aux__
                        request))))
         ;;run trigger body within handle_* function
         `(run-triggers__
            (tuple
             ',type
             ,apiname
             State__
             (get-state-from-result ',type r-aux__)
             r-aux__
             request)))
      ;;return the result from executing body
      r-aux__))

  ;;make the clauses and the body for the handle_call functions
  (defun mk-hca-clause
    ;;match-state from 'state-match
    ;;api element (in the main genserver macro call)
    ([(list call name args body) match-state api opts]
      (when (is_list args) (or (== 'call call) (== 'callp call)))
      `([(= request (tuple ',name ,@args)) From ,match-state]
           ,(get-body api 'call `',name body opts)))
    ;;match-state from 'call element
    ([(list 'call-match-1 name args match-state body) _ api opts] (when (is_list args))
      `([(= request (tuple ',name ,@args)) From (= State__ ,match-state)]
           ,(get-body api 'call `',name body opts)))
    ;;match-msg, match-from and match-state from 'call element
    ([(list 'call-match-3 match-msg match-from match-state body) _ api opts]
      `([(= request ,match-msg) ,match-from (= State__ ,match-state)]
           ,(get-body api 'call `,match-msg body opts)))
    ;; Same but with docstrings
    ([(list call name args doc body) match-state api opts]
      (when (is_list args) (or (== 'call call) (== 'callp call)))
      `([(= request (tuple ',name ,@args)) From ,match-state]
           ,(get-body api 'call `',name body opts)))
    ;;match-state from 'call element
    ([(list 'call-match-1 name args match-state doc body) _ api opts] (when (is_list args))
      `([(= request (tuple ',name ,@args)) From (= State__ ,match-state)]
           ,(get-body api 'call `',name body opts))))

  ;;Default clause for unkown call, return
  ;;#(reply #(badcall Request) State)
  (defun mk-hca-def-clause ()
    `([Request From State__]
        (tuple 'reply
          (tuple 'badcall Request)
          State__)))

  ;;make the clauses and the body for the handle_cast functions
  (defun mk-hct-clause
    ;;match-state from 'state-match
    ;;api element (in the main genserver macro call)
    ([(list cast name args body) match-state api opts]
      (when (is_list args) (or (== 'cast cast) (== 'castp cast)))
      `([(= request (tuple ',name ,@args)) ,match-state]
           ,(get-body api 'cast `',name body opts)))
    ;;match-state from 'cast element
    ([(list 'cast-match-1 name args match-state body) _ api opts] (when (is_list args))
      `([(= request (tuple ',name ,@args)) ,match-state]
           ,(get-body api 'cast `',name body opts)))
    ;;match-request and match-state from 'cast element
    ([(list 'cast-match-2 match-request match-state body) _ api opts]
        (when (and (is_list args) (is_list match-state)))
      `([(= request ,match-request) ,match-state]
           ,(get-body api 'cast `,match-request body opts)))

    ;;with doc-strings
    ([(list cast name args doc body) match-state api opts]
      (when (is_list args) (or (== 'cast cast) (== 'castp cast)))
      `([(= request (tuple ',name ,@args)) ,match-state]
           ,(get-body api 'cast `',name body opts)))
           )

  ;;Default clause for unkown cast
  ;;return #(noreply State)
  ;;and log warning
  (defun mk-hct-def-clause (srvname)
    `([Request State__]
        (progn
          (error_logger:warning_msg
            "     ~s: Unknown cast '~p'"
            (list ',srvname Request))
          (tuple 'noreply State__))))

  ;;make the clauses and the body for the handle_call functions
  (defun mk-hci-clause
    ;;match-state from 'state-match
    ;;api element (in the main genserver macro call)
    ;;match-msg from 'info element
    ([(list 'info msg body) match-state api opts]
      `([(= request ,msg) ,match-state]
           ,(get-body api 'info `,msg body opts)))
    ;;match-state from 'info element
    ([(list 'infomatch match-msg match-state body) _ api opts]
      `([(= request ,match-msg) ,match-state]
           ,(get-body api 'info `,match-msg body opts))))

  ;;Default clause for unkown info msg
  ;;return #(noreply State)
  ;;and log warning
  (defun mk-hci-def-clause (srvname)
    `([Msg State__]
        (progn
          (error_logger:warning_msg
            "     ~s: Unknown msg '~p'"
            (list ',srvname Msg))
          (tuple 'noreply State__))))

  ;;{nodeup, Node} and {nodedown, Node} messages from
  ;;(net_kernel:monitor_nodes 'true)
  (defun mk-hci-nodeup-clause (srvname opts)
    (if (proplists:get_bool 'no_monitor_nodes opts) ;add nodeup clause if needed
      ()
      (list
        `([`#(nodeup ,Node) State__]
          (progn
            (error_logger:info_msg
              "     ~s: Node ~p just connected. Sending api code."
              (list ',srvname Node))
            ,(mk-spray_api-call srvname api opts)
            (tuple 'noreply State__))))))

  (defun mk-hci-nodedown-clause (srvname opts)
    (if (proplists:get_bool 'no_monitor_nodes opts) ;add nodedown clause if needed
      ()
      (list
        `([`#(nodedown ,Node) State__]
            (progn
              (error_logger:info_msg
                "     ~s: Node ~p is down."
                (list ',srvname Node))
              (tuple 'noreply State__))))))

  ;; produce all necessary handle_call functions, e.g.:
  ;;(mk-handle_calls '((call open (door key) (+ door key))
  ;;                    (1 2 3)
  ;;                    (call close (door key) (- door key))))
  ;; produces:
  ;; (defun handle_call
  ;;    (((tuple 'open door key) From State) (+ door key))
  ;;    (((tuple 'close door key) From State) (- door key)))
  (defun mk-handle_calls (api opts)
    (if (any '(call callp call-match-1 call-match-3) api)
      (list (cons 'defun (cons 'handle_call
        (++ (lists:map (lambda (e)
                         (mk-hca-clause e (get-match-state-aux__ api) api opts))
                       (filter-on-1st '(call-match-3 call-match-1 call callp) api))
            (list (mk-hca-def-clause))))))
      (list (cons 'defun (cons 'handle_call
        (list (mk-hca-def-clause)))))
      ))

  ;; produce all necessary handle_cast functions, e.g.:
  (defun mk-handle_casts (srvname api opts)
    (if (any '(cast castp cast-match-1) api)
      (list (cons 'defun (cons 'handle_cast
        (++ (lists:map (lambda (e)
                         (mk-hct-clause e (get-match-state-aux__ api) api opts))
                       (filter-on-1st '(cast-match-1 cast castp) api))
            (list (mk-hct-def-clause srvname))))))
      (list (cons 'defun (cons 'handle_cast
        (list (mk-hct-def-clause srvname)))))
      ))

  ;; produce all necessary handle_info functions, e.g.:
  (defun mk-handle_infos (srvname api opts)
    (if (any 'info api)
      (list (cons 'defun (cons 'handle_info
        (++ (lists:map (lambda (e)
                         (mk-hci-clause e (get-match-state-aux__ api) api opts))
                       (filter-on-1st 'info api))
            (mk-hci-nodeup-clause srvname opts)
            (mk-hci-nodedown-clause srvname opts)
            (list (mk-hci-def-clause srvname))
            ))))
      (list (cons 'defun (cons 'handle_info
        (++ (mk-hci-nodeup-clause srvname opts)
            (mk-hci-nodedown-clause srvname opts)
            (list (mk-hci-def-clause srvname))))))
      ))

  ;; make a call to spray_api only if it is defined
  (defun mk-spray_api-call (srvname api opts)
    (if (and (not (proplists:get_bool 'no_send_api opts))
             (any '(callp castp) api))
      `(: ,(get-api-modname srvname opts) spray_api)
      ())
  )

  ;; make definition of init function
  (defun mk-init-aux (init-args init-body srvname api opts)
    (list 'defun 'init `,init-args
      `(progn
         ,(if (proplists:get_bool 'no_monitor_nodes opts) ;add monitor_node call if needed
            ()
            `(list (net_kernel:monitor_nodes 'true)
  				     ,(mk-spray_api-call srvname api opts)))
         ,init-body)))

  ;; produce gen_server init function
  (defun mk-init (srvname api opts)
    (list
      (if (any 'init api)
        ;;User specified init
        (let ((init-spec (hd (filter-on-1st 'init api))))
          (if (init-has-args api)
            ;;User specified init with args
            (mk-init-aux (lists:nth 2 init-spec)
                         (lists:nth 3 init-spec)
                         srvname api opts)
            ;;User specified init with no args
            (mk-init-aux '(Args)
                         (lists:nth 2 init-spec)
                         srvname api opts)))
        ;;Default init - no init provided by user
        (list 'defun 'init '(Args)
          `(progn
             ,(if (proplists:get_bool 'no_monitor_nodes opts) ;add monitor_node call if needed
                ()
                `(list (net_kernel:monitor_nodes 'true)
						     ,(mk-spray_api-call srvname api opts)))
            #(ok ()))))))

  ; produce gen_server terminate function
  (defun mk-terminate (api)
    (list
      (if (any 'terminate api)
        (list 'defun 'terminate `([reason ,(get-match-state-aux__ api)]
          ,(lists:nth 2 (hd (filter-on-1st 'terminate api)))))
        (list 'defun 'terminate '(Reason State__)
          #(ok)))))

  ;;Group calls by name and number of arguments in the call
  ;;type is 'call or 'cast; returns an orddict
  (defun grp-apicalls (type api)
    (lists:foldl (match-lambda
                   ([(list type name args body) dict]
                     (orddict:append (tuple name (length args))
                                     (list type name args body)
                                     dict))
                   ([(list type name args doc body) dict]
                     (orddict:append (tuple name (length args))
                                     (list type name args doc body)
                                     dict)))
                 (orddict:new)
                 (filter-on-1st type api)))

  ;;get extra args needed for api module functions
  ;;when start_link has extra arguments - it returns
  ;;nil if the init spec has no extra arguments, or srvr-ref__ if
  ;;it does
  (defun get-apifunc-extra-args (api srvr-ref)
    (if (== 'srvr-ref srvr-ref)
      '(srvr-ref__)
      ()))

  ;;type of api entry point, used for gen_server:call/cast ...
  (defun api-type (type)
    (case type ('callp 'call) ('castp 'cast)))

  ;;produce clauses and body for the functions that
  ;;go into the separate api module
  (defun mk-api-clauses (srvname type name arity opts dict srvr-ref)
    (lists:foldl
      (match-lambda
        ([(list type name args body) acc]
          (list (++ acc (list (++ (get-apifunc-extra-args api srvr-ref) args)
            `(: gen_server ,(api-type type) ,(get-reg-name srvname api opts srvr-ref) (tuple ',name ,@args))))))
        ([(list type name args doc body) acc]
          (cons doc (list (++ acc (list (++ (get-apifunc-extra-args api srvr-ref) args)
            `(: gen_server ,(api-type type) ,(get-reg-name srvname api opts srvr-ref) (tuple ',name ,@args))))))))
      ()
      (orddict:fetch
        (tuple name arity)
        dict)))

  ;;produce the functions that
  ;;go into the separate api module
  (defun mk-apimod-funs (srvname type api opts srvr-ref)
    (if (any type api)
      (lists:map
        (match-lambda
          ([(tuple name noargs)]
            `(defun ,name
              ,@(mk-api-clauses srvname type name noargs opts (grp-apicalls type api) srvr-ref))))
        (orddict:fetch_keys
          (grp-apicalls type api)))
      ()))

  ;;produce defun for start_link/2
  (defun mk-srvr-ref-start-link (srvname api opts)
    (if (proplists:get_bool 'multi opts)
      (list `(defun start_link (srvr-reg initarg)
        (gen_server:start_link srvr-reg ',srvname initarg () )))
      ()))

  ;; Get the args from the init specification
  ;; or empty if not specified
  (defun get-init-args (api type)
    (if (any 'init api)
      (let ((init-spec (hd (filter-on-1st 'init api))))
        (if (init-has-args api)
          ;;User-specified init argument
          (if (== 'list type)
            `,(hd (lists:nth 2 init-spec))
            (lists:nth 2 init-spec))
          ;;Empty arg if init specification had no args
          ()))
      ;;Empty arg if no init specified
      ()))

  ;;Return true if user specified args for init spec
  (defun init-has-args (api)
    (if (any 'init api)
      (-> (filter-on-1st 'init api) hd length (== 3))
      'false))

  (defun get-init-args (api)
    (get-init-args api 'formal))

  ;;produce defun for start_link/1
  (defun mk-start-link-def (srvname api opts funargs reg initarg)
    (if (!= 'undefined reg)
      (list `(defun start_link ,funargs
              (gen_server:start_link
                ,reg ',srvname ,initarg () )))
      (list `(defun start_link ,funargs
              (gen_server:start_link
                ',srvname ,initarg () )))))

  ;;produce the gen_server start_link function
  ;;honoring the gproc|public|local options
  (defun mk-start-link (srvname api opts)
    (if (any-fun 'start_link rst)
      ()
        (mk-start-link-def srvname api opts
          (get-init-args api) ;start-link func. args
          (get-reg-name srvname api  opts 'no-srvr-ref) ;reg name
          (get-init-args api 'list)) ;args passed to init
          ))

  ;;Get the pattern match for the state from
  ;;the state-match specification in the api description
  (defun get-match-state-aux__ (api)
    (if (any 'state-match api)
      `(= State__ ,(lists:nth 2 (hd (filter-on-1st 'state-match api))))
      'State__))

  ;;Get api module name from #(api-module <name>) in opts or
  ;; return <srvname>_api
  (defun get-api-modname (srvname opts)
    (let ([n (proplists:get_value 'api-module opts 'undefined)])
      (if (== 'undefined n)
        (list_to_atom (++
                        (atom_to_list srvname)
                        "_api"))
        n)))

  ;;Get the gen_server registered name to use
  ;;in gen_server:call or cast
  (defun get-reg-name (srvname api opts type)
    ;;We have three types:
    ;;  'srvr-ref       : for user specified server ref (for apimod funs and start_link)
    ;;  'no-srvr-ref    : when opts specify the registration
    ;;  'api-no-srvr-ref: same as 'no-srvr-ref but for apimod funs
    ;;                    Needed b/c gen_server:call uses <name> and start_link
    ;;                     uses #(local <name>))
    (if (== 'srvr-ref type)
      'srvr-ref__
      (if (proplists:get_bool 'global opts)
        `#(global ,srvname)
        (if (proplists:get_bool 'gproc opts)
          `#(via gproc ,`#(n l ,srvname))
          (if (!= 'undefined (proplists:get_value 'gproc opts 'undefined))
            `#(via gproc ,(proplists:get_value 'gproc opts))
            (if (proplists:get_bool 'local opts)
              ;gen_server:call != ..:start_link so we need next if
              (if (== 'api-no-srvr-ref type) `',srvname `#(local ,srvname))
              (if (or (proplists:get_bool 'noreg opts)
                      (proplists:get_bool 'multi opts))
                'undefined ;no registration
                (if (== 'api-no-srvr-ref type) ;gen_server:call != ..:start_link
                  `',srvname
                  `#(local ,srvname)))))))))

  ;;Return true if any of the server registration
  ;;options has been given or if the local option
  ;;is used by default
  (defun has-reg (opts)
    (or (proplists:is_defined 'global opts)
        (or (proplists:is_defined 'gproc opts)
            (or (proplists:is_defined 'local opts)
                (not (or (proplists:get_bool 'noreg opts)
                         (proplists:get_bool 'multi opts)))))))

  ;;extract the state from the result of executing
  ;;the user's code for each api call (i.e. from #(reply Reply State)..)
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

  ;;produce the run-triggers function; needed at run-time to process triggers.
  ;;Also generates the following utility macros for the user:
  ; - get_reply: to get the result of executing the body for
  ;              the api call that caused the trigger
  ; - get-args: to get a list of the arguments passed to the api call
  ;             that caused the trigger
  ;;Produces trigger debugging printrouts if trigger_debug is specified in the opts
  ;;for the genserver macro call
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
              (filter-on-1st '(trigger-all trigger) api))

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

  ;;Api functions without user specified server reference
  ;;are produced if server registration options
  ;;were specified (e.g. local, global, etc) or if the
  ;;local option is enabled by default.
  (defun mk-plain-apifuns? (api opts)
    (and (any '(callp castp) api)
         (has-reg opts)))

  ;;Api functions with user specified server reference are
  ;;made if the multi or noreg options were specified
  (defun mk-srvr-ref-apifuns? (api opts)
    (and (any '(callp castp) api)
         (not (has-reg opts))))

  (defun mk-export (api opts rst)
    (if (or (mk-plain-apifuns? api opts)
            (mk-srvr-ref-apifuns? api opts))
      `(export
         (spray_api 0)
         ,@(++
             (if (mk-plain-apifuns? api opts)
               (lc ((<- (tuple name noargs)
                        (++ (orddict:fetch_keys (grp-apicalls 'callp api))
                            (orddict:fetch_keys (grp-apicalls 'castp api)))))
                 `(,name ,noargs))
               ())
             (if (mk-srvr-ref-apifuns? api opts)
               (lc ((<- (tuple name noargs)
                    (++ (orddict:fetch_keys (grp-apicalls 'callp api))
                        (orddict:fetch_keys (grp-apicalls 'castp api)))))
                 `(,name ,(+ 1 noargs)))
               ())))))

  (defun mk-api-module (srvname api opts rst)
    (++
        ;;Produce api module only if there is any call or cast
        (list `(defmodule ,(get-api-modname srvname opts)
                 "This is the API module, it contains the user visible callable
                  functions, which are generally calls or casts. The global or gproc
                  option of the `genserver` macro needs to be specified in order
                  for the functions to work across nodes through a network connection."
                ,(mk-export api opts rst)
                ;(export all)
                ))

        ;;See mk-plain-apifuns? to learn when they are built
        (if (mk-plain-apifuns? api opts)
          (++ (mk-apimod-funs srvname 'callp api opts 'api-no-srvr-ref)
              (mk-apimod-funs srvname 'castp api opts 'api-no-srvr-ref))
          ())

        ;;api functions with user specified server reference
        ;;only if the init specification has arguments
        (if (mk-srvr-ref-apifuns? api opts)
          (++ (mk-apimod-funs srvname 'callp api opts 'srvr-ref)
              (mk-apimod-funs srvname 'castp api opts 'srvr-ref))
          ())

        ;;function to send api module to all connected nodes
        (if (any-fun 'spray_api rst)
          ()
          (list `(defun spray_api ()
                  "Send the code of this api module to all the connected nodes.
                   This makes it easy for a client in another node to have access
                   to the api. The generated api functions can call across the network
                   if the global or gproc options are used to register the _genserver_."
                  (tonodes ',(get-api-modname srvname opts)))))))

  ;;generates the gen_server module, its corresponding api module
  ;;and all the necessary functions.
  (defun genserver-aux__ (srvname api opts rst)
    (++ '(progn)
      ;;Produce api module only if there is any call or cast
      (if (or (mk-srvr-ref-apifuns? api opts)
              (mk-plain-apifuns? api opts))
        (mk-api-module srvname api opts rst)
        ())


      ;;Produce genserver module
      (list `(defmodule ,srvname
              ,(proplists:get_value
                'doc
                opts
                (fmt
                  "*For internal use*. `~p` is a `gen_server` generated by lbx.
                   Its functions are not meant to be called directly by the user,
                   use the api module instead."
                   (list srvname)))
              (behaviour gen_server)
              (export (code_change 3)
                      (handle_call 3)
                      (handle_cast 2)
                      (handle_info 2)
                      (init 1)
                      (terminate 2)
                      ,@(++ (list `(start_link ,(if (init-has-args api) 1 0)))
                           (if (proplists:get_bool 'multi opts)
                             (list '(start_link 2))
                             ()))
              )))

      (list '(defmacro state () 'State__))

      (mk-start-link srvname api opts)
      (mk-srvr-ref-start-link srvname api opts)

      ;;We use ++ so that if any of these functions return an empty
      ;;list they will dissapear in the final result
      ;;This also requires that the previous elements be wrapped in a
      ;;list
      (mk-init srvname api opts)
      (mk-terminate api)
      (mk-handle_calls api opts)
      (mk-handle_casts srvname api opts)
      (mk-handle_infos srvname api opts)

      ;;User-defined functions, etc.
      rst

      ;;code_change fun if it has not been specified by user in rst
      (if (any-fun 'code_change rst)
        ()
        (list `(defun code_change (oldvsn State__ Extra)
                (tuple 'ok State__))))

      ;;Used by triggers - auxiliary function
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
