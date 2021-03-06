(include-lib "../include/lbx.lfe")
(include-lib "../include/mkapp.lfe")

;An exmaple of a gen_server and its api generated bu mk-genserver
;This creates the info module and the info_api module
(genserver cluster
  ((callp store (val)
     `#(reply ,val ,val))

   (castp cstore (val)
     `#(noreply ,val))

  ) ;end of api
  (print global)
)

(genserver nosend
  ((castp store (val)
     `#(noreply ,val))

  ) ;end of api
  (print global no_send_api)
)


;Anything here goes in the info module


;Tests
(defmodule mkapp-cluster-tests
 (behaviour ltest-unit)
 (export all)
 (import
  (from ltest
   (check-failed-is 2)
   (check-wrong-is-exception 2))))

(include-lib "ltest/include/ltest-macros.lfe")

;------------------------------------
; Cluster tests
;------------------------------------
(defun rmt-node ()
  (list_to_atom (++ "ctest1@" (lbx:getifaddr))))

(defun local-node ()
  (list_to_atom (++ "ctest0@" (lbx:getifaddr))))

(defun cluster-set-up ()
  (progn
    (ets:new 'clusterdb '(public named_table))
    ;Start networking and slave node
    (os:cmd "epmd -daemon")
    (? 100 'ok) ;wait for epmd
    (net_kernel:start `(,(local-node)))
    (slave:start_link (lbx:getifaddr)
                      "ctest1"
                      "-pa _build/test/lib/*/ebin/")
	 ;the next line is needed to solve the
	 ;{badarg,[{erlang,atom_to_list,[[]]}, ... error
	 (tonodes (MODULE))
    (cluster:start_link)
    (nosend:start_link)
    (global:sync)))

;; Stop slave node, clean ets
(defun cluster-tear-down (set-up-result)
  (slave:stop (rmt-node))
  (gen_server:stop #(global cluster))
  (ets:delete 'clusterdb))

(defun global-opt ()
  `(,(is-equal (rmt-node) (node))
    ,(is-equal '(cluster nosend) (global:registered_names))))

(defun no-send-opt ()
  `(,(is-equal (rmt-node) (node))
    ,(is-equal 'false (code:is_loaded 'nosend_api))))

(defun auto-send ()
  `(,(is-equal (rmt-node) (node))
    ,(is-not-equal 'false (code:is_loaded 'cluster_api))))

(defun remote_call ()
  `(,(is-equal (rmt-node) (node))
    ,(is-equal 333 (cluster_api:store 333))))

(defun remote_cast ()
  `(,(is-equal (rmt-node) (node))
    ,(progn
		 (format "^r^registered names in ~p: ~p" (list (node) (global:registered_names)))
       (is-equal 'ok (cluster_api:cstore 777))
       (is-equal '777 (sys:get_state #(global cluster))))))

(defun remote_api (set-up-result)
  "Return a lists for remote api tests"
  `(
     ,(tuple "global option" #'global-opt/0)
     ,(tuple "auto send api on init" #'auto-send/0)
     ,(tuple "no_send_api option" #'no-send-opt/0)
     ,(tuple "remote call" #'remote_call/0)
     ,(tuple "remote cast" #'remote_cast/0)
	))

(deftestgen cluster-run
  `#(setup
      ,(tuple 'spawn (rmt-node))
      ,(defsetup cluster-set-up)
      ,(defteardown cluster-tear-down)
		,#'remote_api/1
      ))

