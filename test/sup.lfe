(include-lib "../include/lfebox.lfe")
(include-lib "../include/mkapp.lfe")

(application myapp mysup (print (description "Test app")))

(supervisor mysup 
            (;(worker wrk1 restart temporary)
             (supervisor mysup1)
             (supervisor (mysup2 intensity 15 period 70))
             (supervisor mysup3 
               ((worker wrk31)
                (worker wrk32))))
            (strategy one_for_one)
            (print))

(genserver wrk31 
  ((call who () #(reply "I am wrk31~n" State)))
  (global)
)

(genserver wrk32 
  ((call who () #(reply "I am wrk32~n" State)))
  (global)
)
