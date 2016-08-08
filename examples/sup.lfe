(include-lib "../include/lfebox.lfe")
(include-lib "../include/mkapp.lfe")

(supervisor mysup 
            (;(worker wrk1 restart temporary)
             (supervisor mysup1 () () (print))
             ;(supervisor mysup1)
             (supervisor (mysup2 intensity 15 period 70))
             ;(supervisor (mysup2 intensity 15 period 70))
             (supervisor mysup3 
               ((worker wrk31)
                (worker wrk32)))
            )
            (strategy one_for_one)
            (print))
