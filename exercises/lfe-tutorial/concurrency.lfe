(defmodule concurrency
  (export all))

(defun proc (num)
  (receive
    (`#(,from ,x) (when (< x num))
     (io:format "~p: Received ~p from ~p.~n"
                (list (self) x from))
     (! from (tuple (self) (+ x 1)))
     (proc num))
    (`#(,from ,num)
     (! from (tuple (self) num))
     (io:format "Process ~p finished gracefully.~n"
                (list (self))))))

;; Starts two processes which communicate back and forth
;; n times, then terminate gracefully
(defun back-and-forth (n)
  (let ((proc1 (spawn 'concurrency 'proc (list n)))
        (proc2 (spawn 'concurrency 'proc (list n))))
    (! proc1 (tuple proc2 0))))

;; ==========================================================


(defun loopingproc (pid)
  (receive
    (x (when (> x 0))
         (io:format "~p: Pushing ~p forward to ~p~n"
                    (list (self) x pid))
         (! pid x)
         (loopingproc pid))
    (0
     (io:format "~p: Finishing gracefully.~n" (list (self)))
     (! pid 0))))

(defun loopingproc-master ()
  (io:format "~p (MASTER): Spawned~n" (list (self)))
  (receive
    (`#(,send-to ,x)
     (io:format "~p (MASTER): Pushing ~p forward to ~p~n"
                (list (self) (- x 1) send-to))
     (! send-to (- x 1))
     (loopingproc-master send-to))))

(defun loopingproc-master (send-to)
  (receive
    (x (when (> x 0))
         (io:format "~p (MASTER): Pushing ~p forward to ~p~n"
                    (list (self) (- x 1) send-to))
         (! send-to (- x 1))
         (loopingproc-master send-to))
    (0
     (io:format "~p (MASTER): Finishing everything gracefully.~n"
                (list (self))))))

(defun spawn-process-ring
  ((first n it) (when (== n it))
   (io:format "Spawning last process #~p~n" (list n))
   (spawn 'concurrency 'loopingproc (list first)))
  ((first n it)
   (io:format "Spawning process #~p~n" (list it))
   (spawn 'concurrency 'loopingproc
          (list (spawn-process-ring first n (+ it 1))))))

;; Spawns N processes and loops messages through them,
;; then finishes the processes gracefully.
;; N => amount of processes
;; M => amount of full loops through all processes
(defun process-ring (n m)
  ;; Spawn master and rest
  (let* ((master (spawn 'concurrency 'loopingproc-master '()))
         (rest   (spawn-process-ring master (- n 1) 0)))
    (io:format "Sending message~n")
    (! master (tuple rest m))))

;; =============================================================


(defun star-proc ()
  (receive
    (`#(,from ,m) (when (> m 0))
     (io:format "~p: Received message ~p from ~p~n"
                (list (self) m from))
     (star-proc))
    (`#(,from 0)
     (io:format "~p: Received last message from ~p~n"
                (list (self) from)))))

(defun star-dispatch
  ([(cons h-pid t) m]
   (! h-pid (tuple (self) m))
   (star-dispatch t m))
  (['() _] 'true))

(defun star-dispatch-all
  ([pid-list m] (when (> m 0))
   (io:format "~p: Dispatching message #~p~n"
              (list (self) m))
   (star-dispatch pid-list m)
   (star-dispatch-all pid-list (- m 1)))
  ([pid-list 0]
   (io:format "~p: Dispatching last message~n"
              (list (self)))
   (star-dispatch pid-list 0)
   (io:format "~p: Finishing gracefully.~n"
              (list (self)))))

(defun star-create-proc
  ([proc-list n] (when (> n 0))
   (star-create-proc
    (lists:append proc-list
                  (list (spawn 'concurrency 'star-proc '())))
    (- n 1)))
  ([proc-list 0] proc-list))


;; Creates a star of N processes. Another process will send a message
;; to all of them M times, and then all of them will exit gracefully.
;; N => Amount of processes
;; M => Amount of time the message is sent.
(defun process-star (n m)
  ;; Create a process list
  (io:format "Creating process list.~n")
  (let ((proc-list (star-create-proc '() n)))
    ;; Echo a message M times through the list
    (io:format "Dispatching messages ~p times decrementally~n" (list m))
    (spawn 'concurrency 'star-dispatch-all (list proc-list m))))