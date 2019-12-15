(defpackage #:cl-graph
  (:use #:cl))

(in-package :cl-graph)

(defclass graph ()
  ((%vertices :initarg :vertices
              :accessor vertices)
   (%edges    :initarg :edges
              :accessor edges)
   (%distances :initarg :dists
               :accessor distances)
   (%neighbors :initarg :nhood
               :accessor neighborhood)
   (%digraph-p :initform nil
               :initarg :digraph
               :accessor digraph-p)))

(defmethod print-object ((obj graph) stream)
  (format stream
          (concatenate
           'string
           "#<~a G = (V, E) where~%"
           "    V = ~a~%"
           "    E =~%")
          (if (digraph-p obj) "digraph" "graph")
          (vertices obj))
  (loop for value being the hash-values of (distances obj)
     using (hash-key key)
     do (format stream "~&      ~a [dist=~a]" key value))
  (princ ">" stream)
  (terpri stream))

(defun make-graph (vertices edges &optional (digraph-p nil))
  (unless digraph-p
    (setf edges
          (append edges
                  (loop for (a b dist) in edges
                     collect (list b a dist)))))
  ;; Validation
  (let ((distances (make-hash-table :test 'equal))
        (neighbors (make-hash-table)))
    (loop for edge in edges
       do (loop for vertex in (butlast edge)
             unless (member vertex vertices)
             do (error "~S is not a valid vertex"
                       vertex))
       do (setf (gethash (butlast edge) distances)
                (first (last edge))
                (gethash (first edge) neighbors)
                (cons (second edge)
                      (gethash (first edge) neighbors))))
    (make-instance 'graph
                   :vertices vertices
                   :dists distances
                   :edges edges
                   :nhood neighbors
                   :digraph digraph-p)))

(defmethod emit-dot ((graph graph) &optional (highlight-path nil))
  "Emit Graphviz code for drawing the given GRAPH."
  (let ((sep      (if (digraph-p graph) "->" "--"))
        (highlit  (loop for (a b) on highlight-path
                     unless (or (null b)
                                (digraph-p graph))
                     collect (list b a)
                     unless (null b)
                     collect (list a b)))
        (fst-node (first highlight-path))
        (lst-node (first (last highlight-path))))
    (princ (if (digraph-p graph) "digraph" "graph"))
    (princ #\Space)
    (princ "G {")
    (terpri)
    (princ "bgcolor=\"#00000000\";")
    (terpri)
    (princ (concatenate
            'string
            "graph["
            "nodesep=\"0.2\", ranksep=\"0.0\", splines=\"curved\", "
            "dpi=150, fixedsize=true];"))
    (terpri)
    (princ "node[shape=circle, fillcolor=white, style=filled];")
    (terpri)
    (when fst-node
      (format t "~a[fillcolor=yellow];~%"
              fst-node))
    (when lst-node
      (format t "~a[shape=doublecircle, fillcolor=green];~%"
              lst-node))
    (loop for value being the hash-values of (distances graph)
       using (hash-key key)
       unless (and (not (digraph-p graph))
                   (member (reverse key)
                           already-printed
                           :test 'equal))
       do (format t "~&~a ~a ~a[label=\"~a\"~a];"
                  (first key)
                  sep
                  (second key)
                  value
                  (if (member key highlit :test 'equal)
                      ", color=red, penwidth=2"
                      ""))
       collect key
       into already-printed)
    (terpri)
    (princ "}")
    (terpri)))

(defmethod show-graph ((graph graph) &optional (highlight-path nil))
  "Show graph on screen. Translates the graph structure to Graphviz,
then generates a PNG image which can be opened by feh."
  (let ((dot-text
         (with-output-to-string (s)
           (let ((*standard-output* s))
             (emit-dot graph highlight-path)))))
    (with-open-file (stream "/tmp/graph-tmp.dot"
                            :direction :output
                            :if-exists :supersede)
      (princ dot-text stream))
    #+sbcl
    (sb-ext:run-program "/usr/bin/dot"
                        '("-Ksfdp"
                          "-Tpng"
                          "/tmp/graph-tmp.dot"
                          "-o"
                          "/tmp/graph-tmp.png"))
    #+sbcl
    (sb-ext:run-program "/usr/bin/feh"
                        '("/tmp/graph-tmp.png")
                        :wait nil))
  nil)

(defmacro show-graph-with-shortest-path (graph from to)
  `(show-graph ,graph
               (shortest-path ,graph ,from ,to)))
       
(defmethod shortest-path ((graph graph) from to)
  "Dijkstra's algorithm for shortest path between two
vertices in a graph."
  ;; Error checks
  (unless (member from (vertices graph))
    (error "~S~%is not a member of~%~S" from graph))
  (unless (member to (vertices graph))
    (error "~S~%is not a member of~%~S" to graph))
  ;; Initialize variables  distances set to MAX,
  ;; and a variable for the shortest result +
  ;; shortest distance
  (let ((dists (loop for vertex in (vertices graph)
                  collect (list vertex
                                most-positive-fixnum)))
        (res-path nil))
    ;; Helper functions.
    ;;
    ;; - UNVISITED-NEIGHBORS finds all neighbors that were
    ;;   not recursively visited. The VERTEX and the list
    ;;   of recursively VISITED neighbors
    ;;   must be fed to the function.
    (labels ((unvisited-neighbors (vertex visited)
               (remove-if
                (lambda (v)
                  (member v visited))
                (gethash vertex (neighborhood graph))))
             ;; - DISTANCE is a shortcut for the distance of A and B.
             (distance (a b)
               (gethash (list a b) (distances graph)))
             ;; - TENT-DISTANCE calculates the tentative distance of
             ;;   a vertex VERT, given a list of recursively VISITED
             ;;   vertices. The calculation depends on an accumulated
             ;;   distance ACC-DIST.
             (tent-distance (vert visited acc-dist)
               ;; Stop on target arrival
               (if (eq vert to)
                   (progn
                     ;; If arrived at desired location, then check if
                     ;; this is the shortest distance found. If so,
                     ;; save that path (unless no path was found yet,
                     ;; so we'll save it anyway)
                     (when (or (null res-path)
                               (< acc-dist
                                  (second res-path)))
                       (setf res-path
                             (list (reverse (cons vert visited))
                                   acc-dist))))
                   (progn
                     (loop for nbor in (unvisited-neighbors
                                        vert visited)
                        do (let ((new-dist
                                  (+ acc-dist
                                     (distance vert nbor))))
                             ;; Update distance if needed
                             (when (< new-dist
                                      (second (assoc nbor dists)))
                               (setf (second (assoc nbor dists))
                                     new-dist))
                             ;; Recursively perform for nbors of
                             ;; current nbor. Consider current one
                             ;; visited to him
                             (tent-distance nbor
                                            (cons vert visited)
                                            new-dist)))))))
      ;; Set distance of starting node to 0
      (setf (second (assoc from dists)) 0)
      ;; Calculate tentative distance for all nodes,
      ;; given no visited nodes, accumulated distance 0,
      ;; and starting at node FROM
      (tent-distance from nil 0)
      ;; Return resulting smallest path, path distance,
      ;; and resulting distances altogether
      (values (first res-path)
              (second res-path)
              dists))))
      


(defparameter *graph1*
  (make-graph '(a b c)
              '((a b 3)
                (b c 5)
                (c a 7))))

;; (show-graph *graph1*)
;; (show-graph-with-shortest-path *graph1* 'a 'c)

(defparameter *graph2*
  (make-graph '(a b c d e f)
              '((a b 2)
                (a c 1)
                (a d 5)
                (b c 5)
                (b e 4)
                (c d 3)
                (c e 7)
                (d f 8)
                (e f 9))))

;; (show-graph *graph2*)
;; (show-graph-with-shortest-path *graph2* 'a 'f)

(defparameter *graph3*
  (make-graph '(a b c d e)
              '((a b 3)
                (a c 7)
                (a d 2)
                (a e 17)
                (b c 7)
                (b d 11)
                (b e 8)
                (c d 9)
                (c e 12)
                (d e 10))))

;; (show-graph *graph3*)
;; (show-graph-with-shortest-path *graph3* 'a 'e)

(defparameter *digraph1*
  (make-graph '(a b c d e f g)
              '((a b 37)
                (a d 5)
                (b c 97)
                (c e 68)
                (c g 10)
                (d b 16)
                (d c 26)
                (d g 43)
                (e f 58)
                (f g 99))
              t))

;; (show-graph *digraph1*)
;; (show-graph-with-shortest-path *digraph1* 'a 'g)

(defparameter *graph4*
  (make-graph '(1 2 3 4 5 6 7)
              '((1 2 4)
                (1 3 1)
                (2 3 2)
                (2 5 2)
                (3 4 4)
                (3 5 3)
                (4 5 3)
                (4 6 6)
                (5 6 3)
                (5 7 4)
                (6 7 5))))

;; (show-graph *graph4*)
;; (show-graph-with-shortest-path *graph4* 1 7)


(defparameter *graph5*
  (make-graph '(a b c d e f g h i)
              '((a b 4)
                (a h 8)
                (b c 8)
                (b h 11)
                (c d 7)
                (c f 4)
                (c i 2)
                (d e 9)
                (d f 14)
                (e f 10)
                (f g 2)
                (g h 1)
                (g i 6)
                (h i 7))))

;; (show-graph *graph5*)
;; (show-graph-with-shortest-path *graph5* 'a 'i)


(defparameter *graph6*
  (make-graph '(0 1 2 3 4 5)
              '((0 1 6)
                (0 2 1)
                (0 3 5)
                (1 2 2)
                (1 4 5)
                (2 3 2)
                (2 4 6)
                (2 5 4)
                (3 5 4)
                (4 5 3))))

;; (show-graph *graph6*)
;; (show-graph-with-shortest-path *graph6* 0 5)


(defparameter *graph7*
  (make-graph '(a b c d e f)
              '((a b 1)
                (a d 4)
                (b c 5)
                (b d 7)
                (b e 3)
                (b f 6)
                (c f 9)
                (d e 8)
                (e f 2))))

;; (show-graph *graph7*)
;; (show-graph-with-shortest-path *graph7* 'a 'e)


(defparameter *digraph2*
  (make-graph '(a b c d e f g)
              '((a b 1)
                (b c 3)
                (b d 2)
                (b e 1)
                (c d 1)
                (c e 4)
                (d a 2)
                (d e 2)
                (e f 3)
                (g d 1))
              t))

;; (show-graph *digraph2*)
;; (show-graph-with-shortest-path *digraph2* 'c 'a)


(defparameter *digraph3*
  (make-graph '(a b c d e f g h)
              '((a b 20)
                (a d 80)
                (a g 90)
                (b f 10)
                (c d 10)
                (c f 50)
                (c h 20)
                (d g 20)
                (e b 50)
                (e g 30)
                (e f 50)
                (f d 40)
                (f c 10)
                (g a 20))
              t))

;; (show-graph *digraph3*)
;; (show-graph-with-shortest-path *digraph3* 'g 'f)

(defparameter *digraph4*
  (make-graph '(start a b c d finish)
              '((start a 5)
                (start b 2)
                (a c 4)
                (a d 2)
                (b a 8)
                (b d 7)
                (c d 6)
                (c finish 3)
                (d finish 1))
              t))

;; (show-graph *digraph4*)
;; (show-graph-with-shortest-path *digraph4* 'start 'finish)


(defparameter *digraph5*
  (make-graph '(a b c d e f g h)
              '((a b 4)
                (a d 2)
                (a e 7)
                (b e 2)
                (c e 4)
                (d g 1)
                (d h 4)
                (e f 2)
                (f c 1)
                (g h 2)
                (h f 1))
              t))

;; (show-graph *digraph5*)
;; (show-graph-with-shortest-path *digraph5* 'a 'c)


(defparameter *digraph6*
  (make-graph '(s a b c d)
              '((s a 10)
                (s c 5)
                (a b 1)
                (a c 2)
                (b d 4)
                (c b 9)
                (c d 2)
                (d b 6)
                (d s 7))
              t))

;; (show-graph *digraph6*)
;; (show-graph-with-shortest-path *digraph6* 's 'd)


(defparameter *digraph7*
  (make-graph '(s a b c d)
              '((s a 2)
                (s b 7)
                (a b 3)
                (a c 8)
                (a d 5)
                (b a 2)
                (b c 1)
                (c d 4)
                (d c 5))
              t))

;; (show-graph *digraph7*)
;; (show-graph-with-shortest-path *digraph7* 's 'd)
