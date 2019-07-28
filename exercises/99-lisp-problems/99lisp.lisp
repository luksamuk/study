;;; p01 Find the last box of a list
(defun my-last (lst)
  (if (eq (cdr lst) nil)
      (car lst)
      (my-last (cdr lst))))

;;; p02 Find the last but one box of a list
(defun my-but-last (lst)
  (if (eq (cddr lst) nil)
      (cons (car lst)
            (if (not (eq (cadr lst) nil))
                (cons (cadr lst) nil)
                nil))
      (my-but-last (cdr lst))))

;;; p03 Find the kth element of a list, base-1
(defun element-at (lst k)
  (cond ((or (eq (car lst) nil)
             (< k 1))
         nil)
        ((= k 1)
         (car lst))
        (t (element-at (cdr lst)
                       (- k 1)))))

;;; p04 Find the number of elements on a list
(defun num-elements (lst)
  (let ((n 0))
    (loop for x in lst
       do (incf n))
    n))

;;; p05 Reverse a list
(defun rev-list (lst)
  (let ((nwlst nil))
    (loop for x in lst
       do (push x nwlst))
    nwlst))

;;; p06 Find out whether a list is a palindrome
;; So I'm gonna try to make things a little efficient here.
;; The idea is to divide the list in two, reverse the second,
;; then compare one by one until I find one or more nils.
(defun list-palindromep (lst)
  ;; Function which divides a list into a list of two half-lists
  (labels ((split-lst (lst)
             (let ((lst-mid (ceiling (num-elements lst)  2))
                   (frst-half nil))
               (loop for itr from 0 upto (+ lst-mid 1)
                  do
                    (when (< itr lst-mid)
                      (setf frst-half
                            (append frst-half
                                    (list (pop lst))))))
               (list frst-half lst))))
    ;; Fetch'em and invert the second
    (let* ((halves (split-lst lst))
           (lst1 (car halves))
           (lst2 (rev-list (cadr halves))))
      ;; Loop until nth in lst2 is nil
      (loop for itr from 0 to (num-elements lst2)
         do (let ((n1 (nth itr lst1))
                  (n2 (nth itr lst2)))
              ;; Compare each until one in lst1 remains
              (cond ((eq n2 nil)
                     (return t))
                    ;; If ever found any unequal, ret nil
                    ((not (eq n1 n2))
                     (return nil))))))))

;;; A more efficient way inspired on @RainerJoswig
(defun palindromep (list)
  (labels
      ;; First we split our list with a smart loop
      ((split-list (list)
         ;; "values" allows multiple value returns
         (values (loop repeat (ceiling (length list) 2)
                    collect (pop list)) ; Just collect the first half
                 list))) ; "list" now holds 2nd half. 1st half implicitly retd
    (multiple-value-bind (list1 list2)
        (split-list list) ; Bind results of function to list1 and list2
      (loop for v1 in list1
         and v2 in (reverse list2) ; While not reaching end of any list
           always (eql v1 v2))))) ; Ensure both atoms are equal
