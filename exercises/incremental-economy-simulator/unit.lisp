;;;; This file defines unit types and the unit class.

;;; Materials
(deftype material-type ()
  '(member
     :wood
     :brick
     :citizen
     :corn
     :food
     :hay
     :meat
     :egg
     :chicken))

;;; Units
(deftype unit-type ()
  '(member
     :homestead
     :lumberjack
     :potter
     :shelter
     :warehouse
     :ranch
     :plantation
     :barn
     :cattle
     :troop
     :farm
     :salesmen))

;;; Unit class
(defclass unit ()
  ((unittype
     :type unit-type
     :initarg :unit-type
     :initform 'nil)
   (max-num
     :type int
     :initarg :max-num
     :initform -1)
   (build-cost
      :initarg :build-cost
      :initform 'nil)
   (build-req
      :initarg :build-req
      :initform 'nil)
   (build-prize
      :initarg :build-prize
      :initform 'nil)
   (consume-cost
     :initarg :consume-cost
     :initform 'nil)
   (production
     :initarg :production
     :initform 'nil)
   (unlocks
     :initarg :unlocks
     :initform 'nil)))

;;; Inventory class
(defclass inventory ()
  ((max-storage
     :type int
     :initarg :max-storage
     :initform 1000)
   (materials
     :initarg :materials
     :initform '('(:citizen 5)))
   (units
     :initarg :units
     :initform '('(:homestead 1)))
   (coins
     :type int
     :initarg :coins
     :initform 0)
   (trades
     :initarg :trades
     :initform 'nil)))

;;; Creates an unit
(defun create-unit (createtype myinventory)
  ;; Create the unit instance
  (let ((newunit (make-instance 'unit :type createtype)))
    ;; Define values per unit
    (ecase createtype
          ;; Create a homestead (only one per game)
          ;; NOTE: I doubt we'll need this since the
          ;; inventory is initialized with a homestead
          ;; already!
          (:homestead
           (setf (slot-value newunit 'max-num) 1)
           (setf (slot-value newunit 'build-prize)
                 '('(:citizen 5))))
          ;; Create Lumberjack
          (:lumberjack
           (setf (slot-value newunit 'build-cost)
                 '('(:citizen 1)))
           (setf (slot-value newunit 'production)
                 '('(:wood 20))))
          ;; Create Potter
          (:potter
           (setf (slot-value newunit 'build-cost)
                 '('(:citizen 1)))
           (setf (slot-value newunit 'production)
                 '('(:brick 10))))
          ;; Create Shelter
          (:shelter
           (setf (slot-value newunit 'build-cost)
                 '('(:wood 100)
                   '(:brick 200)))
           (setf (slot-value newunit 'build-prize)
                 '('(:citizen 20))))
          ;; Create Warehouse
          (:warehouse
            (setf (slot-value newunit 'build-cost)
                  '('(:wood 400)
                    '(brick 800))))
          ;; Create ranch
          (:ranch
            (setf (slot-value newunit 'max-num) 1)
            (setf (slot-value newunit 'build-cost)
                  '('(:wood 400)
                    '(:brick 800)
                    '(:citizen 2)))
            (setf (slot-value newunit 'unlocks)
                  '(:plantation :barn)))
          ;; Create Plantation
          (:plantation
            (setf (slot-value newunit 'build-cost)
                  '(:wood 50))
            (setf (slot-value newunit 'production)
                  '('(:food 10)
                    '(:hay 10)))
            (setf (slot-value newunit 'unlocks)
                  '(:food :hay)))
          ;; Create Barn
          (:barn
            (setf (slot-value newunit 'build-cost)
                  '('(:wood 200)
                    '(:brick 400)
                    '(:citizen 2)))
            (setf (slot-value newunit 'production)
                  '('(:corn 10)))
            (setf (slot-value newunit 'unlocks)
                  '(:cattle :troop :farm :corn)))
          ;; Create Cattle
          (:cattle
            (setf (slot-value newunit 'consume-cost)
                  '('(:hay 10)))
            (setf (slot-value newunit 'build-req)
                  '('(:barn 1)))
            (setf (slot-value newunit 'production)
                  '('(:meat 2)))
            (setf (slot-value newunit 'unlocks)
                  '(:meat)))
          ;; Create Troop
          (:troop
            (setf (slot-value newunit 'consume-cost)
                  '('(:hay 10)))
            (setf (slot-value newunit 'build-req)
                  '('(:barn 1)))
            (setf (slot-value newunit 'unlocks)
                  '(:salesmen)))
          ;; Create Farm
          (:farm
            (setf (slot-value newunit 'build-cost)
                  '('(:wood 200)
                    '(:brick 400)
                    '(:citizen 3)))
            (setf (slot-value newunit 'consume-cost)
                  '('(:corn 10)))
            (setf (slot-value newunit 'production)
                  '('(:egg 12)
                    '(:chicken 1)))
            (setf (slot-value newunit 'unlocks)
                  '(:egg :chicken)))
          ;; Create Salesmen
          ;; TO-DO
          (:salesmen nil))
    ;; Validate the instance
    ;; TO-DO
    ))
          

;;; Unit methods

