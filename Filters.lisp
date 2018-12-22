;====================================
; Filter functions (low pass, median)
;
; Mikhail Malt, 2005
;====================================

(defvar FILTRES)

(defpackage "FILTRES" 
(:use "COMMON-LISP" "OpenMusic"))

(in-package "FILTRES")


;;=======================================================================
; low pas filter
;;=======================================================================

(om::defmethod! low-pass  ((data list) (window number)) 
  :initvals '('(1 2 3 4 5 6)   100 )
  :indoc '("list of data"  "window size in samples data" )
  :icon '(213) 
  :numouts 1
  :doc   "Traditional Low pass filter, where <list> is the data flow to filter and <window> 
is the parameter to calculate the window delay. The <window delay> will be (2*window + 1)"
  
  (om::x-append (om::first-n data (1- window))
                (loop for x in data
                      for i from window to (length data)
                      collect (om::om-mean (subseq data (- i window) i)))))


;;=======================================================================
; low pas filter recursive
;;=======================================================================

(om::defmethod! low-pass-rec  ((data list) (window number) (deep number)) 
  :initvals '('(1 2 3 4 5 6)   100  1)
  :indoc '("list of data"  "window size in samples data" "recursion level")
  :icon '(213) 
  :numouts 1
  :doc   "Recursive Low pass filter"
  (let ((aux data))
    
    (dotimes  (x deep aux)
      (setf aux (low-pass aux window)))))


;;=======================================================================
; fonctions auxiliaires
;;=======================================================================



(defun debut (list elem)
  (loop for x in elem
        collect (om::om-mean (om::first-n list (1+ (om::om* x 2))))))

(defun fin (list elem)
  (loop for x in elem
        collect (om::om-mean (om::first-n (reverse list) (1+ (om::om* x 2))))))

;;=======================================================================
; mean-filter
;;=======================================================================

(om::defmethod! mean-filter  ((data list) (window number) ) 
  :initvals '('(1 2 3 4 5 6)   100  1)
  :indoc '("list of data"  "window size in samples data" "recursion level")
  :icon '(213) 
  :numouts 1
  :doc   "Traditional Mean filter, where <list> is the data flow to filter and <window> 
is the parameter to calculate the effective window delay. The <window delay> will be (2*window + 1). 
We will use the average of the effective window delay"
  (let ((aux data)
        (modulo (om::om// (1+ (om::om* window 2)) 2)))
    
    
    (om::x-append (first data)
                  (debut data (om::arithm-ser 1
                                              (1- modulo)
                                              1))
                  
                  (loop for i from modulo to (om::om- (1- (length data)) modulo)
                        collect (om::om-mean 
                                 (subseq data
                                         (om::om- i modulo)
                                         (om::om+ i (1+ modulo)))))
                  (fin data (om::arithm-ser 1
                                            (1- modulo)
                                            1))
                  (om::last-elem data))))

;;=======================================================================
; mean-filter recursive
;;=======================================================================

(om::defmethod! mean-filter-rec  ((data list) (window number) (deep number)) 
  :initvals '('(1 2 3 4 5 6)   100  1)
  :indoc '("list of data"  "window size in samples data" "recursion level")
  :icon '(213) 
  :numouts 1
  :doc   "Recursive mean-filter"
  (let ((aux data))
    
    (dotimes  (x deep aux)
      (setf aux (mean-filter aux window)))))


;;=======================================================================
; median-filter
;;=======================================================================

(defun median-point (list N)
  (nth N (om::sort. list)))


(om::defmethod! median-filter  ((data list) (window number) ) 
  :initvals '('(1 2 3 4 5 6)   100  1)
  :indoc '("list of data"  "window size in samples data" "recursion level")
  :icon '(213) 
  :numouts 1
  :doc   "Traditional Median filter, where <list> is the data flow to filter and <window> 
is the parameter to calculate the window delay. The <window delay> will be (2*window + 1).
We will use the median point of the effective window delay"
  (let ((aux data)
        (modulo (om::om// (1+ (om::om* window 2)) 2)))
    
    
    (om::x-append (first data)
                  (debut data (om::arithm-ser 1
                                              (1- modulo)
                                              1))
                  
                  (loop for i from modulo to (om::om- (1- (length data)) modulo)
                        collect (median-point
                                 (subseq data
                                         (om::om- i modulo)
                                         (om::om+ i (1+ modulo))) window))
                  (fin data (om::arithm-ser 1
                                            (1- modulo)
                                            1))
                  (om::last-elem data))))

;;=======================================================================
; median-filter recursive
;;=======================================================================

(om::defmethod! median-filter-rec  ((data list) (window number) (deep number)) 
  :initvals '('(1 2 3 4 5 6)   100  1)
  :indoc '("list of data"  "window size in samples data" "recursion level")
  :icon '(213) 
  :numouts 1
  :doc   "Recursive median-filter"
  (let ((aux data))
    
    (dotimes  (x deep aux)
      (setf aux (median-filter aux window)))))





;=======================================================================
;filling lib package
;=======================================================================
(om::fill-library 
 '(("filtres" nil nil 
    (filtres::low-pass filtres::low-pass-rec filtres::median-filter filtres::median-filter-rec filtres::mean-filter filtres::mean-filter-rec ) nil)
   ("UTILS" nil nil (debut fin) nil)))

