;====================================
; Filter functions (low pass, median)
;
; Mikhail Malt, 2005
;====================================


(in-package :om)

(compile&load (om-relative-path '("sources") "filters"))

(set-lib-release 0.3)

(fill-library 
 '(("filtres" nil nil 
    (filtres::low-pass filtres::low-pass-rec filtres::median-filter filtres::median-filter-rec filtres::mean-filter filtres::mean-filter-rec) 
    nil)
   ("UTILS" nil nil (debut fin) nil)))

