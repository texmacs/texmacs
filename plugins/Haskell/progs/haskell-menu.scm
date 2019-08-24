
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : haskellskell-menu.scm
;; DESCRIPTION : Menu for using the Haskell Plugin
;; NEEDs       : Binary Prototype and bash-script HaskellPlugin
;; COPYRIGHT   : B.Bratschi 12 March 2018
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BASED ON    : haskell-menu.scm
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (haskell-menu))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert haskell primitive
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (haskell-cursor-pos l)
  (cond ((null? l) 0)
	((null? (cdr l)) 1)
	((and (== (car l) #\() (== (cadr l) #\))) 1)
	((and (== (car l) #\() (== (cadr l) #\,)) 1)
	((and (== (car l) #\,) (== (cadr l) #\))) 1)
	((and (== (car l) #\,) (== (cadr l) #\,)) 1)
	(else (+ (haskell-cursor-pos (cdr l)) 1))))

(define (haskell-insert s)
  (insert-go-to s (list (haskell-cursor-pos (string->list s)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; haskell-menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind haskell-menu
            
        
   (->"Control" 
      
          
          ("Ending"
                (haskell-insert ":quit"))
          ("Killing"
                (haskell-insert "kill"))
          ("(Re-)Launching"
              (haskell-insert "launch")))  
 ---
  
 
   (->"Settings" 
       (->"Debug mod"
          ("Off"
              (haskell-insert "Debug-Off")) 
          ("On"
                (haskell-insert "Debug-On")) )  )  
   
   (->"Example : steps with prime numbers"
      
         ("1) d divides n ?"
             (haskell-insert "divides d n = rem n d == 0"))
         ("2 Smallest divisor < sqrt(n) ) "
             (haskell-insert "ldf k n | divides k n = k                                                          | k^2 > n = n                                                     
         | otherwise = ldf (k+1) n"))
         
         ("3) Is n prime ?"
             (haskell-insert "primeQ n | abs n ==1 = 0                                                            | n == ldf 2 n = 1                                                         | otherwise = 0"
))
         ("4) List of primes bewteen a and b"
              (haskell-insert"listprime a b = filter (/= 0) [k*primeQ k|k<-[a..b]]"))
         ("4) Example: primes bewteen 100 and 170"
              (haskell-insert"listprime 100 170"))
   )
   )
       
    
