;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname TuringMachine) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;;-----------------------------------USAGE--------------------------------------
;; (start TURING-PROGRAM INPUT) will start the simulation.
;; Press any key to perform the next move in the simulation.
;;
;; INPUT is a scheme list of symbols/strings/numbers which the program will
;; run on.
;;
;; TURING-PROGRAM must specify the list of all states with transitions. The
;; start state is the first state in the list.
;;
;; Sample Turing Program:
;;(make-tp
;;  '(; STATE1, the only state used for this turing program
;;      (q1 (0 0 R q1)
;;          (_ _ R accept)
;;          (* * R reject)))
;;   '(q1 (0 0 R q1)
;;        (_ _ R accept)
;;        (* * R reject)))
;;
;; For more information, read Definition of a TuringProgram.
;;
;; This Software is distributed under the MIT License (see License.txt)
;;
;;------------------------------------------------------------------------------

;;------------------------Definition of a TuringProgram-------------------------

;; A TuringProgram is one of:
;; - (cons State empty)
;; - (cons State TuringProgram)
;; i.e. a TuringProgram is a non-empty list of states

;; A State is one of
;; - 'accept
;; - 'reject
;; - (cons Symbol [List-of Transition])
;;     Interpretation:
;;       The Symbol is the name of the state
;;       [List-of Transition] is the transition function for the State.

;; A Transition is a (list Alphabet Alphabet Direction Symbol)
;; Interpretation:
;;   The first Alphabet is the symbol read from the tape at head location
;;   The second Alphabet is the symbol written to the tape at head location
;;   The Direction is the direction the head is moved in
;;   The Symbol is the name for the new state for the Turing Machine

;; An Alphabet is one of:
;; - '*
;; - '_
;; - Symbol
;; - Number
;; - String

;; A Direction is one of:
;; - 'left or 'L
;; - 'right or 'R


;; Examples
(define S1-TRANSITION1 '(0 0 R q1))
(define S1-TRANSITION2 '(_ _ R accept))
(define S1-TRANSITION3 '(* * R reject))

(define LOT1 (list S1-TRANSITION1 S1-TRANSITION2 S1-TRANSITION3))

(define STATE1 (cons 'q1 LOT1))

(define PROGRAM1 (list STATE1))

;; Program 1, rewritten for better visibiity as Program 2
(define PROGRAM2
  '(; STATE1, the only state used for this turing machine
    (q1 (0 0 R q1)
        (_ _ R accept)
        (* * R reject))

    ; Dummy State, not used
    (q0 (* * R reject))))

;; Predicates

;; Any -> Boolean
;; Is this a State?

(check-expect (state? 1) #false)
(check-expect (state? 'a) #false)
(check-expect (state? "wololo") #false)
(check-expect (state? STATE1) #true)
(check-expect (state? '(q1 (0 0 3))) #false)
(check-expect (state? '((0 0 3) (1 1 2))) #false)

(define (state? x)
  (or (and (symbol? x) (symbol=? x 'accept))
      (and (symbol? x) (symbol=? x 'reject))
      (and (cons? x) (symbol? (first x))
           (andmap transition? (rest x)))))

;; Any -> Boolean
;; Is this a Transition?

(check-expect (transition? 1) #false)
(check-expect (transition? 'a) #false)
(check-expect (transition? "wololo") #false)
(check-expect (transition? '(0 0 3)) #false)
(check-expect (transition? '(a b c d)) #false)
(check-expect (transition? '(a b left d)) #true)

(define (transition? x)
  (and (list? x) (= (length x) 4)
       (alphabet? (first x))
       (alphabet? (second x))
       (direction? (third x))
       (symbol? (fourth x))))

;; Any -> Boolean
;; Is this an alphabet?

(check-expect (alphabet? '(a b left d)) #false)
(check-expect (alphabet? '()) #false)
(check-expect (alphabet? 'symbols) #true)
(check-expect (alphabet? '*) #true)
(check-expect (alphabet? "strings") #true)
(check-expect (alphabet? 123) #true)

(define (alphabet? x)
  (or (symbol? x) (number? x) (string? x)))

;; Any -> Boolean
;; Is this a blank?

(check-expect (blank? 1) #false)
(check-expect (blank? "string") #false)
(check-expect (blank? 'symbol) #false)
(check-expect (blank? '_) #true)

(define (blank? x)
  (and (symbol? x) (symbol=? x '_)))

;; Alphabet Alphabet -> Boolean
;; Are these alphabets equal?

(check-expect (alphabet=? 'a 'a) #true)
(check-expect (alphabet=? 'a 'apple) #false)
(check-expect (alphabet=? 'a "a") #false)
(check-expect (alphabet=? 'a 1) #false)


(define (alphabet=? x y)
  (or (and (symbol? x) (symbol? y) (symbol=? x y))
      (and (number? x) (number? y) (= x y))
      (and (string? x) (string? y) (string=? x y))
      (star? x) (star? y)))

(check-expect (alphabet=? '* "string") #true)
(check-expect (alphabet=? '* 'symbol) #true)
(check-expect (alphabet=? '* 1) #true)
(check-expect (alphabet=? "string" '*) #true)
(check-expect (alphabet=? 'symbol '*) #true)
(check-expect (alphabet=? 1 '*) #true)
(check-expect (alphabet=? 1 1) #true)
(check-expect (alphabet=? 1 12) #false)
(check-expect (alphabet=? 1 "1") #false)
(check-expect (alphabet=? "apple" "apple") #true)

;; Any -> Boolean
;; Is this a wildcard?

(check-expect (star? '*) #true)
(check-expect (star? 'star) #false)
(check-expect (star? "string") #false)
(check-expect (star? 123) #false)

(define (star? x)
  (and (symbol? x) (symbol=? x '*)))

;; Any -> Boolean
;; Is this a direction?

(check-expect (direction? 'left) #true)
(check-expect (direction? 'right) #true)
(check-expect (direction? 'L) #true)
(check-expect (direction? 'R) #true)
(check-expect (direction? "left") #false)
(check-expect (direction? "right") #false)
(check-expect (direction? 'symbol) #false)
(check-expect (direction? 9) #false)

(define (direction? x)
  (or (left? x) (right? x)))

;; Any -> Boolean
;; Is this a left direction?

(define (left? x)
  (or (and (symbol? x) (symbol=? x 'L))
      (and (symbol? x) (symbol=? x 'left))))

;; Any -> Boolean
;; Is this a right direction?

(define (right? x)
  (or (and (symbol? x) (symbol=? x 'R))
      (and (symbol? x) (symbol=? x 'right))))

;;------------------------------------------------------------------------------

;;------------------------Definition of a TuringMachine-------------------------

;; A TuringMachine is a (make-tm TuringTape State [List-of State])
(define-struct tm [tape cur-state states])
;; Interpretation:
;;   tape is the alphabets on the Turing Tape and the read/write head location
;;   cur-state is the current state of the TuringMachine
;;   states is the List of States from a TuringProgram for this TuringMachine

;; A TuringTape is a (make-tape [NEList-of Alphabet] [NEList-of Alphabet])
(define-struct tape [left right])
;; Interpretation:
;;   left are all the Alphabets to the left of the head
;;   (first left) is the alphabet directly to the left of the head
;;   right are all the Alphabets to the right of the head
;;   (first right) is the alphabet that the head is currently pointing at

;; A SafeAlphabet is an Alphabet that is not:
;; - '*
;; - '_

;; An Input is a [List-of SafeAlphabet]

(define TAPE1 (make-tape '(_) '(0 0 0 0 0 0 0 0)))

(define TURING-MACHINE1 (make-tm TAPE1 STATE1 PROGRAM1))

;;----------------------------------CONSTANTS-----------------------------------

(define FONT-SIZE 20)


;;------------------------------------------------------------------------------

;; TuringProgram Input -> TuringMachine
;; Simulates a TuringMachine running the given program on given input
(define (start program input)
  (big-bang (make-tm (make-tape '(_) input)
                     (first program)
                     (cons 'accept (cons 'reject program)))
            [on-key handle-key]
            [to-draw draw-world]))

;;-----------------------------------TO DRAW------------------------------------

(define SPACE (text "        " FONT-SIZE "black"))

;; TuringMachine -> Image
;; Draws the TuringMachine and its state.

(check-expect (draw-world TURING-MACHINE1)
              (above (draw-state STATE1)
                     (beside SPACE (draw-machine TURING-MACHINE1) SPACE)))

(define (draw-world a-tm)
  (above (draw-state (tm-cur-state a-tm))
         (beside SPACE (draw-machine a-tm) SPACE)))

;; State -> Image
;; Draws the state

(check-expect (draw-state STATE1) (text "q1" FONT-SIZE "black"))

(define (draw-state a-state)
  (text (state->string a-state) FONT-SIZE "black"))

;; State -> String
;; Get the name of a State

(check-expect (state->string STATE1) "q1")
(check-expect (state->string '(1 (* * R reject))) "1")
(check-expect (state->string 'accept) "accept")
(check-expect (state->string 'reject) "reject")

(define (state->string a-state)
  (cond [(symbol? a-state) (symbol->string a-state)]
        [(and (cons? a-state ) (symbol? (first a-state)))
         (symbol->string (first a-state))]
        [(and (cons? a-state) (number? (first a-state)))
         (number->string (first a-state))]))

;; TuringMachine -> Image
;; Draws the TuringMachine.

(check-expect (draw-machine TURING-MACHINE1)
              (draw-tape TAPE1))

(define (draw-machine a-tm)
  (draw-tape (tm-tape a-tm)))

;; Tape -> Image
;; Draws the tape.

(check-expect (draw-tape TAPE1)
              (beside (draw-alphabets '(_))
                      (draw-head 0)
                      (draw-alphabets '(0 0 0 0 0 0 0))))

(define (draw-tape tape)
  (beside (draw-alphabets (reverse (tape-left tape)))
          (draw-head (first (tape-right tape)))
          (draw-alphabets (rest (tape-right tape)))))

;; [List-of Alphabet] -> Image
;; Draws a list of Alphabets.

(check-expect (draw-alphabets '(a b c d 1 2 3))
              (foldr beside empty-image
                     (map draw-framed-alphabet '(a b c d 1 2 3))))

(define (draw-alphabets list)
  (foldr beside empty-image
         (map draw-framed-alphabet
              (remove-terminal-blanks list))))

;; [List-of Alphabet] -> [List-of Alphabet]
;; Remove terminal '_ alphabet from the list of alphabets.
(check-expect (remove-terminal-blanks '(_)) '())
(check-expect (remove-terminal-blanks '(a b c _)) '(a b c))
(check-expect (remove-terminal-blanks '(a b _ c _)) '(a b _ c))
(check-expect (remove-terminal-blanks '(a b _ c _ _)) '(a b _ c))

(define (remove-terminal-blanks list)
  (cond [(empty? list) empty]
        [(cons? list)
         (local [(define no-blank-rest
                   (remove-terminal-blanks (rest list)))]
         (if (and (empty? no-blank-rest) (blank? (first list)))
             empty
             (cons (first list) no-blank-rest)))]))

;; Alphabet -> Image
;; Draws an Alphabet with a green frame.

(check-expect (draw-head 'a) (draw-frame 'red 1 (draw-alphabet 'a)))

(define (draw-head alphabet)
  (draw-frame 'red 1 (draw-alphabet alphabet)))

;; Alphabet -> Image
;; Draws an Alphabet with a frame.

(check-expect (draw-framed-alphabet 'a)
              (draw-frame 'black 1 (draw-alphabet 'a)))

(define (draw-framed-alphabet alphabet)
  (draw-frame 'black 1 (draw-alphabet alphabet)))

;; Color Number Image -> Image
;; Draws a frame of given color with given thickness (in pixels)
;; around the given image.

(check-expect (draw-frame 'red 1 (rectangle 10 10 "solid" "blue"))
              (overlay (rectangle 10 10 "solid" "blue")
                       (rectangle 12 12 "outline" "red")))

(define (draw-frame color frame-size image)
  (local [(define w (+ (image-width image) (* 2 frame-size)))
          (define h (+ (image-height image) (* 2 frame-size)))]
    (overlay image
             (rectangle w h "outline" color))))

;; Alphabet -> Image
;; Draws an Alphabet.

(check-expect (draw-alphabet 'a) (text "a" FONT-SIZE "black"))
(check-expect (draw-alphabet 'more) (text "more" FONT-SIZE "black"))
(check-expect (draw-alphabet 5) (text "5" FONT-SIZE "black"))
(check-expect (draw-alphabet -15) (text "-15" FONT-SIZE "black"))
(check-expect (draw-alphabet "omega") (text "omega" FONT-SIZE "black"))

(define (draw-alphabet alphabet)
  (cond [(symbol? alphabet)
         (text (symbol->string alphabet) FONT-SIZE "black")]
        [(number? alphabet)
         (text (number->string alphabet) FONT-SIZE "black")]
        [(string? alphabet)
         (text alphabet FONT-SIZE "black")]))


;;------------------------------------------------------------------------------
;;-----------------------------------ON KEY-------------------------------------

;; TuringMachine Key -> TuringMachine
;; Move to the next step in evaluation.

(check-expect (handle-key (make-tm TAPE1 STATE1 (list STATE1)) "left")
              (make-tm (make-tape '(0) (rest (tape-right TAPE1)))
                       STATE1 (list STATE1)))

(define (handle-key tm a-key)
  (next-step (tm-tape tm)
             (tm-cur-state tm)
             (tm-states tm)))

;; Tape State [List-of States] -> TuringMachine
;; Move to the next step in evaluation.

(check-expect (next-step TAPE1 STATE1 (list STATE1))
              (make-tm (make-tape '(0) (rest (tape-right TAPE1)))
                       STATE1 (list STATE1)))
(check-expect (next-step (make-tape '(0) '(0)) STATE1 (list STATE1))
              (make-tm (make-tape '(0 0) '(_))
                       STATE1 (list STATE1)))

(define (next-step tape cur-state states)
  (local [;; The transition to make
          (define transition (get-transition tape cur-state))]
    (make-tm (update-tape tape transition)
             (update-state transition cur-state states)
             states)))

;; Tape State -> [Maybe Transition]
;; Selects the transition to make based on current alphabet or #false
;; if no transition can be made.

(check-expect (get-transition (make-tape '(_) '(0 0 0 0 0 )) STATE1)
              S1-TRANSITION1)
(check-expect (get-transition (make-tape '(0 0 0 0 0 0) '(_)) STATE1)
              S1-TRANSITION2)
(check-expect (get-transition (make-tape '(0 0 0 0 0 0) '(a)) STATE1)
              S1-TRANSITION3)
(check-expect (get-transition (make-tape '(a b c d e) '(f g h)) 'accept)
              #false)

(define (get-transition tape cur-state)
  (cond [(symbol? cur-state) #false]
        [else (get-uds-transition (get-alphabet tape)
                                  (rest cur-state))]))

;; Tape -> Alphabet
;; Read the alphabet on the tape pointed at by head

(check-expect (get-alphabet (make-tape '(b a) '(c d))) 'c)

(define (get-alphabet tape)
  (first (tape-right tape)))

;; Alphabet [List-of Transition] -> Transition
;; Selects the transition to make based on current alphabet.

(check-error (get-uds-transition 'a empty) "behavior not defined")
(check-expect (get-uds-transition '0 LOT1) S1-TRANSITION1)
(check-expect (get-uds-transition '_ LOT1) S1-TRANSITION2)
(check-expect (get-uds-transition 'i LOT1) S1-TRANSITION3)

(define (get-uds-transition alphabet a-lot)
  (cond [(empty? a-lot) (error "behavior not defined")]
        [(cons? a-lot)
         (if (alphabet=? alphabet (first (first a-lot)))
             (first a-lot)
             (get-uds-transition alphabet (rest a-lot)))]))

;; Tape [Maybe Transition] -> Tape
;; Update the Tape based on the current state.

(check-expect (update-tape (make-tape '(b a) '(c d)) #false)
              (make-tape '(b a) '(c d)))
(check-expect (update-tape (make-tape '(b a) '(c d)) '(c z R q1))
              (make-tape '(z b a) '(d)))

(define (update-tape tape transition)
  (cond [(false? transition) tape]
        [(cons? transition)
         (move-tape (third transition)
                    (write-tape (get-element tape transition) tape))]))

;; Direction Tape -> Tape
;; Moves the tape in the given direction

(check-expect (move-tape 'left (make-tape '(b a) '(c d)))
              (make-tape '(a) '(b c d)))
(check-expect (move-tape 'left (make-tape '(a) '(b c d)))
              (make-tape '(_) '(a b c d)))
(check-expect (move-tape 'right (make-tape '(b a) '(c d)))
              (make-tape '(c b a) '(d)))
(check-expect (move-tape 'right (make-tape '(c b a) '(d)))
              (make-tape '(d c b a) '(_)))

(define (move-tape dir tape)
  (local [(define lt (tape-left tape))
          (define rt (tape-right tape))]
    (cond [(left? dir) (make-tape (ne-rest lt) (ne-cons (first lt) rt))]
          [(right? dir) (make-tape (ne-cons (first rt) lt) (ne-rest rt))])))

;; [NEList-of Alphabet] -> [NEList-of Alphabet]
;; Returns the rest of the tape on given half of tape

(check-expect (ne-rest (list 'a)) (list '_))
(check-expect (ne-rest (list 'a 'b 'c)) (list 'b 'c))

(define (ne-rest nelist)
  (cond [(empty? (rest nelist)) (list '_)]
        [(cons? (rest nelist)) (rest nelist)]))

;; Alphabet [NEList-of Alphabet] -> [NEList-of Alphabet]
;; Adds a character to a half of a tape and removes terminal '_

(check-expect (ne-cons 'a '(_)) '(a))
(check-expect (ne-cons 'a '(_ a)) '(a _ a))
(check-expect (ne-cons 'a '(a)) '(a a))

(define (ne-cons alphabet nelist)
  (cond [(and (empty? (rest nelist)) (alphabet=? '_ (first nelist)))
         (cons alphabet empty)]
        [else (cons alphabet nelist)]))

;; Tape Transition -> Alphabet
;; Get the element to write to the tape from the transition and the tape

(check-expect (get-element (make-tape '(b a) '(c d)) '(* * R q1)) 'c)
(check-expect (get-element (make-tape '(b a) '(c d)) '(* X R q1)) 'X)

(define (get-element tape transition)
  (if (star? (second transition))
      (get-alphabet tape)
      (second transition)))

;; Alphabet Tape -> Tape
;; Writes the given alphabet to the tape

(check-expect (write-tape 'a (make-tape '(a b) '(c d)))
              (make-tape '(a b) '(a d)))

(define (write-tape alphabet tape)
  (make-tape (tape-left tape)
             (cons alphabet (rest (tape-right tape)))))

;; [Maybe Transition] State [List-of State] -> State
;; Decide which state to go in based on given Transition.

(check-expect (update-state #false 'accept '()) 'accept)
(check-expect (update-state '(0 0 0 q1) 'accept (list STATE1)) STATE1)

(define (update-state transition cur-state states)
  (cond [(false? transition) cur-state]
        [else (get-state (fourth transition) states)]))

;; Symbol [List-of States] -> State
;; Returns the state with given Symbol as name.
;; Returns the 'reject state if no state matches name.

(check-expect (get-state 'q1 (list STATE1)) STATE1)
(check-error (get-state 'wololo (list STATE1)) "got no match for:wololo")
(check-error (get-state 'accept (list )) "got no match for:accept")
(check-expect (get-state 'accept (list 'accept)) 'accept)

(define (get-state name los)
  (cond [(empty? los) (error (string-append "got no match for:"
                                            (symbol->string name)))]
        [(cons? los) (if (state-has-name (first los) name)
                         (first los)
                         (get-state name (rest los)))]))

;; State Symbol -> Boolean
;; Does the given State have the given name?

(check-expect (state-has-name 'reject 'reject) #true)
(check-expect (state-has-name 'reject 'accept) #false)
(check-expect (state-has-name STATE1 'q1) #true)
(check-expect (state-has-name STATE1 'q5) #false)

(define (state-has-name state name)
  (cond [(symbol? state) (symbol=? state name)]
        [(cons? state) (symbol=? (first state) name)]))