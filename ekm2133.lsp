;15 Puzzle 

;State for 15 puzzle is ( ( row1 ) ( row2 ) ( row3 ) ( row4 ) (r c))
;where (r c) are the coordinates of the blank

(defun 15-puzzle (start goal)
  (print "BFS:")
  (bfs start goal)
  (print "DFS:")
  (dfs start goal 2500)
  (print "IDDFS:")
  (iddfs start goal 2 5)
  (print "UCS:")
  (ucs start goal 'uniform-cost)
  (print "Greedy (Manhattan):")
  (greedy start goal 'manhattan)
  (print "A* (Manhattan):")
  (a-star start goal 'uniform-cost 'manhattan)
  (print "Greedy (Tiles Out of Row and Column):")
  (greedy start goal 'tiles-out-of-row-and-column)
  (print "A* (Tiles Out of Row and Column):") 
  (a-star start goal 'uniform-cost 'tiles-out-of-row-and-column)
  (format nil "END")
)

;Breadth-first search
;Node representation: (state operator parent)

;Define accessor functions
(defun state (node) (first node)) 
(defun operator (node) (second node)) 
(defun parent (node) (third node))


(defun bfs (s0 sg) 
(let ((open (list (list s0 nil nil))) ;Set open to be a list containing the starting node
      (closed nil)                    ;Set closed to be an empty list
      (n nil)                         
      (daughters nil)
      (total-nodes 0)
      (previously-generated 0))
(loop      
(if (null open)                       ;If the open list is empty, there are no more nodes to explore
    (return 'fail))                   ;BFS fails
(setf n (pop open))                   ;Remove the first element of open and set it as n
(push n closed)                   ;Put n on closed list
(if (**equal** (state n) sg)      ;Check if n is the goal node
    (let ()
      (format t "~%Number of nodes generated: ~S" total-nodes) 
      (format t "~%Number of nodes containing states previously generated: ~S" previously-generated)
      (format t "~%Number of nodes on OPEN list: ~S" (length open))
      (format t "~%Number of nodes on CLOSED list: ~S" (length closed))
      (format t "~%Number of steps to solution: ~S" (count-steps n))
      (format t "~%Solution path:")
    (return (trace-solution n))))      ;Return solution path
(setf daughters (successor-function-bfs n))      ;Set daughters as the list of the successors of n
(setf total-nodes (+ total-nodes (length daughters)))
(setf previously-generated (+ previously-generated (count-previous daughters open closed)))
(setf daughters (diff daughters (append open closed)))    ;Remove nodes we have previously visited
(setf open (append open daughters))   ;Add daughters list to open list
)))

;successor-function specific to Breadth-first Search
(defun successor-function-bfs (node)  ;Generates successors of a node
(let ((son-nodes nil)                 ;List of nodes successor-function-bfs will return
      (son-state nil)                 
      (state (first node)))
(setf son-state (North state))        ;Applies north function to the state
(if (not (null son-state))            ;If the state is not null, create node and add to son-nodes list
    (setf son-nodes (list (list son-state 'N node))))
(setf son-state (South state))       ;Applies south function to the state
(if (not (null son-state))
    (setf son-nodes (append son-nodes (list (list son-state 'S node)))))
(setf son-state (East state))        ;Applies east function to the state
(if (not (null son-state))
    (setf son-nodes (append son-nodes (list (list son-state 'E node)))))
(setf son-state (West state))        ;Applies west function to the state
(if (not (null son-state))
    (setf son-nodes (append son-nodes (list (list son-state 'W node)))))
son-nodes))                          ;Returns the list son-nodes

;Depth-first search
;Node representation: (state operator parent depth-of-node)

;Do not need to change first three accessor functions, only need to write a fourth one
(defun depth-of-node (node) (fourth node))

(defun dfs (s0 sg depth-bound)   ;DFS takes a starting state, a goal state and a depth bound
(depth-bounded-dfs (list s0 nil nil depth-bound) sg depth-bound))   ;Depth-bounded-DFS called with a newly-created  node containing
                                                                    ;the start state, the goal state, and the depth-bound
(defun depth-bounded-dfs (node goal depth)
(let ((open (list node))         ;Set open as a list containing the node with the start state
      (closed nil)
      (n nil)
      (daughters nil)
      (total-nodes 0)
      (previously-generated 0)
      (current-depth depth)      ;Current-depth tracks the depth of the tree we are currently exploring
)
(loop
(if (null open) 
    (return 'fail))
(setf n (pop open))
(push n closed)
(if (**equal** (state n) goal)
    (let ()
      (format t "~%Number of nodes generated: ~S" total-nodes) 
      (format t "~%Number of nodes containing states previously generated: ~S" previously-generated)
      (format t "~%Number of nodes on OPEN list: ~S" (length open))
      (format t "~%Number of nodes on CLOSED list: ~S" (length closed))
      (format t "~%Number of steps to solution: ~S" (count-steps n))
      (format t "~%Solution path:")
    (return (trace-solution n))))
(if (= current-depth 1) (return 'fail))       ;If we have reached the depth limit, DFS fails
(setf daughters (successor-function-dfs n))
(setf total-nodes (+ total-nodes (length daughters)))
(setf previously-generated (+ previously-generated (count-previous daughters open closed)))
(setf daughters (diff daughters (append open closed)))
(setf open (append daughters open))
(decf current-depth)                          ;Decrements current-depth by 1
)))

;succcessor-function specific to Depth-first Search
(defun successor-function-dfs (node) 
(let ((son-nodes nil)
      (son-state nil)
      (state (first node)))
(setf son-state (North state))
(if (not (null son-state))
    (setf son-nodes (list (list son-state 'N node (- (depth-of-node node) 1))))) ;Adds a node to the son-nodes list, where the
(setf son-state (South state))                                                   ;new node has a depth value 1 less than its parent
(if (not (null son-state))
    (setf son-nodes (append son-nodes (list (list son-state 'S node(- (depth-of-node node) 1))))))
(setf son-state (East state))
(if (not (null son-state))
    (setf son-nodes (append son-nodes (list (list son-state 'E node (- (depth-of-node node) 1))))))
(setf son-state (West state))
(if (not (null son-state))
    (setf son-nodes (append son-nodes (list (list son-state 'W node (- (depth-of-node node) 1))))))
son-nodes))

;Iterative deepening depth-first search

;Node representation is the same as for DFS

(defun iddfs (s0 sg depth increment)             ;IDDFS takes a start state, a goal state, a starting depth, and an increment value
(if (not (equal 'fail (dfs s0 sg depth)))        ;If DFS at the starting depth does not fail, IDDFS returns true
    t
  (let ()
    (iddfs s0 sg (+ depth increment) increment)
    (return-from iddfs 'fail)
)))  ;If DFS does fail, increment the depth-bound by 1

;Uniform-cost search

;Node representation: (state operator g-hat parent)

;Redefine accessor functions (only need to define g-hat and redefine parent)
(defun g-hat (node) (third node))
(defun parent-ucs (node) (fourth node))

;Specify a uniform cost function (cost between any two nodes is 1)
(defun uniform-cost (m n) 1)

(defun ucs (s0 sg COST)
 (let ((open (list (list s0 nil 0 nil)))
       (closed nil)
       (n nil)
       (daughters nil)
       (total-nodes 0)
       (previously-generated 0))
 (loop
  (if (null open) 
      (return 'fail))
  (setf n (pop open))  
  (push n closed)
 (if (**equal** (state n) sg)
     (let ()
      (format t "~%Number of nodes generated: ~S" total-nodes) 
      (format t "~%Number of nodes containing states previously generated: ~S" previously-generated)
      (format t "~%Number of nodes on OPEN list: ~S" (length open))
      (format t "~%Number of nodes on CLOSED list: ~S" (length closed))
      (format t "~%Number of steps to solution: ~S" (count-steps-ucs n))
      (return (trace-solution-ucs n))))
 (setf daughters (successor-function-ucs n COST))
 (setf total-nodes (+ total-nodes (length daughters)))
 (setf previously-generated (+ previously-generated (count-previous daughters open closed)))
 (setf daughters (diff daughters closed))      ;Removes duplicate states previously explored
 (setf open (update daughters open))           ;Compares new daughter nodes with nodes in open list
 (setf open
     (sort open #'(lambda(node1 node2) (< (g-hat node1) (g-hat node2)))))   ;Sorts open list by g-hat value in increasing order
)))

;update checks for duplicates in daughters list, if it finds a duplicate, it chooses the node with the lower g-hat
(defun update (daughters open)
 (let ((m  nil) 
       (found-old-m nil))
 (loop 
  (if (null daughters) 
      (return open))
  (setf m (pop daughters))
  (setf found-old-m (member-state (state m) open))   ;Checks to see if daughter node contains a state already on open list
  (if found-old-m                                    ;If daughter duplicates a state already on the open list:
      (let ((old-m (first found-old-m)))             ;Member-state returns a list with the first element being the duplicate
            (if (< (g-hat m) (g-hat old-m))          ;Compare g-hat value of daughter node with g-hat of node already in open
                (setf (first found-old-m) m)))       ;Keeps node with the smaller g-hat value             
      (push m open)))))                              ;If we didn't find a duplicate, push the node to the open list

;member-state checks for a duplicate state, returns a list of nodes starting with the node containing the duplicate state
(defun member-state (state list-of-nodes)
 (if (null list-of-nodes)
     nil
   (if (**equal** state (state (first list-of-nodes)))       
         list-of-nodes                               ;Returns a list of nodes starting with the duplicate node
     (member-state state (rest list-of-nodes)))))

;successor-function specific to Uniform Cost Search
(defun successor-function-ucs (node COST)
  (let ((son-nodes nil) 
        (son-state nil) 
        (state (first node)))
    (setf son-state (North state))
    (if (not (null son-state))
       (setf son-nodes (list (list son-state 'N (+ (g-hat node) (funcall COST (state node) son-state)) node)))) ;Creates a new node and
    (setf son-state (South state))                                                                ;sets g-hat using uniform-COST
    (if (not (null son-state))
       (setf son-nodes (append son-nodes (list (list son-state 'S (+ (g-hat node) (funcall COST (state node) son-state)) node)))))
    (setf son-state (East state))
    (if (not (null son-state))
       (setf son-nodes (append son-nodes (list (list son-state 'E (+ (g-hat node) (funcall COST (state node) son-state)) node)))))
    (setf son-state (West state))
    (if (not (null son-state))
       (setf son-nodes (append son-nodes (list (list son-state 'W (+ (g-hat node) (funcall COST (state node) son-state)) node)))))
son-nodes))

;trace-solution specific to Uniform Cost Search
(defun trace-solution-ucs (node)             ;Define a separate trace-solution function because this one calls parent-ucs
(cond ((null (parent-ucs node))
        nil)
      (t
       (trace-solution-ucs (parent-ucs node))
       (print (operator node)))))

;count-steps specific to Uniform Cost Search
(defun count-steps-ucs (node)
(let ((count 0)
      (current-node node))
(loop
(if (null (parent-ucs current-node))
    (return count))
(incf count)
(setf current-node (parent-ucs current-node)))))

;Greedy Search Algorithm
;Node representation: (state operator parent)

(defun greedy (s0 sg HEURISTIC)                     ;Greedy takes an extra argument specifying 
 (let ((open (list (list s0 nil nil)))              ;the heuristic function to use
       (closed nil)
       (n nil)
       (daughters nil)
       (total-nodes 0)
       (previously-generated 0))
 (loop
  (if (null open) 
      (return 'fail))
  (setf n (pop open))  
  (push n closed)
 (if (**equal** (state n) sg)
     (let ()
      (format t "~%Number of nodes generated: ~S" total-nodes) 
      (format t "~%Number of nodes containing states previously generated: ~S" previously-generated)
      (format t "~%Number of nodes on OPEN list: ~S" (length open))
      (format t "~%Number of nodes on CLOSED list: ~S" (length closed))
      (format t "~%Number of steps to solution: ~S" (count-steps n))
      (format t "~%Solution path:")
       (return (trace-solution n))))
 (setf daughters (successor-function-bfs n))         ;Can use the same successor-function defined for BFS since node representation is same
 (setf total-nodes (+ total-nodes (length daughters)))
 (setf previously-generated (+ previously-generated (count-previous daughters open closed)))
 (setf daughters (diff daughters closed)) 
 (setf open (update-greedy daughters open HEURISTIC sg)) 
 (setf open
     (sort open 
           #'(lambda(node1 node2) (< (funcall HEURISTIC (state node1) sg) (funcall HEURISTIC (state node2) sg))))))))
           ;Sorts open list based on the h-hat values, generated by applying heuristic            

;Update function specific to Greedy Search
(defun update-greedy (daughters open HEURISTIC goal)
 (let ((m  nil) 
       (found-old-m nil))
 (loop 
  (if (null daughters) 
      (return open))
  (setf m (pop daughters))
  (setf found-old-m (member-state (state m) open))
  (if found-old-m                  
      (let ((old-m (first found-old-m)))
            (if (< (funcall HEURISTIC (state m) goal) (funcall HEURISTIC (state old-m) goal))   ;Similar to update function
                (setf (first found-old-m) m)))                         ;for UCS except it calls
      (push m open)))))                                                ;heuristic instead

 ;A* Search Algorithm

;Node representation: (state operator g-hat h-hat parent)

;Need to create h-hat accessor and new parent accessor
(defun h-hat (node) (fourth node))
(defun parent-a (node) (fifth node))

(defun a-star (s0 sg COST HEURISTIC)
 (let ((open (list (list s0 nil 0 (funcall HEURISTIC s0 sg) nil)))
       (closed nil)
       (n nil)
       (daughters nil)
       (total-nodes 0)
       (previously-generated 0))
 (loop
  (if (null open) 
      (return 'fail))
  (setf n (pop open))  
  (push n closed)
 (if (**equal** (state n) sg)
     (let ()
      (format t "~%Number of nodes generated: ~S" total-nodes) 
      (format t "~%Number of nodes containing states previously generated: ~S" previously-generated)
      (format t "~%Number of nodes on OPEN list: ~S" (length open))
      (format t "~%Number of nodes on CLOSED list: ~S" (length closed))
      (format t "~%Number of steps to solution: ~S" (count-steps-a n))
      (format t "~%Solution path:")
      (return (trace-solution-a n))))
 (setf daughters (successor-function-a n COST HEURISTIC sg))
 (setf total-nodes (+ total-nodes (length daughters)))
 (setf previously-generated (+ previously-generated (count-previous daughters open closed)))
 (setf open (update-a daughters open closed COST HEURISTIC sg))
 (setf open
     (sort open 
           #'(lambda(node1 node2) (< (+ (g-hat node1) (h-hat node1)) (+ (g-hat node2) (h-hat node2))))))
)))

;Update function specific to A* Search
(defun update-a (daughters open closed COST HEURISTIC goal)
 (let ((m  nil) 
       (found-old-m-open nil)
       (found-old-m-closed nil))
 (loop 
  (if (null daughters) 
      (return open))
  (setf m (pop daughters))
  (setf found-old-m-open (member-state (state m) open))
  (setf found-old-m-closed (member-state (state m) closed))
  (cond ((not (null found-old-m-open))
      (let ((old-m-open (first found-old-m-open)))
            (if (< (+ (g-hat m) (funcall COST (state (parent-a m)) (state  m))) (g-hat old-m-open))
                (setf (first found-old-m-open) (list (state (first found-old-m-open)) (operator (first found-old-m-open)) (+ (g-hat m) (funcall COST (state (parent-a m)) (state  m))) (h-hat (first found-old-m-open)) (parent-a m))))))
        ((not (null found-old-m-closed))
         (let ((old-m-closed (first found-old-m-closed)))
            (if (< (+ (g-hat m) (funcall COST (state (parent-a m)) (state  m))) (g-hat old-m-closed))
                (let ()
                 (setf (first found-old-m-closed) (list (state (first found-old-m-closed)) (operator (first found-old-m-closed)) (+ (g-hat m) (funcall COST (state (parent-a m)) (state  m))) (h-hat (first found-old-m-closed)) (parent-a m)))
                (push (first found-old-m-closed) open)))))
        (t
        (push m open)
        (setf m (list (state m) (operator m) (+ (g-hat m) (funcall COST (state (parent-a m)) (state m))) (funcall HEURISTIC (state m) goal) (parent-a m))))))))

;successor-function specific to A* Search
(defun successor-function-a (node COST HEURISTIC goal)
  (let ((son-nodes nil) 
        (son-state nil) 
        (state (first node)))
 (setf son-state (North state))
    (if (not (null son-state))
       (setf son-nodes (list (list son-state 'N (+ (g-hat node) (funcall COST (state node) son-state)) (funcall HEURISTIC son-state goal) node)))) ;Creates a new node and
    (setf son-state (South state))                                                                                                    ;sets g-hat using uniform-COST
    (if (not (null son-state))
       (setf son-nodes (append son-nodes (list (list son-state 'S (+ (g-hat node) (funcall COST (state node) son-state)) (funcall HEURISTIC son-state goal) node)))))
    (setf son-state (East state))
    (if (not (null son-state))
       (setf son-nodes (append son-nodes (list (list son-state 'E (+ (g-hat node) (funcall COST (state node) son-state)) (funcall HEURISTIC son-state goal) node)))))
    (setf son-state (West state))
    (if (not (null son-state))
       (setf son-nodes (append son-nodes (list (list son-state 'W (+ (g-hat node) (funcall COST (state node) son-state)) (funcall HEURISTIC son-state goal) node)))))
son-nodes))

;trace-solution specific to A* Search
(defun trace-solution-a (node) 
(cond ((null (parent-a node))
        nil) 
      (t 
       (trace-solution-a (parent-a node)) 
       (print (operator node)))))

;count-steps specific to A* Search
(defun count-steps-a (node)
(let ((count 0)
      (current-node node))
(loop
(if (null (parent-a current-node))
    (return count))
(incf count)
(setf current-node (parent-a current-node)))))

;Heuristic functions

;Manhattan heuristic
(defun manhattan (start goal)
  (let ((distance 0))
    (dotimes (x 4)
      (dotimes (y 4)
        (if (not (equal (get-value start x y) 0))
            (setf distance (+ distance (+ (abs (- x (find-x (get-value start x y) goal))) (abs (- y (find-y (get-value start x y) goal)))))))))
distance))

;tiles-out-of-row-and-column counts the number of tiles which are not in the correct row or column
(defun tiles-out-of-row-and-column (start goal)
  (let ((tiles-out 0))
    (dotimes (x 4)
      (dotimes (y 4)
        (if (not (equal (get-value start x y) 0))
            (let ()
              (cond ((not (equal (find-x (get-value start x y) start) (find-x (get-value start x y) goal)))
                     (incf tiles-out))
                    ((not (equal (find-y (get-value start x y) start) (find-y (get-value start x y) goal)))
                     (incf tiles-out)))))))
    tiles-out))
                     
;Accessory functions for heuristics

;get-value returns the value in a location specified by x and y coordinates
(defun get-value (state x y)
(nth x (nth y state)))

;find-x returns the x-coordinate of a specified value
(defun find-x (value state)
  (let ((x-coord nil))
  (dotimes (x 4)
    (dotimes (y 4)
      (if (equal (get-value state x y) value)
          (setf x-coord x))))
  x-coord))

;find-y returns the y-coordinate of a specified value
(defun find-y (value state)
  (let ((y-coord nil))
  (dotimes (x 4)
    (dotimes (y 4)
      (if (equal (get-value state x y) value)
          (setf y-coord y))))
  y-coord))


;Other functions 

;diff returns the new-nodes which do not already occur in open-and-closed nodes
(defun diff (new-nodes open-and-closed-nodes) 
(let ((return-nodes nil) )
(do 
((nodes new-nodes (rest nodes)))
((null nodes) return-nodes)
(do 
((scan-nodes open-and-closed-nodes (rest scan-nodes)))
((null scan-nodes) (push (first nodes) return-nodes))
(cond ((**equal** (state (first nodes)) (state (first scan-nodes)))
       (return nil)
))
))
return-nodes))

;trace-solution returns the solution path
(defun trace-solution (node)
(cond ((null (parent node))
        nil)
      (t
       (trace-solution (parent node))
       (print (operator node)))))

;count-steps returns the number of steps in the solution path
(defun count-steps (node)
(let ((count 0)
      (current-node node))
(loop
(if (null (parent current-node))
    (return count))
(incf count)
(setf current-node (parent current-node)))))
  
;count-previous counts the number of states generated that have already been generated before
(defun count-previous (daughters open closed)
 (let ((m  nil) 
       (found-old-m-open nil)
       (found-old-m-closed nil)
       (previously-generated 0))
 (loop 
  (if (null daughters) 
      (return open))
  (setf m (pop daughters))
  (setf found-old-m-open (member-state (state m) open))
  (setf found-old-m-closed (member-state (state m) closed))
  (cond ((not (null found-old-m-open))
         (incf previously-generated))
        ((not (null found-old-m-closed))
         (incf previously-generated))))
previously-generated))

;**equal** checks to see if two states are equal
(defun **equal** (state-1 state-2)
  (cond
   ((and (null state-1)(null state-2)) 
    t)
   ((or (null state-1) (null state-2)) nil)
   ((atom (first state-1))
     (cond ((equal (first state-1) (first state-2)) 
            (**equal** (rest state-1) (rest state-2)))
           (t nil)))
   ((listp (first state-1))
    (cond ((equal (first state-1) (first state-2))
           (**equal** (rest state-1) (rest state-2)))
          (t nil)))
    (t 
     (**equal** (rest state-1) (rest state-2)))))


;Operator functions

;moves the blank tile north
(defun North (state)
  (let ((newstate nil))
  (cond
  ((<= (first (fifth state)) 0) nil)  
  (t                              
   (setf newstate                       
     (swap (copy-tree state) 
             (first (fifth state))  
             (second (fifth state))  
             (- (first (fifth state)) 1) 
             (second (fifth state)))
    )))))

;moves the blank tile south
(defun South (state)
  (let ((newstate nil))
  (cond
  ( (>= (first (fifth state)) 3) nil)  
  (t                              
   (setf newstate                      
     (swap (copy-tree state) 
             (first (fifth state))  
             (second (fifth state)) 
             (+ 1 (first (fifth state))) 
             (second (fifth state))) 
    )))))

;moves the blank tile east
(defun East (state)
  (let ((newstate nil))
  (cond
  ( (>= (second (fifth state)) 3) nil)   
  (t                            
   (setf newstate                      
     (swap (copy-tree state) 
             (first (fifth state))  
             (second (fifth state))  
             (first (fifth state)) 
             (+ 1 (second (fifth state)))) 
    )))))

;moves the blank tile west
(defun West (state)
  (let ((newstate nil))
  (cond
  ( (<= (second (fifth state)) 0) nil)   
  (t                              
   (setf newstate                      
     (swap (copy-tree state) 
             (first (fifth state))   
             (second (fifth state))  
             (first (fifth state)) 
             (- (second (fifth state)) 1)) 
    )))))

;called by other operator functions to swap the tile values
(defun swap (state from-row from-col to-row to-col)
 (let ( (temp nil) )
     (setf temp (nth from-col (nth from-row state)))
     (setf (nth from-col (nth from-row state))
           (nth to-col (nth to-row state)))
     (setf (nth to-col (nth to-row state)) temp)
     (setf (fifth state) (list to-row to-col))
     state) 
   ) 