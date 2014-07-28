15-puzzle
=========

Comparison of search algorithms for solving a 15-puzzle (Artificial Intelligence, Spring 2013)

I implemented the 15-puzzle using Common Lisp in the LispWorks compiler. The file "ekm2133.lsp" contains all the code necessary to run the project. The test file has a (load "ekm2133.lsp") call that should enable you to run the test cases. For the medium and hard test cases, it will sometimes be necessary to comment out the appropriate parts of the code to skip certain search algorithms which take too long to run. 

"15-puzzle" is the main function, and it is called with two arguments: a start state and a goal state. These arguments are then passed to the various search algorithms, along with any additional functions as necessary (e.g. a depth limit for Depth-First Search, a depth limit and increment for Iterative Deepening Depth-First Search, a cost function for Uniform Cost Search, a heuristic function for Greedy Search, and a cost function and heuristic function for A* Search). 

The puzzle is represented using the "row-major representation"
( (row 1) (row 2) (row 3) (row 4) (x y) )
where x and y are the coordinates of the blank space, which is represented as a 0. 

Each search function uses an open and closed list to keep track of the nodes generated. The open list holds all the nodes which still need to be expanded. The closed list holds the nodes which have already been expanded. The search function iterates through the open list until either the open list is null (all possible nodes have been explored) or it finds the solution state. The "diff" function checks to see if a state has already been explored. 

For certain functions, I wrote specialized accessory functions (e.g. "update-greedy" or "parent-a") because those searches use different node representations.

For each search algorithm, if it runs successfully, "15-puzzle" outputs the solution path, as well as some statistics: the total number of nodes generated, the number of nodes containing states previously generated, the number of nodes on the OPEN list at the time of termination, the number of nodes on the CLOSED list at the time of termination, and the number of steps to the solution. The solution path is not returned as a list because I couldn't figure out how to do that, and anyway, Adrian said it was fine to have it in this format. 

I chose to implement the Manhattan heuristic, as well as another one I saw online, the Tiles Out of Row and Column heuristic (http://heuristicswiki.wikispaces.com/Tiles+out+of+row+and+column), which counts the number of tiles not in the right column and the number of tiles not in the right row. 

There are four files in this folder: this one, "ekm2133.lsp" which contains all the code, "ekm2133_test.lsp" which is the test driver, and "ekm2133_test.txt" which provides examples of test cases which I have already run. 

