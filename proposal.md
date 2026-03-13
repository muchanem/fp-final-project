Mark Muchane and Julia Margie 

Games like Connect 4 can be modeled using Monte Carlo Tree Search. Our project aims to implement that for Connect 4, with the end product being a suggestion for a player’s next move in a specific game state. Given the high branching factor in many adversarial games, it can be efficient to do beam search to only expand the k most promising states at each step. The k most promising steps give a natural unit to parallelize over, but creates a difficult problem to solve: propagating the MCTS statistics to select the k states. 

During our project, we hope to learn and implement  

* parallelism, taking advantage of the immutability of Haskell  
* a prediction model, using a DAG and efficiently (not) calculating repeated states 

More specifically, we will split this into several, smaller problems: (1) building the tree structure and its statistics, (2) building the search/simulation strategy, and (3) parallelizing the search. The goals below are organized into difficulty levels, but goals labelled with (1) all pertain to building the tree, with (2), the search, and (3), parallelization.

There are interesting Haskell specific aspects to this problem:

* Persistent vs mutable data structures for the tree/DAG  
* Software Transactional Memory (STM) could make concurrent tree updates faster  
* All of the search policies in (2) are pure functions over the statistics

Goals

* Easy  
  1) Base implementation. Flat game tree with visit counts and win rates at each node.   
  2) search strategy of pure Monte Carlo–random rollout from the leaf nodes.  
  3) each thread does independent rollouts from each leaf (embarrassingly parallel)  
* Medium  
  1) Turn the tree into a DAG to reduce redundancy by adding a transposition table  
  2) Add virtual loss to improve parallel compute (i.e. once a thread is exploring a node, penalize it so we explore more than exploit)  
  3) Add an Upper Confidence Bound (UCT) to select leaves with better statistics.  
  4) Each thread builds its own independent tree, which all merge at the end  
* Challenge  
1) Add progressive widening (the more visited a node is, the more children it is allowed to have so that a more mature tree continues to explore more)  
   2) Add Rapid Action Value Estimation (RAVE) or All Moves As First (AMAF) to share information across sibling nodes (there might be moves that are generally bad or good, and we want to propagate that information–which presents a new parallelization difficulty, particularly given the immutability of Haskell).   
   3) Finally, we could have all threads share one tree–which would require fast (i.e. lock-free) concurrent access to the tree, which again is complicated by properties of Haskell.

These directions can be pursued semi-independently (i.e. without one implementation blocking the other), but improvement in one direction improves the other. Avoiding duplicating the same states in (1) means that (2) doesn’t waste compute. Conversely, calculating tree statistics with UCT in (2) means the data incoming is more immediately useful for (1). This means that we can easily split the work for equitable contributions, and if we are unsuccessful in medium or challenge level goals in one aspect of the project, we can still implement those levels in another. We plan to use *Parallel and Concurrent Programming in Haskell*, as recommended by Professor Kurtz.
