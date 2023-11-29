# sat
a boolean satisfiablity solver (sat solver) can determine the satisfiability of propositional logic formulas -- i.e. they are able to decide whether there is an assignment of truth values to variables in the formula that make the entire formula true. 

### motivation
when i was learning to use cbmc for my verification work on [sean anderson's bit-twiddling hacks](https://github.com/space-miner/bit-twiddling). i was derailed when i saw the [dimacs](https://jix.github.io/varisat/manual/0.2.0/formats/dimacs.html) flag (to generate cnf in dimacs format) in the backend options section of the manpage. 

i made another dpll sat solver to verify the dimacs generated from cbmc when trying to verify c programs. dpll is a recursive depth-first search with unit propagation and backtracking to explore the search space. the solver works on simple test cases i found like [uf20-91](https://github.com/space-miner/sat/tree/main/test/uf20-91) but is not able to do well with the dimacs produced by cbmc on real c programs -- i will no longer be competing at the [international sat competition](http://www.satcompetition.org/), and will instead be focusing on making my sat solver good enough to verify my programs.

![](https://img.imgdd.com/f210f3.0a05960d-aacb-4543-a4bf-2fe7300f6df7.png)

### todo
* write a checker for resolution proofs
* generate unsat certificates and pass sat results to a checker to verify
* extend to do bcp with two watched literals
* implement cdcl
