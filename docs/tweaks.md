Idea 1: Simple bit flip

We are going to flip one 0 to a 1 in the current answer, and one 1 to a 0.



Idea 2: Randomly flip 1 to 3 bits

For each mutation it randomly picks a integer from 1 to 3,
Then choses that many random indexes andn flips those bits in choices.

Idea 3: Same tweak strategy as idea 2, but start with a greedy answer

We wrote a greedy algorithm to see how random search compared. After doing that we decided it would be cool to combine the two. It seems that they result in similar answers with a sufficient number of tries. If you only switch bits a small number of times the greedy plus random performs better than the fully random version.
