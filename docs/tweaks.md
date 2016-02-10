Idea 1: Simple bit flip

We are going to flip one 0 to a 1 in the current answer, and one 1 to a 0.
This ended up causing errors (if there wasnt a 1 or a 0 it would freak out) so we got rid of this.



Idea 2: Randomly flip 1 to 3 bits

For each mutation it randomly picks a integer from 1 to 3,
Then choses that many random indexes andn flips those bits in choices.
This works really well actually! We compared it to both random-answer- and a greedy solution and it performed better than both the majority of the time.

Idea 3: Same tweak strategy as idea 2, but start with a greedy answer

We wrote a greedy algorithm to see how random search compared. After doing that we decided it would be cool to combine the two. It seems that they result in similar answers with a sufficient number of tries. If you only switch bits a small number of times the greedy plus random performs better than the fully random version.

Idea 4: Added jumping every 50 tries to see if a different hill is found

This strategy seems a little worse then just climbing one hill, but increasing the amount of time a answer climbs before jumping may make it better.


Results:
Running once on knapPI_16_20_1000_1 with 1000 mutations:
  - Idea 2: 2070N
  - Idea 3: 2050N
  - Idea 4: 2093N

Running once on knapPI_16_200_1000_1 with 1000 mutations:
  - Idea 2: -28716N
  - Idea 3: -12280N
  - Idea 4: 5193N

  - This did not do well for Idea 2 and 2. I think this is because we did not run enough mutations. Also we decided to use normal recursion instead of loop recur. This was a bad decision because we stack overflow around 4000 tries. If I had time to rewrite it I am sure we would get better results.
