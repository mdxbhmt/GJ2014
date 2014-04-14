2014 Google Code Jam
===============

This is my Haskell code for this year's GCJ, where I hope I can get some coding practice.

[Contest page](https://code.google.com/codejam/contest/2974486/dashboard#s=p0)

All parsing done using  `Attoparsec`, trying to stick with `bytestring`/`vectors` whenever possible.

A
-

Easy problem, pick row g1 from grid 1, pick row g2 from grid 2. Find the intersection. 
If: it's empty -> Volunteer cheated; a single element -> Volunteer card; Multiple cards -> baaad magician.

Had some initial trouble with attoparsec learning that `Space \= string " "` since it includes white space.

B
-

Trick here is to get a piece of paper, find a formula to determine at which rate it's better to wait out. ie

        X/rate < C/rate + X/(rate+F)
        
Having the rate you can find the index it happens `(rate=2+n*F)`, then calculate the time.

Straightforward code, with output formating from `Text.Printf`

Piece of cookie

C
-

Couldn't figure out a way to solve this one. Tried filling the smaller side that can be filled with mines, if not possible, fill the largest.

Must think more.


D
-

For War, Naomi hasn't much of a say on how the game is played. Ken always get the best of his blocks; he can use the worst block that wins,
 or the worst block that loses.

For Deceitful War, Naomi has to make Ken waste his blocks. Since she knows his blocks, Naomi can force him to use the best block to win against her worst block, 
or use her worst block to win against his worst block.

Since there is many comparisons between best block and worst block and drops, I decided to sort the list of blocks first and use a deque - called `Sequence` in Haskell.

This way the relevant elements are easy to access.
