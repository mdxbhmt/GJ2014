2014 Google Jam
===============

This is my Haskell code for this year's Gj, where I hope I can get some coding practice.

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
        
Having the rate you can find the index it happens (rate=2+n*F), then calculate the time.

Piece of cookie

