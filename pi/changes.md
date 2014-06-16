Version 0.0.5:
--------------

Changelog!

- Fixed an issue with pattern matching on lists. Lists of the same length pattern match exactly: i.e.
let list(a,b,c) = list(1,2,3) => a = 1 , b = 2, c = 3 . 

- Added the primitive append(list(a..),list(b..)) => list(a..,b..)

