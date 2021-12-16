# Matrix

Parse a string with a matrix and print the numbers in the column or row in the given index

Example: "1 2 3\n4 5 6" will look like the one below after parsing:

```
1 2 3

4 5 6
```

Your code should have a way to collect the numbers in the row and the column:

```
row "1 2 3\n4 5 6" 1
[1, 2, 3]

column "1 2 3\n4 5 6" 1
[1, 4]
```