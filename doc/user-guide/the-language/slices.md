# Slices

A slice is a collection of values typed only by the element type, not the
length of the collection.

```
names = []{"Sarah", "Joe", "Maxime"}

numbers : []{int}
numbers = []{1, 2, 3, 4, 5}
```

Slice elements can be accessed with square brackets:

```
greeting = "Hello, " ++ names[0]

twoLevelSlice = []{[]{1, 2, 3}, []{4, 5, 6}}
isSix = twoLevelSlice[1][2] == 6
```
