
Function-based data structures
==============================

In programming languages where functions are statically compiled, such
as C++, it's natural to view data structures as aggregations of
machine-level primitive values.  Data produced in a running program
consists of blocks of data containing integers, pointers, strings, and
so forth, but not functions.
The only functions in a program are those that exist when the program
is loaded into memory.  Thus, functions play little or no role in
data structures.

With the ability to make functions dynamically, functions are another
kind of primitive value that can be used to hold data.  Functions
are maps from inputs to outputs, built out of code.
For instance, the Haskell function `sqrt` maps nonnegative numbers to their
square roots; the Haskell function `words` maps strings to the lists of
words that they contain.  Keep in mind this notion of functions as
mappings; it is often more useful than the imperative view of functions as
instructions for a computer to execute.

The exercises in this repository take you through function-based encodings
of some familiar data structures.

Intensional Sets
------------------------

A set is a collection of values.  Consider the following two sets:

* Set `S` contains 2, 4, 6, and 8 (and nothing else).
* Set `T` contains the even integers that are greater than 0 and less than 10.

`S` and `T` are the same set, but they suggest different ways of
implementing sets as data structures.
The first definition explicitly lists the elements of the set, and is
called an *extensional* definition.
Typical set data structures follow the extensional definition by storing
each of a set's elements in memory.
For example, `S` may be represented as the list `[2, 4, 6, 8]`.
The second definition states what properties distinguish set members from
non-members, and is called an *intensional* definition.
Such sets are represented by a function, called an *indicator function*
or *characteristic function*,
that maps the set's members to `True` and non-members to `False`.
`T` can be implemented as the function
```\n -> n `mod` 2 == 0 && n > 0 && n < 10```.
You can read this function as saying, "n is in the set if it is even,
greater than zero, and less than ten."
The intensional definition of sets is given in `Set.hs` like this:

    newtype Set = Set (Int -> Bool)

That is, a `Set` consists of an indicator function.

Indicator functions perform better than lists or hash tables in
some situations.  Indicator functions can encode large sets in a small
amount of space, as long as the set has a simple description.
For instance, the set of all `Int`s is `Set (\_ -> True)`, which takes
up much less space than storing every `Int` value in memory.
Indicator functions sometimes lead to faster code, in particular by
taking advantage of compiler optimizations that simplify functions.
Understanding the performance tradeoffs requires familiarity with
compiler optimizations.  This is too large a subject to
cover here; interested readers can investigate *short-cut fusion* for
an example of how functions interact with compiler optimizations in
practice.

As practice, implement the functions in `Set.hs` whose
bodies are undefined.  Recall that an indicator function represents a
statement of the form "The set contains a given integer, n, if ...";
several of the functions boil down to defining an indicator function
that completes this statement appropriately.

Property checks for testing your implementation are in `CheckSet.hs`.
To check your implementation, run

    runhaskell CheckSet.hs

and verify in the output that all tests pass.

Bounded Sets
------------------------

Indicator functions do one thing: test whether a value is in the set.
From the previous exercise, you can see that operations such as set union
are easy to define using indicator functions.
However, many operations are unreasonably difficult to define this way.
For instance, the only way to decide whether a set is empty is to
test, individually, whether every `Int` value is in the set.

When it's known in advance that a set's members will fall into a small
range of integer values, we can annotate sets with range information
and simply ignore the set elements outside the range.  In `BoundedSet.hs`,
the data type `BoundedSet` is defined for this purpose.

    data BoundedSet = BoundedSet Int Set

We define a `BoundedSet b s` to mean the set of numbers that are
both in the range `0` through `b`, inclusive, and in set `s`.  It is
now reasonably efficient to loop over all of the set's elements.
An example of such a loop is `boundedSetToList`, which returns a list
of set members.  This code is almost a straightforward translation
of the above definition.

    boundedSetToList :: BoundedSet -> [Int]
    boundedSetToList (BoundedSet b s) = [x | x <- [0..b], x `member` s]

As practice, implement the functions in `BoundedSet.hs` whose
bodies are undefined.  Binary functions like `bsUnion` should work
correctly when the two arguments have different bounds.
You may find that using `bsResize` simplifies the problem.

Property checks for testing your implementation are in `CheckBoundedSet.hs`.
To check your implementation, run

    runhaskell CheckBoundedSet.hs

and verify in the output that all tests pass.

Graphs
------------------------

You should be familiar with the BFS (breadth-first search) algorithm.
This exercise is to implement BFS for a given graph data type that
involves functions and `BoundedSet`s.
You can use a textbook implementation—the point is that you can
forget about the fact that the graph is "made of" functions.

Implement `breadthFirstSearch` in `Graph.hs`.  Recall that BFS
keeps track of the set of visited nodes and the set of newly
discovered nodes (the frontier), and loops until it
discovers no new graph nodes.

A test is in `CheckGraph.hs`. To check your implementation, run

    runhaskell CheckGraph.hs

and verify in the output that the test passed.
