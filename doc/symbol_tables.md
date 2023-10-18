Functional symbol tables and red-black trees in Haskell
=======================================================

In past posts I developed a decent lexer and parser for the Tiger programming
language by using the `alex` and `happy` parser generator tools. The parser can be
designed to use monads to include some basic error reporting using token
position within the input stream. Unfortunately for various personal and
work-related reasons I put down the work on compilers and never got back to it
in depth, but as part of my 2018 New Year Resolutions I'm determined to continue
and get it done!

In this post I plan on talking about the last phase of the "front-end" of a
compiler, the type-checking phase. Before we can do type-checking, we must
perform some associated tasks like building a symbol table that tracks the names
and types of all symbols declared in the program.

# Type-checking

Before we move on to implementation, let's discuss what a *type* is, and why we
even want to bother with them.

Tiger is a *statically-typed* language, meaning the types of symbols (variables
and functions) must be declared ahead of time. Types are descriptions like
"Integer" or "String" that are essentially metadata about how we plan to
interpret and use the data stored in the variable.

To be a bit more concrete, consider the following. At the hardware level
everything is 0's and 1's, so all data (every variable!) is really 0's and 1's.
Let's say 101100 is stored in a variable x. What does that mean? Well, naively,
101100 in binary directly translates to 44 (32+8+4, by adding up the places of
the digits) in decimal, so we might assume the program needs the number 44 for
some calculation. But are we sure that's what we meant? Commonly in computer
science and computer engineering, we use *two's-complement arithmetic*, where the
highest value bit is considered a negative value so that we may represent
integers within a range of positive and negative values (notice the naive direct
translation can never produce negative numbers!). So if *x* was meant to store a
two's-complement number, it would actually be equivalent to the decimal number
-20 (-32+8+4). So maybe *x* represents a 44? or maybe -20? Which is it?
Furthermore, maybe it was never meant to be a number we do arithmetic with, but
rather data used to store a message. If *x* represented a letter of an English
message, encoded in ASCII format, an ASCII 44 is actually the code for a comma,
so *x* would represent a comma character (presumably as part of a larger message).
Each of these interpretations come from the same set of bits.

What we'd love to do is explicitly mark what type of value we intend to store in
*x*; not only would be it be useful for documentation (while you personally might
know what you intend to do with *x*, you might not remember in a month, and if
someone else takes over the project from you, how are they supposed to know?),
but we'd really love it if the compiler could keep track of what *x* represents to
make sure that we never try to add an ASCII character to a two's-complement
number, or something similarly nonsensical. If we ever tried such an operation
that confused the meaning of our data, that would be a pretty clear sign
something went off the rails and is likely incorrect, so the compiler could
helpfully give us a warning that we're about to do something that might end
badly (we should always be performing arithmetic on two's complement numbers, or
only printing ASCII characters to the screen, for example).

*Types* in a programming language are exactly these sorts of annotations that
alert the programmer (and compiler) to what sort of data we're storing and how
we intend to use it. When we define *x*, we don't just immediately shove data into
memory, we define it as "`int x`" , "`unsigned y`", or "`char z`" to be specific we
intend *x* to be a typical two's-complement integer, *y* to be an unsigned "plain"
binary integer, and *z* to be an ASCII character. The type information not only
acts as documentation but allows the compiler to perform the necessary
conversions to the proper format in memory. The compiler deals with the binary
0s and 1s representation of the data, and the programmer can focus on program
logic.

More robust type systems allow the programmer to define more complex types that
better represent the data and how it is meant to be used. For example, tracking
not just characters but whole Strings of characters, or creating a Vector type
that tracks three different numerical values simultaneously to represent the XYZ
coordinates of an object. We're more likely to have a "correct" program if we do
heavy checking in a more strongly-typed language like Haskell than in a more
weakly-typed language like C, or dynamically-typed Python. (Particularly in
dynamically-typed languages, the interpreter will convert the data between types
on the fly to try to make it fit. It becomes much harder to track down errors in
some situations, and you can't even necessarily detect this type of error up
front, only at run time when it might be the most costly to encounter and fix.)
Exactly what is meant by "more complex types" quickly goes beyond the scope of
this post and into type theory; suffice it to say for this Tiger project that we
can build new types from various combinations of basic types like integers and
strings.

# Symbol Tables Implementation

Returning to the Tiger compiler, our goal is to support
*type-checking*, which involves comparing the types of all literal values and
symbols (variables, function return values, and function parameters) to make
sure they match with their usage. For example, a function expecting an integer
parameter better not receive a string parameter instead! If we ever get a
mismatch, there's likely a bug in our program.

Before we can do this comparison, we must build *symbol tables* (also known as
*environments*). Symbol tables track the names and types of all the symbols in a
program, which includes variables, records, arrays, functions, and parameters.
This isn't quite as easy as it sounds because symbols in Tiger only apply within
a certain scope. *Scope* refers to the visibility of symbols within parts of the
program; while some symbols are *global* symbols, valid everywhere, other symbols
(called *local* symbols) apply only to a small part, or even only a single
statement! Sometimes two symbols can share the same name and type but have
different values depending on what scope they are called in.

Therefore, symbol tables must track symbol names, values, and scopes throughout
the program in order to properly type-check the program.

According to Appel, an efficient functional implementation of symbol tables is
done by use of a *red-black tree* so let's first look at what trees are and how
to use them, then we can dive into implementing our symbol table for
type-checking.

## Binary Trees

Appel does not actually give an implementation in the book,
so I consulted "Purely Functional Data Structures" by Chris Okasaki. Curious
about what trees are mathematically, I also looked at the graph theory
definitions of a tree in "A First Course in Graph Theory" by Chartrand and
Zhang.

*Graphs* are mathematical structures made up of nodes (drawn as circles
typically) connected by edges; more formally, graphs are the combination of a
set of items (*nodes*) with a sets of *edges* (defined as a pair of nodes) that
represents some sort of relationship between the nodes.

From a graph theory perspective, a *tree* is an acyclic, connected graph. *Acyclic*
refers to the fact that the graph must not have any cycles, which are looping
paths over the edges and nodes of a graph that ends at the same node it started
at. *Connected* refers to the idea that every node of the graph is reachable by
some path through the edges of the graph. In other words, starting from a chosen
root node, we can reach any other node in the tree by some path that only
traverses each node in the path once, and there are no loops in the path. These
restrictions mean the nodes tend to "branch out" from the root, hence the name
"tree".

From a computer science perspective, we attach to each node in the graph a value
so we can use the tree as a data structure. More specifically, we require that
the value be subject to an *ordering*, which is a way of ranking of the values
(basically, you must be able to define what the less-than operator < means for
the values; we're used to integer comparisons, but anything else works too as
long as you can define < over all possible values; extending this concept leads
to the mathematical ideas of *ordered sets* and *lattices*, which are important
for more advanced program analysis).

If we define an ordering on the values stored in the tree and restrict each node
to being connected to at most two "children" nodes, we call this tree a *binary
tree*. Typically we refer to the children of a node as the left and right
subtrees, and by defining an ordering, we can enforce that nodes with values
smaller than the current node's value must always be in the left subtree, and
larger values must always be in the right subtree. This creates a
*binary search tree* that is very efficient at locating values since we can
directly follow the path to where it "should be" in the tree. We only need to
do a relatively small number of comparisons ( \\( O(log n) \\), where `n` is
the number of nodes) to determine if we go right or left from the root node.
If we find the value, it is in the tree. If we get to the end without finding
the value, we know for sure the value is NOT in the tree because the ordering
enforces where it would be found.

Of course, since we can choose any node we want to be the root node, we might
end up choosing a root that is, for example, smaller than all other values; this
would mean that all other node values must be in the right subtree, and so in
certain scenarios we'd end up with one big long string of nodes all on the right
-- basically, a big linked list. This removes the efficiency we receive from
binary trees since it requires us to compare against most or even all of the
values in the tree rather than only a small number of comparisons, so we prefer
to avoid this scenario. One way to do so is by introducing further rules on how
the binary tree is constructed, such that it "balances" itself even as we add
new items to an existing tree. This ensures approximately equal numbers of nodes
in the left and right subtrees so that we get maximum efficiency for searches.
This balanced binary search tree is known as a *red-black tree*, since we use
color labels to help us define rules for balancing the tree.

## Red-Black Trees in Haskell

Red-black trees are called such because they label each node as one of
two colors, red or black. We define the colors, and the tree itself, as follows
(following Okasaki's lead):

    data Color = R | B
    data RedBlackTree a = E | T Color (RedBlackTree a) a (RedBlackTree a)

The definition of `RedBlackTree` encodes the concept that each node (`T`) has a
color, a value of type of `a`, and both a left subtree and a right subtree that
store values of the same type `a`. Empty nodes (`E`) are always considered black,
so there's no need to encode that color data into the type since it won't vary.

We define two invariants to ensure that the tree remains balanced:

1. No red node has a red child.
2. Every path from root to an empty node must contain the same number of black
   nodes.

When we insert nodes, we initially color them red, and then check for violations
of these invariants. If so, we must re-structure the tree to satisfy these
invariants in addition to the general binary search tree requirements of
ordering.

The two invariants ensure that we must always add new nodes at the same depth of
the tree, which ensures the tree doesn't get too unbalanced. However, since we
initially add red nodes (to avoid changing the number of black nodes in the
path, to satisfy invariant #2), we must then check that invariant #2 isn't
violated. There are only four possible configurations in a binary tree in which
two red nodes may exist in a row:



We can actually balance all four cases into a single case:


We therefore write this rule in Haskell as:

    balance :: Ord a => Color -> RedBlackTree a -> a -> RedBlackTree a -> RedBlackTree a
    balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
    balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
    balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
    balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
    balance color a x b = T color a x b

The Haskell definitions above correspond in order to the possible configurations
in the binary tree. The last rule is to simply accept any other configuration as
OK. The type signature is explicitly given here so that we can put the Ord a
restriction on it. This enforces that the type of the values being put in our
tree is something that is part of the Ord class, meaning that it defines an
ordering (<) on the values. As we stated above, the existence of an ordering is
important to the definition of our binary tree. I used the type restrictions
rather than using an instance of a Set as Okasaki defines because his code
requires language extensions, not pure Haskell! By putting our type restrictions
on the functions rather than the type class, we can mimic behavior without
requiring the compiler language extensions.

Editorial note: I started writing this a few months ago, and it was put on
hiatus as I ended up busy with personal and work projects. Turns out another
Haskeller beat me to the punch on this post! So I refer you to
[Abhiroop Sarkar's great post][1] on further details for a
Persistent Red Black Tree implementation
that includes deleting nodes from the Red Black Tree.

# Up Next

Now that we have a symbol table implementation, our next goal is to
implement the actual typechecker that walks the AST and the symbol table and
checks that the types of all symbols make sense. Once we do this final phase, we
can feel confident we've read in and understood a correct program that can now
be passed to the "middle-end" of the compiler for analysis and optimization.

# Further Reading

One aspect I learned as I worked on Tiger was that my knowledge of functional
data structures and design patterns is somewhat incomplete. Particularly as I
started looking at the typing system, even a supposedly
functional-language-based textbook leaned on imperative-style psuedo-code for
some tasks. From what I can tell, there aren't really any textbooks for
programmers that focus on functional data structures and algorithms. Okasaki's
book is the only possibility I'm aware of but since it was deriving from a PhD
thesis it seems a little too academic and not practical enough. His widely cited
implementation of Red-Black trees, for example, doesn't even cover the case of
deleting nodes, instead leaving it as an "exercise for the reader," which I more
commonly these days interpret as "I don't understand it well enough to write
about it yet, so you do it."

Without a good book on the subject, I looked for alternatives to complement my
understanding of functional design and functional thinking. I started reading
some of the ["Functional Pearls" papers][2], collected in a mostly-up-to-date
list on the Haskell website.

[1]: https://abhiroop.github.io/Haskell-Red-Black-Tree/
[2]: https://wiki.haskell.org/Research_papers/Functional_pearls
