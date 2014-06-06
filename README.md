Exercises for Principles of Programming Languages
==========================

This repository contains all the source code for the exercise lectures for the 2014 edition of the *Principles of Programming Languages* course offered by *Politecnico di Milano*. Please refer to the [course site](http://home.deib.polimi.it/pradella/PL.html) for the lessons material.

# Lecture 1 - Scheme
* Basic scheme programming: if, cond, named let
* Recursion and tail recursion
* Lists

# Lecture 2 - Scheme
* Argument passing
* More on lists and vectors
* Structs
* Macro: simple examples, C vs. Scheme

# Lecture 3 - Scheme
* Macro: more advanced examples, C vs. Scheme (reprise)
* Continuations: break, continue, coroutines

# Lecture 4 - Haskell
* Basic Haskell programming: if, pattern matching, where
* Recursion and tail recursion
* Lists, list comprehensions, laziness
* Data types: intro and *deriving*, *instance*

# Lecture 5 - Haskell
* Folds
* Data types: ordered binary tree, with utility functions
* Functor and Foldable: generic map and fold for new data types
* Monads: Monad class, Maybe and the phonebook example

[Functors, applicatives and monads in pictures (tutorial)](http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html)

# Lecture 6 - Haskell
* Monads: instantiating a Monad and creating utility methods
* Create a new Logger Monad and reimplement the Tree methods to take advantage in it
* (Very) brief examples of Monads in Scala

*Please note*: I have included today's slides with the project description, but refer to the course site for the updated version.

# Lecture 7 - Erlang
* Introduction and brief history
* Functional Erlang
* Examples on basic syntax, recursion and tail recursion, higher order functions
* The binary tree, Erlang style

*Please note*: there is no need for a specific Erlang version, any version from (at least) 14 to 17 should be enough for our examples.

Each function is preceded by the specification of parameters and return value types: while this is not as precise as in languages like Haskell, and it does not given any information to the compiler (so there is not compile time checking, Erlang being a dynamically typed language), these specifications are supported by an Erlang tool called [Dialyzer](http://learnyousomeerlang.com/dialyzer), which analyzes an application and checks (with limitations) if the input and output types are correct. It is also useful from a readability point of view, and when (Javadoc-like) documentation is generated.

# Lecture 8 - Erlang
* Concurrent Erlang: the actor model
* Basic process syntax, fundamentals of fault tolerance
* Distributed Erlang: nodes and networks
* Examples
