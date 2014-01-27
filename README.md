pi-calculus
===========

3rd year final project : Implementation of a New Web Language

The applied pi-calculus is a small, formal, and very expressive language to describe computations based on processes sending messages to each other. It is used in industry and in academia in particular to describe realistic security protocols, whose security can then be analyzed with static analysis tools.

A limitation of this approach is that models expressed in the applied pi-calculus are not directly executable, and therefore cannot be used as proof-of-concept implementations, and cannot be debugged very easily.

The goal of the project is to provide an implementation for the language so that for example the applied-pi model of a web protocol (such as the OAuth authentication protocol used by Facebook, Yahoo, Twitter, etc.) can be directly executed, iteroperably, with existing JavaScript and PHP based implementations. The resulting language will be a novel, concise, general purpose langauge for web tinkering (the equivalent of a shell script langauge in Linux).

Author: Will de Renzy-Martin
Supervisor: Sergio Maffeis
