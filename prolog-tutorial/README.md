* * *

# Prolog Tutorial

J. A. Robinson: A program is a theory (in some logic) and computation is deduction from the theory.  
N. Wirth: Program = data structure + algorithm  
R. Kowalski: Algorithm = logic + control

* * *

**Introduction to Prolog**

[Introduction](#intro)  
[The Structure of Prolog Program](#struc)

[Syntax](#syntax)

[Types](#types)

[Simple](#simple)  
[Composite](#composite)

[Expressions](#expr)  
[Unification and Pattern Matchine](#pat)  
[Functions](#fun)  
[Lists](#list)  
[Iteration](#iteration)  
[Iterators, Generators and Backtracking](#itgenback)  
[Tuples](#tuples)  
[Extra-Logical Predicates](#extralogical)

[Input/Output](#io)

[Style and Layout](#style)

**Applications & Advanced Programming Techniques**

[Negation and Cuts](#negation)  
[Definite Clause Grammars](#dcg)  
[Incomplete Data Structures](#inc)  
[Meta Level Programming](#meta)  
[Second-Order Programming](#2ndOrder)  
[Database](#database)  
[Expert Systems](#expert)  
[Object-Oriented Programming](#oop)

[Appendix](#appendix)

[References](#ref)

## <a name="intro"></a>Introduction

Prolog, which stands for PROgramming in LOGic, is the most widely available language in the logic programming paradigm. Logic and therefore Prolog is based the mathematical notions of relations and logical inference. Prolog is a declarative language meaning that rather than describing how to compute a solution, a program consists of a data base of facts and logical relationships (rules) which describe the relationships which hold for the given application. Rather then running a program to obtain a solution, the user asks a question. When asked a question, the run time system searches through the data base of facts and rules to determine (by logical deduction) the answer.

Among the features of Prolog are `logical variables' meaning that they behave like mathematical variables, a powerful pattern-matching facility (unification), a backtracking strategy to search for proofs, uniform data structures, and input and output are interchangeable.

Often there will be more than one way to deduce the answer or there will be more than one solution, in such cases the run time system may be asked find other solutions. backtracking to generate alternative solutions. Prolog is a weakly typed language with dynamic type checking and static scope rules.

Prolog is used in artificial intelligence applications such as natural language interfaces, automated reasoning systems and expert systems. Expert systems usually consist of a data base of facts and rules and an inference engine, the run time system of Prolog provides much of the services of an inference engine.

## <a name="struc"></a>The Structure of Prolog Programs

*   A Prolog program consists of a database of facts and rules, and queries (questions).
    *   Fact: ... .
    *   Rule: ... :- ... .
    *   Query: ?- ... .
    *   Variables: must begin with an upper case letter.
    *   Constants: numbers, begin with lowercase letter, or enclosed in single quotes.
    *   Inductive definitions: base and inductive cases

*   Towers of Hanoi: move N disks from pin a to pin b using pin c.

    ```prolog
    hanoi(N) :- hanoi(N, a, b, c).
    hanoi(0,_,_,_).
    hanoi(N,FromPin,ToPin,UsingPin) :-
            M is N-1,
            hanoi(M,FromPin,UsingPin,ToPin),
            move(FromPin,ToPin),
            hanoi(M,UsingPin,ToPin,FromPin).
    move(From,To) :- write([move, disk from, pin, From, to, pin, ToPin]),
        nl.
    ```

*   Lists: append, member

    ```prolog
    list([]).
    list([X|L]) :- [list(L).
    ```
    Abbrev: [X1|[...[Xn|[]...] = [X1,...Xn]
    ```prolog
    append([],L,L).
    append([X|L1],L2,[X|L12]) :- append(L1,L2,L12).
    member(X,L) :- concat(_,[X|_],L).
    ```

*   Ancestor

    ```prolog
    ancestor(A,D) :- parent(A,B).
    ancestor(A,D) :- parent(A,C),ancestor(C,D).
    ```
    _but not_
    ```prolog
    ancestor(A,D) :- ancestor(A,P), parent(P,D).
    ```
    _since infinite recursion may result._

*   Depth-first search: Maze/Graph traversal

    A database of arcs (we will assume they are directed arcs) of the form: 
    ```prolog
    a(node_i,node_j).
    ```

    Rules for searching the graph:

    ```prolog
    go(From,To,Trail).
    go(From,To,Trail) :- a(From,In), not visited(In,Trail), go(In,To,[In|Trail]).
    visited(A,T) :- member(A,T).
    ```

*   I/O: terms, characters, files, lexical analyzer/scanner

    *   `read(T)`, `write(T)`, `nl`.
    *   `get0(N)`, `put(N)`: ascii value of character
    *   `name(Name,Ascii_list)`.
    *   `see(F)`, `seeing(F)`, `seen`, `tell(F)`, `telling(F)`, `told`.

*   Natural language processing:

    Context-free grammars may be represented as Prolog rules. For example, the rule

    ```ebnf
    sentence ::= noun_clause verb_clause
    ```

    _can be implemented in Prolog as_

    ```prolog
    sentence(S) :- append(NC,VC,S), noun_clause(NC), verb_clause(VC).
    ```

    _or in DCG as:_

    ```prolog
    sentence --> noun_clause, verb_clause.
    ?- sentence(S,[]).
    ```

    Note that two arguments appear in the query. Both are lists and the first is the sentence to be parsed, the second the remaining elements of the list which in this case is empty.

A Prolog program consists of a data base of facts and rules. There is no structure imposed on a Prolog program, there is no main procedure, and there is no nesting of definitions. All facts and rules are global in scope and the scope of a variable is the fact or rule in which it appears. The readability of a Prolog program is left up to the programmer.

A Prolog program is executed by asking a question. The question is called a query. Facts, rules, and queries are called _clauses_.

## <a name="syntax"></a>Syntax

### <a name="facts"></a>Facts

A **fact** is just what it appears to be --- a fact. A fact in everyday language is often a proposition like "It is sunny." or "It is summer." In Prolog such facts could be represented as follows:

```prolog
'It is sunny'.  
'It is summer'.
```

### <a name="queries"></a>Queries

A **query** in Prolog is the action of asking the program about information contained within its data base. Thus, queries usually occur in the interactive mode. After a program is loaded, you will receive the query prompt,

```prolog
?- 
```

at which time you can ask the run time system about information in the data base. Using the simple data base above, you can ask the program a question such as

```prolog
?- 'It is sunny'.
```

and it will respond with the answer


```prolog
Yes
?- 
```

A `yes` means that the information in the data base is consistent with the subject of the query. Another way to express this is that the program is capable of proving the query true with the available information in the data base. If a fact is not deducible from the data base the system replys with a no, which indicates that based on the information available (the closed world assumption) the fact is not deducible.

If the data base does not contain sufficient information to answer a query, then it answers the query with a no.

```prolog
?- 'It is cold'.
no
?- 
```

### <a name="rules"></a>Rules

**Rules** extend the capabilities of a logic program. They are what give Prolog the ability to pursue its decision-making process. The following program contains two rules for temperature. The first rule is read as follows: "It is hot if it is summer and it is sunny." The second rule is read as follows: "It is cold if it is winter and it is snowing."

```prolog
'It is sunny'.
'It is summer'.
'It is hot' :- 'It is summer', 'It is sunny'.
'It is cold' :- 'It is winter', 'It is snowing'.
```

The query,

```prolog
?- 'It is hot'.
Yes
?- 
```

is answered in the affirmative since both 'It is summer' and 'It is sunny' are in the data base while a query `?- 'It is cold.'` will produce a negative response.

The previous program is an example of propositional logic. Facts and rules may be parameterized to produce programs in predicate logic. The parameters may be variables, atoms, numbers, or terms. Parameterization permits the definition of more complex relationships. The following program contains a number of predicates that describe a family's genelogical relationships.

```prolog
female(amy).
female(johnette).

male(anthony).
male(bruce).
male(ogden).

parentof(amy,johnette).
parentof(amy,anthony).
parentof(amy,bruce).
parentof(ogden,johnette).
parentof(ogden,anthony).
parentof(ogden,bruce).
```

The above program contains the three simple predicates: `female`; `male`; and `parentof`. They are parameterized with what are called 'atoms.' There are other family relationships which could also be written as facts, but this is a tedious process. Assuming traditional marriage and child-bearing practices, we could write a few rules which would relieve the tedium of identifying and listing all the possible family relations. For example, say you wanted to know if `johnette` had any siblings, the first question you must ask is "what does it mean to be a sibling?" To be someone's sibling you must have the same parent. This last sentence can be written in Prolog as

```prolog
siblingof(X,Y) :-
        parentof(Z,X),
        parentof(Z,Y).
```

A translation of the above Prolog rule into English would be "X is the sibling of Y provided that Z is a parent of X, and Z is a parent of Y." `X, Y,` and `Z` are variables. This rule however, also defines a child to be its own sibling. To correct this we must add that `X` and `Y` are not the same. The corrected version is:

```prolog
siblingof(X,Y) :- 
        parentof(Z,X),
        parentof(Z,Y),
        X  Y.
```

The relation `brotherof` is similar but adds the condition that `X` must be a male.

```prolog
brotherof(X,Y) :-
        parentof(Z,X),
        male(X),
        parentof(Z,Y),
        X  Y.
```

From these examples we see how to construct facts, rules and queries and that strings are enclosed in single quotes, variables begin with a capital letter, constants are either enclosed in single quotes or begin with a small letter.

## <a name="types"></a>Types

Prolog provides for numbers, atoms, lists, tuples, and patterns. The types of objects that can be passed as arguments are defined in this section.

### <a name="simple"></a>Simple Types

Simple types are implementation dependent in Prolog however, most implementations provide the simple types summarized in the following table.


| TYPE     | VALUES                 |
|----------|------------------------|
| boolean  | true, fail             |
| integer  | integers               |
| real     | floating point numbers |
| variable | variables              |
| atom     | character sequences    |

The boolean constants are not usually passed as parameters but are propositions. The constant `fail` is useful in forcing the generation of all solutions. Variables are character strings beginning with a capital letter. Atoms are either quoted character strings or unquoted strings beginning with a small letter.

### <a name="composite"></a>Composite Types

In Prolog the distinction between programs and data are blurred. Facts and rules are used as data and data is often passed in the arguments to the predicates. Lists are the most common data structure in Prolog. They are much like the array in that they are a sequential list of elements, and much like the stack in that you can only access the list of elements sequentially, that is, from one end only and not in random order. In addition to lists Prolog permits arbitrary patterns as data. The patterns can be used to represent tuples. Prolog does not provide an array type. But arrays may be represented as a list and multidimensional arrays as a list(s) of lists. An alternate representation is to represent an array as a set of facts in a the data base.

<center>

<table><caption>

<center>

TYPE

</center>

</caption>

<tbody>

<tr>

<td>REPRESENTATION  
list</td>

<td>[ _comma separated sequence of items_ ]  
pattern</td>

<td>_sequence of items_  
</td>

</tr>

</tbody>

</table>

</center>

A list is designated in Prolog by square brackets ([ ]+). An example of a list is

```prolog
[dog,cat,mouse]
```

This says that the list contains the elements `dog`, {\tt cat, and `mouse`, in that order. Elements in a Prolog list are ordered, even though there are no indexes. Records or tuples are represented as patterns. Here is an example.

```prolog
book(author(aaby,anthony),title(labmanual),data(1991))
```

The elements of a tuple are accessed by pattern matching.

```prolog
book(Title,Author,Publisher,Date).
author(LastName,FirstName,MI).
publisher(Company,City).
```

```prolog
book(T,A,publisher(C,rome),Date)
```

### <a name=""></a>Type Predicates

Since Prolog is a weakly typed language, it is important for the user to be able to determine the type of a parameter. The following built in predicates are used to determine the type of a parameter.

| PREDICATE | CHECKS IF |
|-----------|-----------|
| `var(V)`	        | `V` is a variable              |
| `nonvar(NV)`      | `NV` is not a variable         |
| `atom(A)`	        | `A` is an atom                 |
| `integer(I)`      | `I` is an integer              |
| `real(R)`	        | `R` is a floating point number |
| `number(N)`	    | `N` is an integer or real      |
| `atomic(A)`	    | `A` is an atom or a number     |
| `functor(T, F,A)` |	`T` is a term with functor `F` and arity `A`    |
| `T =..L`	        | `T` is a term, `L` is a list (see example below). |
| `clause(H,T)`     | `H :- T` is a rule in the program                 |

The last three are useful in program manipulation (metalogical or meta-programming) and require additional explanation. `clause(H,T)` is used to check the contents of the data base. `functor(T,F,A)` and `T=..L` are used to manipulate terms. The predicate, `functor` is used as follows.

```prolog
functor(T,F,A)
```

`T` is a term, `F` is its functor, and `A` is its arity. For example,

```prolog
?- functor(t(a,b,c),F,A).
F = t
A = 3
yes
```

`t` is the functor of the term `t(a,b,c)`, and 3 is the arity (number of arguments) of the term. The predicate `=..` (univ) is used to compose and decompose terms. For example:

```prolog
?- t(a,b,c) =..L.
L = [t,a,b,c]
yes
?- T =..[t,a,b,c].
T = t(a,b,c)
yes
```

## <a name="expr"></a>Expressions

Arithmetic expressions are evaluated with the built in predicate `is` which is used as an infix operator in the following form.

```prolog
_variable_ is _expression_
```

For example,

```prolog
?- X is 3*4.
X = 12
yes
```

### <a name=""></a>Arithmetic Operators

Prolog provides the standard arithmetic operations as summarized in the following table.

| SYMBOL | OPERATION        |
|--------|------------------|
| +	     | addition         |
| -	     | subtraction      |
| *	     | multiplication   |
| /	     | real division    |
| //	 | integer division |
| mod	 | modulus          |
| **	 | power            |

### <a name=""></a>Boolean Predicates

Besides the usual boolean predicates, Prolog provides more general comparison operators which compare terms and predicates to test for unifiability and whether terms are identical.


| SYMBOL   | OPERATION     | ACTION |
|----------|---------------|--------|
| A ?= B   | unifiable     | A and B are unifiable but does not unify A and B
| A = B    | unify         | unifys A and B if possible
| A \+= B  | not unifiable | 
| A == B   | identical     | does not unify A and B
| A \+== B | not identical | 
| A =:= B  | equal (value) | evaluates A and B to determine if equal
| A =\+= B | not equal (value)          |
| A < B    | less than (numeric)        |
| A =< B   | less or equal (numeric)    |
| A > B    | greater than (numeric)     |
| A >= B   | greater or equal (numeric) |
| A @< B   | less than (terms)          |
| A @=< B  | less or equal (terms)      |
| A @> B   | greater than (terms)       |
| A @>= B  | greater or equal (terms)   |

For example, the following are all true.

```prolog
3 @< 4
3 @< a
a @< abc6
abc6 @< t(c,d)
t(c,d) @< t(c,d,X)
```

Logic programming definition of natural number.

```prolog
% natural_number(N) <- N is a natural number.

natural_number(0).
natural_number(s(N)) :- natural_number(N).
```

Prolog definition of natural number.

```prolog
natural_number(N) :- integer(N), N >= 0.
```

Logic programming definition of inequalities

```prolog
% less_than(M,N) <- M is less than M

less_than(0,s(M)) :- natural_number(M).
less_than(s(M),s(N)) :- less_than(M,N).

% less_than_or_equal(M,N) <- M is less than or equal to M

less_than_or_equal(0,N) :- natural_number(N).
less_than_or_equal(s(M),s(N)) :- less_than_or_equal(M,N).
```

Prolog definition of inequality.

```prolog
M =< N.
```

Logic programming definition of addition/substraction

```prolog
% plus(X,Y,Z) <- Z is X + Y

plus(0,N,N) :- natural_number(N).
plus(s(M),N,s(Z)) :- plus(M,N,Z).
```

Prolog definition of addition

```prolog
plus(M,N,Sum) :- Sum is M+N.
```

This does not define substration. Logic programming definition of multiplication/division

```prolog
% times(X,Y,Z) <- Z is X*Y 

times(0,N,0) :- natural_number(N).
times(s(M),N,Z) :- times(M,N,W), plus(W,N,Z).
```

Prolog definition of multiplication.

```prolog
times(M,N,Product) :- Product is M*N.
```

This does not define substration. Logic programming definition of Exponentiation

```prolog
% exp(N,X,Z) <- Z is X**N

exp(s(M),0,0) :- natural_number(M).
exp(0,s(M),s(0)) :- natural_number(M).
exp(s(N),X,Z) :- exp(N,X,Y), times(X,Y,Z).
```

Prolog definition of exponentiation is implementation dependent.

### <a name=""></a>Logical Operators

Predicates are functions which return a boolean value. Thus the logical operators are built in to the language. The comma on the right hand side of a rule is logical conjunction. The symbol `:-` is logical implication. In addition Prolog provides negation and disjunction operators. The logical operators are used in the definition of rules. Thus,

```prolog
a :- b. % a if b
a :- b,c. % a if b and c.
a :- b;c. % a if b or c.
a :- \++ b. % a if b is not provable
a :- not b. % a if b fails
a :- b -> c;d. % a if (if b then c else d)
```

This table summarizes the logical operators.

| SYMBOL | OPERATION           |
|--------|---------------------|
| `not`  | negation            |
| `\+`   | not provable        |
| `,`    | logical conjunction |
| `;`    | logical disjunction |
| `:-`   | logical implication |
| `->`   | if-then-else        |

## <a name="pat"></a>Unification and Pattern Matching

The arguments in a query are matched (or unified in Prolog terminology) to select the appropriate rule. Here is an example which makes extensive use of pattern matching. The rules for computing the derivatives of polynomial expressions can be written as Prolog rules. A given polynomial expression is matched against the first argument of the rule and the corresponding derivative is returned.

```prolog
% deriv(_Polynomial_, _variable_, _derivative_)
% dc/dx = 0
deriv(C,X,0) :- number(C). 
% dx/dx} = 1
deriv(X,X,1).
% d(cv)/dx = c(dv/dx)
deriv(C*U,X,C*DU) :- number(C), deriv(U,X,DU).
% d(u v)/dx = u(dv/dx) + v(du/dx)
deriv(U*V,X,U*DV + V*DU) :- deriv(U,X,DU), deriv(V,X,DV).
% d(u ± v)/dx = du/dx ± dv/dx
deriv(U+V,X,DU+DV) :- deriv(U,X,DU), deriv(V,X,DV).
deriv(U-V,X,DU-DV) :- deriv(U,X,DU), deriv(V,X,DV).
% du^n/dx = nu^{n-1}(du/dx)
deriv(U^+N,X,N*U^+N1*DU) :- N1 is N-1, deriv(U,X,DU).
```

Prolog code is often bidirectional. In bidirectional code, the arguments may be use either for input or output. For example, this code may be used for both differentiation and integration with queries of the form:

```prolog
?- deriv(_Integral_,_X_,_Derivative_).
```

where either _Integral_ or _Derivative_ may be instantiated to a formula.

## <a name="fun"></a>Functions

Prolog does not provide for a function type therefore, functions must be defined as relations. That is, both the arguments to the function and the result of the function must be parameters to the relation. This means that composition of two functions cannot be constructed. As an example, here is the factorial function defined as relation in Prolog. Note that the definition requires two rules, one for the base case and one for the inductive case.

```prolog
fac(0,1).
fac(N,F) :- N 0, M is N - 1,
        fac(M,Fm), F is N * Fm.
```

The second rule states that if `N` 0, `M = N - 1`, `Fm` is `(N-1)!`, and `F = N * Fm`, then `F` is `N!`. Notice how "is" is used. In this example it resembles an assignment operator however, it may not be used to reassign a variable to a new value. I the logical sense, the order of the clauses in the body of a rule are irrelevant however, the order may matter in a practical sense. `M` must not be a variable in the recursive call otherwise an infinite loop will result. Much of the clumsiness of this definition comes from the fact that `fac` is defined as a relation and thus it cannot be used in an expression. Relations are commonly defined using multiple rules and the order of the rules may determine the result. In this case the rule order is irrelevant since, for each value of `N` only one rule is applicable. Here are the Prolog equivalent of the definitions of the gcd function, Fibonacci function and ackerman's function.

```prolog
gcd(A,B,GCD) :- A = B, GCD = A.
gcd(A,B,GCD) :- A < B, NB is B - A, gcd(A,NB,GCD).
gcd(A,B,GCD) :- A B, NA is A - B, gcd(NA,B,GCD).
```

```prolog
fib(0,1).
fib(1,1).
fib(N,F) :- N 1, N1 is N - 1, N2 is N - 2,
    fib(N1,F1), fib(N2,F2), F is F1 + F2.
```

```prolog
ack(0,N,A) :- A is N + 1.
ack(M1,0,A) :- M 0, M is M - 1, ack(M,1,A).
ack(M1,N1,A) :- M1 0, N1 0, M is M - 1, N is N - 1,
    ack(M1,N,A1), ack(M,A1,A).
```

Notice that the definition of ackerman's function is clumsier than the corresponding functional definition since the functional composition is not available. Logic programming definition of the factorial function.

```prolog
% factorial(N,F) <- F is N!

factorial(0,s(0)).
factorial(s(N),F) :- factorial(N,F1), times(s(N),F1,F).
```

Prolog definition of factorial function.

```prolog
factorial(0,1).
factorial(N,F) :- N1 is N-1, factorial(N1,F1), F is N*F1.
```

Logic programming definition of the minimum.

```prolog
% minimum(M,N,Min) <- Min is the minimum of {M, N}

minimum(M,N,M) :- less_than_or_equal(M,N).
minimum(M,N,N) :- less_than_or_equal(N,M).
```

Prolog programming definition of the minimum.

```prolog
minimum(M,N,M) :- M =< N.
minimum(M,N,N) :- N =< M.
```

Logic programming definition of the modulus.

```prolog
% mod(M,N,Mod) <- Mod is the remainder of the integer division of M by N.

mod(X,Y,Z) :- less_than(Z,Y), times(Y,Q,W), plus(W,Z,X).

% or
mod(X,Y,X) :- less_than(X,Y).
mod(X,Y,X) :- plus(X1,Y,X), mod(X1,Y,Z).
```

Logic programming definition of Ackermann's function.

```prolog
ack(0,N,s(N)).
ack(s(M),0,Val) :- ack(M,s(0),Val).
ack(s(M),s(N),Val) :- ack(s(M),N,Val1), ack(M,Val1,Val).
```

Prolog definition of Ackermann's function.

```prolog
ack(0,N,Val) :- Val is N + 1.
ack(M,0,Val) :- M 0, M1 is M-1, ack(M1,1,Val).
ack(M,N,Val) :- M 0, N 0, M1 is M-1, N1 is N-1, 
                ack(M,N1,Val1), ack(M1,Val1,Val).
```

Logic programming definition of the Euclidian algorithm.

```prolog
gcd(X,0,X) :- X 0.
gcd(X,Y,Gcd) :- mod(X,Y,Z), gcd(Y,Z,Gcd).
```

Logic programming definition of the Euclidian algorithm.

```prolog
gcd(X,0,X) :- X 0.
gcd(X,Y,Gcd) :- mod(X,Y,Z), gcd(Y,Z,Gcd).
```

## <a name="list"></a>Lists

Objective  
Outline
*   Lists
*   Composition of Recursive Programs
*   Iteration

Lists are the basic data structure used in logic (and functional) programming. Lists are a recursive data structure so recursion occurs naturally in the definitions of various list operations. When defining operations on recursive data structures, the definition most often naturally follows the recursive definition of the data structure. In the case of lists, the empty list is the base case. So operations on lists must consider the empty list as a case. The other cases involve a list which is composed of an element and a list.

Here is a recursive definition of the list data structure as found in Prolog.

```prolog
List --> [ ]
List --> [Element|List]
```

Here are some examples of list representation, the first is the empty list.

```prolog
Pair Syntax                Element Syntax
 [ ]             [ ] 
 [a|[ ]]         [a] 
 [a|b|[ ]]       [a,b] 
 [a|X]           [a|X] 
 [a|b|X]         [a,b|X] 
```

Predicates on lists are often written using multiple rules. One rule for the empty list (the base case) and a second rule for non empty lists. For example, here is the definition of the predicate for the length of a list.

```prolog
% length(List,Number) <- Number is lenght of List

length([],0).
length([H|T],N) :- length(T,M), N is M+1.
```

Element of a list.

```prolog
% member(Element,List) <- Element is an element of the list List

member(X,[X|List).
member(X,[Element|List]) :- member(X,List).
```

Prefix of a list.

```prolog
% prefix(Prefix,List) <- Prefix is a prefix of list List

prefix([],List).
prefix([X|Prefix],[X|List]) :- prefix(Prefix,List).
```

Suffix of a list.

```prolog
% suffix(Suffix,List) <- Suffix is a suffix of list List

suffix(Suffix,Suffix).
prefix(Suffix,[X|List]) :- suffix(Suffix,List).
```

Append (concatenate) two lists.

```prolog
% append(List1,List2,List1List2) <- 
%   List1List2 is the result of concatenating List1 and List2.

append([],List,List).
append([Element|List1],List2,[Element|List1List2]) :- 
append(List1,List2,List1List2).
```

Compare this code with the code for plus. sublist -- define using

*   Suffix of a prefix
*   Prefix of a suffix
*   Recursive definition of sublist using prefix
*   Suffix of a prefix using append
*   Prefix of a suffix using append

member, prefix and suffix -- defined using append reverse, delete, select, sort, permutation, ordered, insert, quicksort.

### Iteration

Iterative version of Length

```prolog
% length(List,Number) <- Number is lenght of List
% Iterative version.

length(List,LenghtofList) :- length(List,0,LengthofList).

% length(SufixList,LengthofPrefix,LengthofList) <- 
%         LengthofList is LengthofPrefix + length of SufixList

length([],LenghtofPrefix,LengthofPrefix).
length([Element|List],LengthofPrefix,LengthofList) :- 
PrefixPlus1 is LengthofPrefix + 1, 
length(List,PrefixPlus1,LengthofList).
```

Iterative version of Reverse

```prolog
% reverse(List,ReversedList) <- ReversedList is List reversed.
% Iterative version.

reverse(List,RList) :- reverse(List,[],RList).

% length(SufixList,LengthofPrefix,LengthofList) <- 
%         LengthofList is LengthofPrefix + length of SufixList

reverse([],RL,RL).
reverse([Element|List],RevPrefix,RL) :- 
reverse(List,[Element|RevPrefix],RL).
```

Here are some simple examples of common list operations defined by pattern matching. The first sums the elements of a list and the second forms the product of the elements of a list.

```prolog
sum([ ],0).
sum([X|L],Sum) :- sum(L,SL), Sum is X + SL.

product([ ],1).
product([X|L],Prod) :- product(L,PL), Prod is X * PL.
```

Another example common list operation is that of appending or the concatenation of two lists to form a third list. Append may be described as the relation between three lists, L1, L2, L3, where L1 = [x<sub>1</sub>,...,x<sub>m</sub>], L2 = [y<sub>1</sub>,...,y<sub>n</sub>] and L3 = [x<sub>1</sub>,...,x<sub>m</sub>,y<sub>1</sub>,...,y<sub>n</sub>]. In Prolog, an inductive style definition is required.

```prolog
append([ ],L,L). 
append([X1|L1],L2, [X1|L3]) :- append(L1,L2,L3).
```

The first rule is the base case. The second rule is the inductive case. In effect the second rule says that

<pre>
if L1 = [x<sub>2</sub>,...,x<sub>m</sub>],
      L2 = [y<sub>1</sub>,...,y<sub>n</sub>] and 
      L3 = [x<sub>2</sub>,...,x<sub>m</sub>,y<sub>1</sub>,...,y<sub>n</sub>],
then [x<sub>1</sub>,x<sub>2</sub>,...,x<sub>m</sub>,y<sub>1</sub>,...,y<sub>n</sub>], is the result of
appending  [x<sub>1</sub>,x<sub>2</sub>,...,x<sub>m</sub>] and L2.
</pre>

The append relation is quite flexible. It can be used to determine if an object is an element of a list, if a list is a prefix of a list and if a list is a suffix of a list.

```prolog
member(X,L) :- append(_,[X|_],L).
prefix(Pre,L) :- append(Prefix,_,L).
suffix(L,Suf) :- append(_,Suf,L).
```

The underscore (_+) in the definitions denotes an anonymous variable (or don`t care) whose value in immaterial to the definition. The member relation can be used to derive other useful relations.

```prolog
vowel(X) :- member(X,[a,e,i,o,u]).
digit(D) :- member(D,['0','1','2','3','4','5','6','7','8','9']).
```

A predicate defining a list and its reversal can be defined using pattern matching and the append relation as follows.

```prolog
reverse([ ],[ ]).
reverse([X|L],Rev) :- reverse(L,RL), append(RL,[X],Rev).
```

Here is a more efficient (iterative/tail recursive) version.

```prolog
reverse([ ],[ ]).
reverse(L,RL) :- reverse(L,[ ],RL).

reverse([ ],RL,RL).
reverse([X|L],PRL,RL) :- reverse(L,[X|PRL],RL).
```

To conclude this section, here is a definition of insertion sort.

```prolog
isort([ ],[ ]).
isort([X|UnSorted],AllSorted) :- isort(UnSorted,Sorted),
                                 insert(X,Sorted,AllSorted).

insert(X,[ ],[X]).
insert(X,[Y|L],[X,Y|L]) :- X =< Y.
insert(X,[Y|L],[Y|IL]) :-  X Y, insert(X,L,IL).
```

## <a name="iteration"></a>Iteration

Recursion is the only iterative method available in Prolog. However, tail recursion can often be implemented as iteration. The following definition of the factorial function is an 'iterative' definition because it is 'tail recursive.' It corresponds to an implementation using a while-loop in an imperative programming language.

```prolog
fac(0,1).
fac(N,F) :- N 0, fac(N,1,F).

fac(1,F,F).
fac(N,PP,F) :- N 1, NPp is N*PP, M is N-1, 
       fac(M,NPp,F). 
```

Note that the second argument functions as an _accumulator_. The accumulator is used to store the partial product much as might be done is a procedural language. For example, in Pascal an iterative factorial function might be written as follows.

```pascal
function fac(N:integer) : integer;
var i : integer;
begin
   if N >= 0 then begin
       fac := 1
      for I := 1 to N do
           fac := fac * I
   end
end;
```

In the Pascal solution `fac` acts as an accumulator to store the partial product. The Prolog solution also illustrates the fact that Prolog permits different relations to be defined by the same name provided the number of arguments is different. In this example the relations are fac/2 and fac/3 where fac is the ``functor" and the number refers to the arity of the predicate. As an additional example of the use of accumulators, here is an iterative (tail recursive version) of the Fibonacci function.

```prolog
fib(0,1).
fib(1,1).
fib(N,F) :- N 1, fib(N,1,1,F)

fib(2,F1,F2,F) :- F is F1 + F2.
fib(N,F1,F2,F) :- N 2, N1 is N - 1, NF1 is F1 + F2,
    fib(N1,NF1,F1,F).
```

## <a name="itgenback"></a>Iterators, Generators and Backtracking

The following fact and rule can be used to generate the natural numbers. % Natural Numbers

```prolog
nat(0).
nat(N) :- nat(M), N is M + 1.
```

The successive numbers are generated by backtracking. For example, when the following query is executed successive natural numbers are printed.

```prolog
?- nat(N), write(N), nl, fail.
```

The first natural number is generated and printed, then `fail` forces backtracking to occur and the second rule is used to generate the successive natural numbers. The following code generates successive prefixes of an infinite list beginning with N.

```prolog
natlist(N,[N]).
natlist(N,[N|L]) :- N1 is N+1, natlist(N1,L).
```

As a final example, here is the code for generating successive prefixes of the list of prime numbers.

```prolog
primes(PL) :- natlist(2,L2), sieve(L2,PL).

sieve([ ],[ ]).
sieve([P|L],[P|IDL]) :- sieveP(P,L,PL), sieve(PL,IDL).

sieveP(P,[ ],[ ]). 
sieveP(P,[N|L],[N|IDL]) :- N mod P   0, sieveP(P,L,IDL).
sieveP(P,[N|L],   IDL)  :- N mod P =:= 0, sieveP(P,L,IDL).
```

Occasionally, backtracking and multiple answers are annoying. Prolog provides the cut symbol (!) to control backtracking. The following code defines a predicate where the third argument is the maximum of the first two.

```prolog
max(A,B,M) :- A <  B, M = B.
max(A,B,M) :- A >= B, M = A.
```

The code may be simplified by dropping the conditions on the second rule.

```prolog
max(A,B,B) :- A <  B.
max(A,B,A).
```

However, in the presence of backtracking, incorrect answers can result as is shown here.

```prolog
?- max(3,4,M).

M = 4;

M = 3
```

To prevent backtracking to the second rule the cut symbol is inserted into the first rule.

```prolog
max(A,B,B) :- A < B.!.
max(A,B,A).
```

Now the erroneous answer will not be generated. A word of caution: cuts are similar to gotos in that they tend to increase the complexity of the code rather than to simplify it. In general the use of cuts should be avoided.

## <a name="tuples"></a>Tuples ( or Records)

We illustrate the data type of tuples with the code for the abstract data type of a binary search tree. The binary search tree is represented as either `nil` for the empty tree or as the tuple `btree(Item,L_Tree,R_Tree)`. Here is the Prolog code for the creation of an empty tree, insertion of an element into the tree, and an in-order traversal of the tree.

```prolog
create_tree(niltree).

inserted_in_is(Item,niltree, btree(Item,niltree,niltree)).

inserted_in_is(Item,btree(ItemI,L_T,R_T),Result_Tree) :- 
    Item @< ItemI,
    inserted_in_is(Item,L_Tree,Result_Tree).

inserted_in_is(Item,btree(ItemI,L_T,R_T),Result_Tree) :- 
    Item @ItemI,
    inserted_in_is(Item,R_Tree,Result_Tree).

inorder(niltree,[ ]).
inorder(btree(Item,L_T,R_T),Inorder) :- 
          inorder(L_T,Left),
          inorder(R_T,Right),
          append(Left,[Item|Right],Inorder).
```

The membership relation is a trivial modification of the insert relation. Since Prolog access to the elements of a tuple are by pattern matching, a variety of patterns can be employed to represent the tree. Here are some alternatives.

```prolog
[Item,LeftTree,RightTree]
Item/LeftTree/RightTree
(Item,LeftTree,RightTree)
```

## <a name="extralogical"></a>Extra-Logical Predicates

Objective  
Outline
*   Input/Output
*   Assert/Retract
*   System Access

The class of predicates in Prolog that lie outside the logic programming model are called _extra-logical_ predicates. These predicates achieve a side effect in the course of being satisfied as a logical goal. There are three types of extra-logical predicates, predicates for handling I/O, predicates for manipulating the program, and predicates for accessing the underlying operating system.

### <a name="io"></a>Input/Output

Most Prolog implementations provide the predicates `read` and `write`. Both take one argument, `read` unifies its argument with the next term (terminated with a period) on the standard input and `write` prints its argument to the standard output. As an illustration of input and output as well as a more extended example, here is the code for a checkbook balancing program. The section beginning with the comment ``Prompts" handles the I/0\.

```prolog
% Check Book Balancing Program.
checkbook :- initialbalance(Balance),
   newbalance(Balance).

% Recursively compute new balances 
newbalance(OldBalance) :- transaction(Transaction),
   action(OldBalance,Transaction).

% If transaction amount is 0 then finished.
action(OldBalance,Transaction) :- Transaction = 0, 
   finalbalance(OldBalance). %
%
```

```prolog
% If transaction amount is not 0 then compute new balance.
action(OldBalance,Transaction) :- Transaction \+= 0, 
   NewBalance is OldBalance + Transaction,
   newbalance(NewBalance).  

%
```

```prolog
% Prompts 
initialbalance(Balance) :- write('Enter initial balance: '),
   read(Balance).

transaction(Transaction) :- 
   write('Enter Transaction, '),
    write('- for withdrawal, 0 to terminate): '),
   read(Transaction).
finalbalance(Balance) :- write('Your final balance is: '),
   write(Balance), nl.
```

#### Files

`see(File)`  
    Current input file is now File.

`seeing(File)`  
    File is unified with the name of the current input file.

`seen`  
    Closes the current input file.

`tell(File)`  
    Current output file is now File.

`telling(File)`  
    File is unified with the name of the current output file.

`told`  
    Closes the current output file.

#### Term I/O

`read(Term)`  
   Reads next full-stop (period) delimited term from the current input stream, if eof then returns the atom 'end_of_file'.

`write(Term)`  
    Writes a term to the current output stream.

`print(Term)`  
    Writes a term to the current output stream. Uses a user defined predicate portray/1 to write the term, otherwise uses write.

`writeq(Term)`  
    Writes a term to the current output stream in a form aceptable as input to read.</dd>

#### Character I/O

`get(N)`  
    N is the ASCII code of the next non-blank printable character on the current input stream. If end of file, then a -1 is returned.

`put(N)`  
    Puts the character corresponding to ASCII code N on the current output stream.

`nl`  
    Causes the next output to be on a new line.

`tab(N)`  
    N spaces are output to the current output stream.

#### Program Access

`consult(SourceFile)`  
    Loads SourceFile into the interpreter but, if a predicate is defined accross two or more files, consulting them will result in only the clauses in the file last consulted being used.

`reconsult(File)`  
    available in some systems.

#### Other

`name(Atom,ASCII_List)`  
    the conversion routine between lists of ASCII codes and atoms.

display, prompt

```prolog
% Read a sentence and return a list of words.

read_in([W|Ws]) :- get0(C), read_word(C,W,C1), rest_sent(W,C1,Ws).

% Given a word and the next character, read in the rest of the sentence

rest_sent(W,_,[]) :- lastword(W).
rest_sent(W,C,[W1|Ws]) :- read_word(C,W1,C1), rest_sent(W1,C1,Ws).

read_word(C,W,C1) :- single_character(C),!,name(W,[C]), get0(C1).
read_word(C,W,C2) :- in_word(C,NewC), get0(C1),
                    rest_word(C1,Cs,C2), name(W,[NewC|Cs]).
read_word(C,W,C2) :- get0(C1), read_word(C1,W,C2).

rest_word(C,[NewC|Cs],C2) :- in_word(C,NewC), !, get0(C1), 
rest_word(C1,Cs,C2).
rest_word(C,[],C).

% These are single character words.

single_character(33).      % !
single_character(44).      % ,
single_character(46).      % .
single_character(58).      % :
single_character(59).      % ;
single_character(63).      % ?

% These characters can appear within a word.

in_word(C,C) :- C 96, C < 123\.               % a,b,...,z
in_word(C,L) :- C 64, C < 91, L is C + 32\.   % A,B,...,Z
in_word(C,C) :- C 47, C < 58\.                % 0,1,...,9
in_word(39,39).                                % '
in_word(45,45).                                % -

% These words terminate a sentence.

lastword('.').
lastword('!').
lastword('?').
```

### Program Access and Manipulation

`clause(Head,Body)`  

`assert(Clause)`  
    adds clause to the end of the database

`asserta(Clause)`  
`retract(Clause_Head)`  
`consult(File_Name)`

### System Access

`system(Command)`  
    Execute Command in the operating system

## <a name="style"></a>Style and Layout

Objective  
Outline
*   Style and Layout
*   Debugging

Some conventions for comments.

*   Long comments should precede the code they refer to while short comments should be interspersed with the code itself.

*   Program comments should describe what the program does, how it is used (goal predicate and expected results), limitations, system dependent features, performance, and examples of using the program.

*   Predicate comments explain the purpose of the predicate, the meaning and relationship among the arguments, and any restrictions as to argument type.

*   Clause comments add to the description of the case the particular clause deals with and is usefull for documenting cuts.

Some conventions for program layout

*   Group clauses belonging to a relation or ADT together.

*   Clauses should be short. Their body should contain no more than a few goals.

*   Make use of indentation to improve the readability of the body of a clause.

*   Mnemonic names for relations and variables should be used. Names should indicate the meaning of relations and the role of data objects.

*   Clearly separate the clauses defining different relations.

*   The cut operator should be used with care. The use of `red' cuts should be limited to clearly defined mutually exclusive alternatives.

Illustration

```prolog
merge( List1, List2, List3 ) :-
 ( List1 = [], !, List3 = List2 );
 ( List2 = [], !, List3 = List1 );
 ( List1 = [X|L1], List2 = [Y|L2 ),
((X < Y, ! Z = X, merge( L1, List2, L3 ) );
( Z = Y, merge( List1, L2, L3 ) )),
  List3 = [Z|L3].
```

A better version

```prolog
merge( [], List2, List2 ). 
merge( List1, [], List1 ). 

merge( [X|List1], [Y|List2], [X|List3] ) :-
X < Y, !,  merge( List1, List2, List3 ).  \% Red Cut
merge( List1, [Y|List2], [Y|List3] ) :-
 merge( List1, List2, List3 ).
```

### Debugging

trace/notrace, spy/nospy, programmer inserted debugging aids -- write predicates and p :- write, fail.

## <a name="negation"></a>Negation and Cuts

Objective  
Outline

*   Negation as failure
*   Green Cuts
*   Red Cuts

### Negation

### Cuts

#### Green cuts: Determinism

Selection among mutually exclusive clauses.

#### Tail Recursion Optimization

Prevention of backtracking when only one solution exists.

```prolog
A :- B1,...,Bn,Bn1.
A :- B1,...,Bn,!,Bn1\. % prevents backtracking
```

#### Red cuts: omitting explicit conditions

## <a name="dcg"></a>Definite Clause Grammars

Objective:  
Outline

*   The parsing problem: Context-free grammars; Construct a parse tree for a sentence given the context-free grammar.
*   Representing the Parsing Problem in Prolog
*   The Grammar Rule Notation] (Definite Clause Grammars -- DCG)
*   Adding Extra Arguments
*   Adding Extra Tests

Prolog originated from attempts to use logic to express grammar rules and formalize the parsing process. Prolog has special syntax rules which are called _definite clause grammars_ (DCG). DCGs are a generalization of context free grammars.

### Context Free Grammars

A context free grammar is a set of rules of the form:

```
<nonterminal> --> </nonterminal>
```

where `nonterminal` is a nonterminal and `body` is a sequence of one or more items. Each item is either a nonterminal symbol or a sequence of terminal symbols. The meaning of the rule is that the `body` is a possible form for an object of type `nonterminal`.

```
S --> a b
S --> a S b
```

### DCG

Nonterminals are written as Prolog atoms, the items in the body are separated with commas and sequences of terminal symbols are written as lists of atoms. For each nonterminal symbol, S, a grammar defines a language which is obtained by repeated nondeterministic application of the grammar rules, starting from S.

```prolog
s --> [a],[b].
s --> [a],s,[b].
```

As an illustration of how DCG are used, the string [a,a,b,b] is given to the grammar to be parsed.

```prolog
?- s([a,a,b,b],[]).  
   yes
```

Here is a natural language example.

```prolog
% DCGrammar

sentence --> noun_phrase, verb_phrase.

noun_phrase --> determiner, noun.
noun_phrase --> noun.

verb_phrase --> verb.
verb_phrase --> verb, noun_phrase.

% Vocabulary

determiner --> [the].
determiner --> [a].

noun --> [cat].
noun --> [cats].
noun --> [mouse].
noun --> [mice].

verb --> [scare].
verb --> [scares].
verb --> [hate].
verb --> [hates].
```

Context free grammars cannot define the required agreement in number between the noun phrase and the verb phrase. That information is context dependent (sensitive). However, DCG are more general Number agreement

```prolog
% DCGrammar - with number agreement between noun phrase and verb phrase

sentence --> noun_phrase(Number), verb_phrase(Number).

noun_phrase(Number) --> determiner(Number), noun(Number).
noun_phrase(Number) --> noun(Number).

verb_phrase(Number) --> verb(Number).
verb_phrase(Number) --> verb(Number), noun_phrase(Number1).

% Vocabulary

determiner(Number) --> [the].
determiner(singular) --> [a].

noun(singular) --> [cat].
noun(plural) --> [cats].
noun(singular) --> [mouse].
noun(plural) --> [mice].

verb(plural) --> [scare].
verb(singular) --> [scares].
verb(plural) --> [hate].
verb(singular) --> [hates].
```

### Parse Trees

```prolog
% DCGrammar -- with parse tree as a result

sentence(sentence(NP,VP)) --> noun_phrase(NP), verb_phrase(VP).

noun_phrase(noun_phrase(D,NP)) --> determiner(D), noun(NP).
noun_phrase(NP) --> noun(NP).

verb_phrase(verb_phrase(V)) --> verb(V).
verb_phrase(verb_phrase(V,NP)) --> verb(V), noun_phrase(NP).

% Vocabulary

determiner(determiner(the)) --> [the].
determiner(determiner(a)) --> [a].

noun(noun(cat)) --> [cat].
noun(noun(cats)) --> [cats].
noun(noun(mouse)) --> [mouse].
noun(noun(mice)) --> [mice].

verb(verb(scare)) --> [scare].
verb(verb(scares)) --> [scares].
verb(verb(hate)) --> [hate].
verb(verb(hates)) --> [hates].
```

### Simple Semantics for Natural Language Sentences

#### Transitive and intransitive verbs

```prolog
% DCGrammar -- Transitive and intransitive verbs

sentence(VP) --> noun_phrase(Actor), verb_phrase(Actor,VP).

noun_phrase(Actor) --> proper_noun(Actor).

verb_phrase(Actor,VP) --> intrans_verb(Actor,VP).
verb_phrase(Actor,VP) --> transitive_verb(Actor,Something,VP),
                             noun_phrase(Something).

% Vocabulary

proper_noun(john)  --> [john].
proper_noun(annie) --> [annie].

intrans_verb(Actor,paints(Actor))  --> [paints].

transitive_verb(Somebody,Something,likes(Somebody,Something)) --> [likes].
```

#### Determiners -- `a' and `every'

```prolog
:- op( 100, xfy, and).
:- op( 150, xfy, =>).

% DCGrammar -- Transitive and intransitive verbs

sentence(S) --> noun_phrase(X,Assn,S), verb_phrase(X,Assn).

noun_phrase(X,Assn,S) --> determiner(X,Prop,Assn,S), noun(X,Prop).

verb_phrase(X,Assn) --> intrans_verb(X,Assn).

% Vocabulary

determiner(X,Prop,Assn,exists(X,Prop and Assn)) --> [a].
determiner(X,Prop,Assn,    all(X,Prop =Assn)) --> [every].

noun(X,man(X))  --> [man].
noun(X,woman(X))  --> [woman].

intrans_verb(X,paints(X))  --> [paints].
intrans_verb(X,dances(X))  --> [dances].

```

#### Relative Clauses

### Interleaving syntax and semantics in DCG

```prolog
% Word level
sentence --> word(W), rest_sent(W).

rest_sent(W) --> {last_word(W)}.
rest_sent(_) --> word(W), rest_sent(W).

% Character level
word(W) --> {single_char_word(W)},   [W].
word(W) --> {multiple_char_word(W)}, [W].
```

```prolog
% Read a sentence and return a list of words.

sentence --> {get0(C)}, word(C,W,C1), rest_sent(C1,W).

% Given the next character and the previous word,
% read the rest of the sentence

rest_sent(C,W) --> {lastword(W)}.                  % empty
rest_sent(C,_) --> word(C,W,C1), rest_sent(C1,W).

word(C,W,C1) --> {single_character(C),!,name(W,[C]), get0(C1)}, [W].  % !,.:;?
word(C,W,C2) --> {in_word(C,Cp), get0(C1), rest_word(C1,Cs,C2),
                     name(W,[Cp|Cs])},[W].                         
word(C,W,C2) --> {get0(C1)}, word(C1,W,C2).                   % consume blanks

% These words terminate a sentence.

lastword('.').
lastword('!').
lastword('?').

% This reads the rest of the word plus the next character.

rest_word(C,[Cp|Cs],C2) :- in_word(C,Cp), get0(C1), rest_word(C1,Cs,C2).
rest_word(C,[],C).

% These are single character words.

single_character(33).      % !
single_character(44).      % ,
single_character(46).      % .
single_character(58).      % :
single_character(59).      % ;
single_character(63).      % ?

% These characters can appear within a word.

in_word(C,C) :- C 96, C < 123\.               % a,b,...,z
in_word(C,L) :- C 64, C < 91, L is C + 32\.   % A,B,...,Z
in_word(C,C) :- C 47, C < 58\.                % 0,1,...,9
in_word(39,39).                                % '
in_word(45,45).                                % -
```

a calculator!!

## <a name="inc"></a>Incomplete Data Structures

Objective  
Outline

*   Difference Lists
*   Dictionaries
*   Queue
*   QuickSort

An incomplete data structure is a data structure containing a variable. Such a data structure is said to be 'partially instantiated' or 'incomplete.' We illustrate the programming with incomplete data structures by modifying the code for a binary search tree. The resulting code permits the relation `inserted_in_is` to define both the insertion and membership relations. The empty tree is represented as a variable while a partially instantiated tree is represented as a tuple.

```prolog
create_tree(Niltree) :- var(Niltree). % Note: Nil is a variable

inserted_in_is(Item,btree(Item,L_T,R_T)).
     
inserted_in_is(Item,btree(ItemI,L_T,R_T)) :- 
    Item @< ItemI,
    inserted_in_is(Item,L_T).

inserted_in_is(Item, btree(ItemI,L_T,R_T)) :- 
    Item @ItemI,
    inserted_in_is(Item,R_T).

inorder(Niltree,[ ]) :- var(Niltree).
inorder(btree(Item,L_T,R_T),Inorder) :- 
          inorder(L_T,Left),
          inorder(R_T,Right),
          append(Left,[Item|Right],Inorder).
```

## <a name="meta"></a>Meta Level Programming

Meta-programs treat other programs as data. They analyze, transform, and simulate other programs. Prolog clauses may be passed as arguments, added and deleted from the Prolog data base, and may be constructed and then executed by a Prolog program. Implementations may require that the functor and arity of the clause be previously declared to be a dynamic type.

Objective  
Outline

*   Meta-logical Type Predicates
*   Assert/Retract
*   System Access

### Meta-Logical Type Predicates

`var(V)`  
Tests whether V is a variable.

`nonvar(NV)`  
Tests whether NV is a non-variable term.

`atom(A)`  
Tests whether A is an atom (non-variable term of arity 0 other than a number).

`integer(I)`  
Tests whether I is an integer.

`number(N)`  
Tests whether N is a number.

### Term Comparison

X = Y  
X == Y  
X =:= Y  

### The Meta-Variable Facility

`call(X)`  
this

### Assert/Retract

Here is an example illustrating how clauses may be added and deleted from the Prolog data base. The example shows how to simulate an assignment statement by using `assert` and `retract` to modify the association between a variable and a value.

```prolog
:- dynamic x/1 .% this may be required in some Prologs  

x(0).  % An initial value is required in this example

assign(X,V) :- Old =..[X,_], retract(Old),
                  New =..[X,V], assert(New).
```

Here is an example using the assign predicate.

```prolog
?- x(N).

N = 0
yes
?- assign(x,5).
yes
?- x(N).

N = 5
```

Here are three programs illustrating Prolog's meta programming capability. This first program is a simple interpreter for pure Prolog programs.

```prolog
% Meta Interpreter for pure Prolog

prove(true).
prove((A,B)) :- prove(A), prove(B).
prove(A) :- clause(A,B), prove(B).
```

Here is an execution of an append using the interpreter.

```prolog
 ?- prove(append([a,b,c],[d,e],F)).

F = [a,b,c,d,e]
```

It is no different from what we get from using the usual run time system. The second program is a modification of the interpreter, in addition to interpreting pure Prolog programs it returns the sequence of deductions required to satisfy the query.

```prolog
% Proofs for pure Prolog programs

proof(true,true).
proof((A,B),(ProofA,ProofB)) :- proof(A,ProofA), proof(B,ProofB).
proof(A,(A:-Proof)) :- clause(A,B), proof(B,Proof).
```

Here is a proof an append.

```prolog
?- proof(append([a,b,c],[d,e],F),Proof).

F = [a,b,c,d,e]
Proof = (append([a,b,c],[d,e],[a,b,c,d,e]) :-
           (append([b,c],[d,e],[b,c,d,e]) :-
           (append([c],[d,e],[c,d,e]) :- 
           (append([ ],[d,e],[d,e]) :- true))))
```

The third program is also a modification of the interpreter. In addition to interpreting pure Prolog programs, is a trace facility for pure Prolog programs. It prints each goal twice, before and after satisfying the goal so that the programmer can see the parameters before and after the satisfaction of the goal.

```prolog
% Trace facility for pure Prolog

trace(true).
trace((A,B)) :- trace(A), trace(B).
trace(A) :- clause(A,B), downprint(A), trace(B), upprint(A).

downprint(G) :- write('>'), write(G), nl.
upprint(G)   :- write('<'), write(G), nl.
```

Here is a trace of an append.

```prolog
?- trace(append([a,b,c],[d,e],F)).
>append([a,b,c],[d,e],[a|<sub>1</sub>427104])
>append([b,c],[d,e],[b|<sub>1</sub>429384])
>append([c],[d,e],[c|<sub>1</sub>431664])
>append([ ],[d,e],[d,e])
<append([ ],[d,e],[d,e])
<append([c],[d,e],[c,d,e])
<append([b,c],[d,e],[b,c,d,e])
<append([a,b,c],[d,e],[a,b,c,d,e])

F = [a,b,c,d,e]
```

#### Predictates for program manipulation

*   consult(_file name_)
*   var(_term_), nonvar(_term_), atom(_term_), integer(_term_), atomic(_term_)
*   functor(_Term_,_Functor_,_arity_), arg(_N_,_term_,_N-th arg_), _Term_ =.._List_
*   call(_Term_)
*   clause(_Head_,_Body_), assertz(_Clause_), retract(_Clause_)

## <a name="2ndOrder"></a>Second-Order Programming

Objective:  
    Second-Order Programming

Outline:

*   Setof, Bagof, Findall
*   Other second-order predicates
*   Applications

### Setof, Bagof and Findall

### Other second-order predicates

has_property, map_list, filter, foldr etc

*   Variable predicate names

```prolog
p(P,X,Y) :- P(X,Y).
```

```prolog
p(P,X,Y) :- R =..[P,X,Y], call(R).
```

For the following functions let S be the list [S_1,...,S_n].

1.  The function `map` where `map(f,S)` is [f(S_1),...,f(S_n)].
2.  The function `filter` where `filter(P,S)` is the list of elements of S that satisfy the predicate P.
3.  The function `foldl` where `foldl(Op,In,S)` which folds up S, using the given binary operator Op and start value In, in a left associative way, ie, foldl(op, r,[a,b,c]) = (((r op a) op b) op c).
4.  The function `foldr` where `foldr(Op,In,S)` which folds up S, using the given binary operator Op and start value In, in a right associative way, ie, foldr(op,r,[a,b,c]) = a op (b op (c op r)).
5.  The function `map2` is similar to `map`, but takes a function of two arguments, and maps it along two argument lists.
6.  The function `scan` where `scan(op, r, S)` applies `foldl op r)` to every initial segment of a list. For example `scan (+) 0 x)` computes running sums.
7.  The function `dropwhile` where `dropwhile(P,S)` which returns the suffix of S where each element of the prefex satisfies the predicate P.
8.  The function `takewhile` where `takewhile(P,S)` returns the list of initial element of S which satisfy P.
9.  The function `until` where `until(P,F,V)` returns the result of applying the function F to the value the smallest number of times necessary to satisfy the predicate. Example until (>1000) (2*) 1 = 1024
10.  The function `iterate` where `iterate(f,x)` returns the infinite list [x, f x, f(f x), ... ]
11.  Use the function `foldr` to define the functions, sum, product and reverse.
12.  Write a generic sort program, it should take a comparison function as a parameter.
13.  Write a generic transitive closer program, it should take a binary relation as a parameter.

### Applications

Generalized sort, transitive closure ...

```prolog
transitive_closure(Relation,Item1,Item2) :- Predicate =..[Relation,Item1,Item2],
                                            call(Predicate).
transitive_closure(Relation,Item1,Item2) :- Predicate =..[Relation,Item1,Link],
                                            call(Predicate),
transitive_closure(Relation,Link,Item2).
```

## <a name="database"></a>Database Programming

Objective:
    Logic Programming as Database Programming

Outline

*   Simple Family Database
*   Recursive Rules
*   Logic Programming and the Relational Database Model (relational algebra)

### Simple Databases

Basic predicates: `father/2,mother/2, male/1, female/1`.

```prolog
father(Father,Child).
mother(Mother,Child).
male(Person). 
female(Person).
son(Son,Parent).
daughter(Daughter,Parent).
parent(Parent,Child).
grandparent(Grandparent,Grandchild).
```

Question: Which should be facts and which should be rules? Example: if parent, male and female are facts then father and mother could be rules.

```prolog
father(Parent,Child) :- parent(Parent,Child), male(Parent).
mother(Parent,Child) :- parent(Parent,Child), female(Parent).
```

Some other relations that could be defined are.

```prolog
mother(Woman) :- mother(Woman,Child).
parents(Father,Mother) :- father(Father,Child), mother(Mother,Child).
brother(Brother,Sibling) :- parent(P,Brother), parent(P,Sibling),
    male(Brother), Brother  Sibling.
uncle(Uncle,Person) :- brother(Uncle,Parent), parent(Parent,Person).
sibling(Sib1,Sib2) :- parent(P,Sib1), parent(P,Sib2), Sib1 =\= Sib2.
cousin(Cousin1,Cousin2) :- parent(P1,Cousin1), parent(P2,Cousin2),
   sibling(P1,P2).
```

What about: sister, niece, full_ sibling, mother_in_law, etc.

### Recursive Rules

```prolog
ancestor(Ancestor,Descendent) :- parent(Ancestor,Descendent).
ancestor(Ancestor,Descendent) :- parent(Ancestor,Person),
   ancestor(Persion,Descendent).
```

The `ancestor` relation is an example of the more general relation of transitive closure. Here is an example of the transitive closure for graphs. Transitive closure: connected

```prolog
edge(Node1,Node2).
...
connected(Node1,Node2) :- edge(Node1,Node2).
connected(Node1,Node2) :- edge(Node1,Link), connected(Link,Node2).
```

### Logic programs and the relational database model

The mathematical concept underlying the relational database model is the set-theoretic _relation_, which is a subset of the Cartesian product of a list of domains. A domain is a set of values. A _relation_ is any subset of the Cartesian product of one or more domains. The members of a relation are called _tuples_. In relational databases, a relation is viewed as a table. The Prolog view of a relation is that of a set of named tuples. For example, in Prolog form, here are some unexpected entries in a city-state-population relation.

```prolog
city_state_population('San Diego','Texas',4490).
city_state_population('Miami','Oklahoma',13880).
city_state_population('Pittsburg','Iowa',509).
```

In addition to defining relations as a set of tuples, a relational database management system (DBMS) permits new relations to be defined via a query language. In Prolog form this means defining a rule. For example, the sub-relation consisting of those entries where the population is less than 1000 can be defined as follows:

```prolog
smalltown(Town,State,Pop) :- city_state_pop(Town,State,Pop), Pop < 1000.
```

One of the query languages for relational databases is the Relational Algebra. Its operations are union, set difference, Cartesian product, projection, and selection. They may be defined for two relations r and s as follows.

```prolog
% Union of relations r/n and s/n 
r_union_s(X1,...,Xn) :- r(X1,...,Xn).
r_union_s(X1,...,Xn) :- s(X1,...,Xn).

% Set Difference  r/n $\setminus$ s/n
r_diff_s(X1,...,Xn) :- r(X1,...,Xn), not s(X1,...,Xn).
r_diff_s(X1,...,Xn) :- s(X1,...,Xn), not r(X1,...,Xn).

% Cartesian product r/m, s/n
r_x_s(X1,...,Xm,Y1,...,Yn) :- r(X1,...,Xm), s(Y1,...,Yn).

% Projection
r_p_i_j(Xi,Xj) :- r(X1,...,Xn).

% Selection
r_c(X1,...,Xn) :- r(X1,...,Xn), c(X1,...,Xn).

% Meet
r_m_s(X1,...,Xn) :- r(X1,...,Xn), s(X1,...,Xn).

% Join
r_j_s(X'1,...,X'j,Y'1,...,Y'k) :- r(X1,...,Xn), s(Y1,...,Yn).
```

The difference between Prolog and a Relational DBMS is that the in Prolog the relations are stored in main memory along with the program whereas in a Relational DBMS the relations are stored in files and the program extracts the information from the files.

## <a name="expert"></a>Expert systems

Expert systems may be programmed in one of two ways in Prolog. One is to construct a knowledge base using Prolog facts and rules and use the built-in inference engine to answer queries. The other is to build a more powerful inference engine in Prolog and use it to implement an expert system.

Pattern matching: Symbolic differentiation

|  |  |  |
|--|--|--|
| d(X,X,1)             | :- | !.
| d(C,X,0)             | :- | atomic(C).
| d(-U,X,-A)           | :- | d(U,X,A).
| d(U+V,X,A+B)         | :- | d(U,X,A), d(V,X,B).
| d(U-V,X,A-B)         | :- | d(U,X,A), d(V,X,B).
| d(C*U,X,C*A)         | :- | atomic(C), CX, d(U,X,A) !.
| d(U*V,X,B*U+A*V)     | :- | d(U,X,A), D(V,X,B).
| d(U/V,X,A)           | :- | d(U*V^-1,X,A)
| d(U^C,X,C*U^(C-1)*W) | :- | atomic(C), CX, d(U,X,W).
| d(log(U),X,A*U^(-1)) | :- | d(U,X,A).

## <a name="oop"></a>Object-Oriented Programming

```prolog
object( Object, Methods )
```

```prolog
/******************************************************************************
                                    OOP
******************************************************************************/

/*=============================================================================
                        Interpreter for OOP
=============================================================================*/

send( Object, Message ) :- get_methods( Object, Methods ),
                           process( Message, Methods ).
                      
get_methods( Object, Methods ) :- object( Object, Methods ).
get_methods( Object, Methods ) :- isa( Object, SuperObject ),
                                  get_methods( SuperObject, Methods ).

process( Message, [Message|_] ).
process( Message, [(Message :- Body)|_] ) :- call( Body ).
process( Message, [_|Methods] ) :- process( Message, Methods ).

/*=============================================================================
                        Geometric Shapes
=============================================================================*/

object( polygon( Sides ), [ (perimeter( P ) :- sum( Sides, P )) ] ).

object( reg_polygon( Side, N ), [ ((perimeter( P ) :- P is N*Side)),
                                  (describe :- write('Regular polygon')) ] ).

                                          
object( rectangle( Length, Width ), 
                 [ (area( A ) :- A is Length * Width ),
                   (describe  :- write('Rectangle of size ' ),
                                 write( Length*Width)) ] ).

object( square( Side ), [ (describe :- write( 'Square with side ' ),
                                       write( Side )) ] ).

object( pentagon( Side ), [ (describe :- write('Pentagon')) ] ).

isa( square( Side ), rectangle( Side, Side ) ).
isa( square( Side ), reg_polygon( Side, 4 ) ).
isa( rectange( Length, Width ), polygon([Length, Width, Length, Width]) ).
isa( pentagon( Side ), reg_polygon( Side, 5 ) ).

isa( reg_polygon( Side, N ), polygon( L ) ) :- makelist( Side, N, L ).
```

## <a name="appendix"></a>Appendix

The entries in this appendix have the form: `pred/n definition` where `pred` is the name of the built in predicate, `n` is its arity (the number of arguments it takes), and `definition` is a short explanation of the function of the predicate.

### ARITHMETIC EXPRESSIONS

+, -, *, /, sin, cos, tan, atan, sqrt, pow, exp, log

### I/O

`see/1`  
    the current input stream becomes arg1

`seeing/1`  
    arg1 unifies with the name of the current input stream.

`seen/0`  
    close the current input stream

`tell/1`  
    the current output stream becomes arg1

`telling/1`  
    arg1 unifies with the name of the current output stream.

`told/0`  
    close current output stream

`read/1`
    arg1 is unified with the next term delimited with a period from the current input stream.

`get/1`
    arg1 is unified with the ASCII code of the next printable character in the current input stream.

`write/1`  
    arg1 is written to the current output stream.

`writeq/1`  
    arg1 is written to the current output stream so that it can be read with `read`.

`nl/0`
    an end-of-line character is written to the current output stream.

`spaces/1`
    arg1 number of spaces is written to the current output stream.

### PROGRAM STATE

`listing/0`
    all the clauses in the Prolog data base are written to the current output stream

`listing/1`
    all the clauses in the Prolog data base whose functor name is equal to arg1 are written to the current output stream

`clause(H,B)`  
    succeeds if H is a fact or the head of some rule in the data base and B is its body (true in case H is a fact).

### PROGRAM MANIPULATION

`consult/1`  
    the file with name arg1 is consulted (loaded into the Prolog data base)

`reconsult/1`  
    the file with name arg1 is reconsulted

`assert/1`  
    arg1 is interpreted as a clause and is added to the Prolog data base (functor must be dynamic)

`retract/1`  
    the first clause which is unifiable with arg1 is retracted from the Prolog data base (functor must be dynamic)

### META-LOGICAL

`ground/1`  
    succeeds if arg1 is completely instantiated (BIM)

`functor/3`  
    succeeds if arg1 is a term, arg2 is the functor, and arg3 is the arity of the term.

`T =..L`
    succeeds if T is a term and L is a list whose head is the principle functor of T and whose tail is the list of the arguments of T.

`name/2`  
    succeeds if arg1 is an atom and arg2 is a list of the ASCII codes of the characters comprising the name of arg1.

`call/1`  
    succeeds if arg1 is a term in the program.

`setof/3`
    arg3 is a set (list) of all instances of arg1 for which arg2 holds. Arg3 must be of the form X^T where X is an unbound variables in T other than arg1.

`bagof/3`  
    arg3 is a list of all instances of arg1 for which arg2 holds. See setof.

`\+/1`  
    succeeds if arg1 is not provable (Required instead of **not** in some Prologs if arg1 contains variables.

`not/1`  
    same as \+ but may requires arg1 to be completely instantiated

### SYSTEM CONTROL

`halt/0, C-d`  
    exit from Prolog

### DIRECTIVES

`:- dynamic pred/n .`
    the predicate pred of order n is dynamic

## <a name="ref"></a>References

-   Clocksin & Mellish, _Programming in Prolog_ 4th ed. Springer-Verlag 1994.
-   Hill, P. & Lloyd, J. W., _The Gödel Programming Language_ MIT Press 1994.
-   Hogger, C. J., _Introduction to Logic Programming_ Academic Press 1984.
-   Lloyd, J. W., _Foundations of Logic Programming_ 2nd ed. Springer-Verlag 1987.
-   Nerode, A. & Shore, R. A., _Logic for Applications_ Springer-Verlag 1993.
-   Robinson, J. A., _Logic: Form and Function_ North-Holland 1979.
-   Sterling and Shapiro, _The Art of Prolog_. MIT Press, Cambridge, Mass. 1986.

* * *

© 1996 by [A. Aaby](http://cs.wwc.edu/~cs_dept/KU/PR/Notices.html) Last Updated: Fri May 2 23:24:37 1997 Send comments to: [webmaster@cs.wwc.edu](mailto:webmaster@cs.wwc.edu)
