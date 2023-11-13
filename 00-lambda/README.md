[![Open in Codespaces](https://classroom.github.com/assets/launch-codespace-7f7980b617ed060a017424585567c406b6ee15c891e84e1186181d67ecf80aa0.svg)](https://classroom.github.com/open-in-codespaces?assignment_repo_id=12237004)
# Assignment 0: Lambda Calculus (points: 130 public / 150 private)

## Overview : Lambda Calculus

The objective of this assignment is for you to understand
the **lambda-calculus**, and the notion of computation-by-substitution
i.e. substituting equals for equals.

The assignment is in the files:

1. `tests/01_bool.lc`
2. `tests/02_plus.lc`
3. `tests/03_minus.lc`

You can edit these files and then run them,

* either through the [web interface](http://elsa.goto.ucsd.edu/index.html), OR
* by running `$ elsa path/to/file.lc` (e.g. `elsa tests/01_bool.lc`) on `codespaces`, OR
* by locally installing `elsa` following [these instructions](https://github.com/ucsd-progsys/elsa#install)

If you work on your code in the web interface, be sure to **copy back the result**
into the corresponding source file locally or on `codespaces`.
Whether you are working locally or on `codespaces`, 
do not forget to commit and push all your changes to your GitHub repo.

## Assignment Testing and Evaluation

All the points will be awarded automatically, by
**evaluating your functions against a given test suite**.

When you run

```shell
$ make
```

or

```shell
$ stack test
```

Your last lines should have

```
All N tests passed (...)
OVERALL SCORE = ... / ...
```

**or**

```
K out of N tests failed
OVERALL SCORE = ... / ...
```

**If your output does not have one of the above your code will receive a zero**

The other lines will give you a readout for each test.
You are encouraged to try to understand the testing code,
but you will not be graded on this.

## Submission Instructions

Submit your code via the HW-0 assignment on Gradescope.
Connect your Github account to Gradescope and select your repo.
*If you're in a group, don't forget to add your partner to the submission!*

Detailed instructions on how to submit your code directly from the Git repo can be found on [Piazza](https://piazza.com/class/lfyrhbkf5nw3os/post/9).

## Problem 1: `01_bool.lc`

**NOTE: DO NOT** use the `=*>` or `=~>` operators
anywhere in your solution for this problem, or you
will get 0 points for the assignment.

**NOTE: YOU MAY** replace `=d>` with `=b>` in the
last line.

**REMARK**: For problems 1 and 2, when using `=d>`, you don't need to unfold
every definition. It is often easier to keep some definitions folded until
their code is needed.

### Part (a) (5 points)

Complete the sequence of `=a>`, `=b>` and `=d>`
steps needed to reduce `NOT TRUE` to `FALSE`.


### Part (b) (5 points)

Complete the sequence of `=a>`, `=b>` and `=d>`
steps needed to reduce `AND TRUE FALSE` to `FALSE`.


### Part (c) (5 points)

Complete the sequence of `=a>`, `=b>` and `=d>`
steps needed to reduce `OR FALSE TRUE` to `TRUE`.


## Problem 2: `02_plus.lc`

**NOTE: DO NOT** use the `=*>` or `=~>` operators
anywhere in your solution for this problem, or you
will get 0 points for the assignment.

**NOTE: YOU MAY** replace `=d>` with `=b>` in the
last line.


### Part (a) (5 points)

Complete the sequence of `=a>`, `=b>` and `=d>`
steps needed to reduce `INC ONE` to `TWO`.

### Part (b) (5 points)

Complete the sequence of `=a>`, `=b>` and `=d>`
steps needed to reduce `ADD ZERO ZERO` to `ZERO`.

### Part (c) (5 points)

Complete the sequence of `=a>`, `=b>` and `=d>`
steps needed to reduce `ADD TWO TWO` to `FOUR`.

## Problem 3: `03_minus.lc`

**NOTE:** You only need to write lambda-calculus
definitions for `SKIP1`, `DEC`, `SUB`, `ISZ` and `EQL`.
If you modify **any other** other part of the file
you will get 0 points for the assignment.

### Part (a) (30 points)

Replace the definition of `SKIP1` with a suitable
lambda-term (i.e. replace `TODO` with a suitable
term) so that the following reductions are valid:

```haskell
eval skip1_false :
  SKIP1 INC (PAIR FALSE ZERO)
  =~> (\b -> b TRUE ZERO)         --  PAIR TRUE ZERO

eval skip1_true_zero :
  SKIP1 INC (PAIR TRUE ZERO)
  =~> (\b -> b TRUE ONE)          -- PAIR TRUE ONE

eval skip1_true_one :
  SKIP1 INC (PAIR TRUE ONE)
  =~> (\b -> b TRUE TWO)          -- PAIR TRUE TWO
```

`SKIP1` is a helper function used in part (b) below.
You are supposed to infer the intended meaning of `SKIP1`
from the examples above.

### Part (b) (30 points)

Replace the definition of `DEC` (decrement-by-one)
with a suitable lambda-term (i.e. replace `TODO`
with a suitable term) so that the following reductions
are valid:

```haskell
eval decr_zero :
  DEC ZERO
  =~> ZERO

eval decr_one :
  DEC ONE
  =~> ZERO

eval decr_two :
  DEC TWO
  =~> ONE
```

You **must** use `SKIP1` in your definition of `DEC`.

### Part (c) (10 points)

Replace the definition of `SUB` (subtract) with a
suitable lambda-term (i.e. replace `TODO`
with a suitable term) so that the following
reductions are valid:

```haskell
eval sub_two_zero :
  SUB TWO ZERO
  =~> TWO

eval sub_two_one :
  SUB TWO ONE
  =~> ONE

eval sub_two_two :
  SUB TWO TWO
  =~> ZERO

eval sub_two_three :
  SUB ONE TWO
  =~> ZERO
```

### Part (d) (20 points)

Replace the definition of `ISZ` (is-equal-to-zero)
with a suitable lambda-term (i.e. replace `TODO`
with a suitable term) so that the following
reductions are valid:

```haskell
eval isz_zero :
  ISZ ZERO
  =~> TRUE

eval isz_one :
  ISZ ONE
  =~> FALSE
```

### Part (e) (20 points)

Replace the definition of `EQL` (is-equal)
with a suitable lambda-term (i.e. replace
`TODO` with a suitable term) so that
the following reductions are valid:

```haskell
eval eq_zero_zero :
  EQL ZERO ZERO
  =~> TRUE

eval eq_zero_one :
  EQL ZERO ONE
  =~> FALSE

eval eq_one_two :
  EQL ONE TWO
  =~> FALSE

eval eq_two_two :
  EQL TWO TWO
  =~> TRUE
```
