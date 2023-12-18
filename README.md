[The Complete Haskell Course: From Zero to Expert!](https://www.udemy.com/course/the-complete-haskell-course-from-zero-to-expert)

[![](https://github.com/asarkar/udemy-complete-haskell/workflows/CI/badge.svg)](https://github.com/asarkar/udemy-complete-haskell/actions)

## Syllabus

Section 1: Course Introduction

Section 2: Basic Types

Section 3: Functions

Section 4: Solved Problems - Functions

Section 5: Tuples

Section 6: Lists

Section 7: Solved Problems - Lists

Section 8: Higher Order Functions

Section 9: Solved Problems - Higher Order Functions

Section 10: Solved Problems - Infinite Lists

Section 11: Binary Trees

Section 12: Solved Problems - Binary Trees

Section 13: Multiway Trees

Section 14: Solved Problems - Multiway Trees

Section 15: Graphs

Section 16: Solved Problems - Graphs

Section 17: Advanced Types

Section 18: Functors

Section 19: Applicatives

Section 20: Monads

Section 21: Input and Output

Section 22: Solved Problems - Input and Output

Section 23: Final Exams

## Running tests

```
./.github/run.sh
```

To run all matching tests:
```
./.github/run.sh -m <some_word>
```

To run exactly matching tests:
```
./.github/run.sh -m "/<some_word>/"
```

To run a _specific test_:
```
./.github/run.sh -m "/Ch11/evaluates expression/eval/"
```

To run a file containing a `main` method:
```
stack runhaskell app/Main.hs
```

To run an executable listed in `package.yaml`:
```
stack build
stack exec section16-exe
```

## License

Released under [Apache License v2.0](LICENSE).
