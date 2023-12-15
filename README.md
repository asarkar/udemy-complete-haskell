[The Complete Haskell Course: From Zero to Expert!](https://www.udemy.com/course/the-complete-haskell-course-from-zero-to-expert)

[![](https://github.com/asarkar/udemy-complete-haskell/workflows/CI/badge.svg)](https://github.com/asarkar/udemy-complete-haskell/actions)

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
