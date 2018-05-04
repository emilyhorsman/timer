# timer

Time your tasks and get simple output in a CSV file!

TODO: Stats.

## Development

This is a [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/)
project.

```
$ stack setup
$ stack build                 # Build the project.
$ stack exec timer-exec       # Run the project.
$ stack install               # Install the project to `stack path --local-bin`.
```

## Usage

Start a task and then use `^C` (ctrl+c) to exit.

```
$ timer-exe taskCode!
taskCode!
0h00m22s^C
Exiting Main!
$ cat data.csv
startTime,endTime,code
2018-05-04T21:05:28,2018-05-04T21:05:50,taskCode!
```

Do anything you want with the data!
