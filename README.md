# Reversi Server & Client

## How to Build
```
$ make
```

It generates two execution files: `reversi` and `reversi-serv` that are the client and the server, respectively.


## How to Use 

run server：
```
$ reversi-serv -p 30000 -t 500
```

Use `-p` option for port specification.

After start running server, the following message appears.

```
Waiting 2 connections ...
```

After this, start running the client：

```
$ reversi -H "localhost" -p 30000 -n Player1
```

Then the following message appears.

```
Waiting 1 connections ...
```
So start running another client:

```
$ reversi -H "localhost" -p 30000 -n Player2
```
As doing above, the game starts.
NOTE: Player name must be different.

`--help` option will tell you the detail of each option.

Refer to `reversi-protocol.pdf` for the detailed protocol.
