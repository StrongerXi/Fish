# Fish

This repo contains an implementation of the Fish game in OCaml for [cs4500 Software
Development]( https://www.ccs.neu.edu/home/matthias/4500-f20/index.html) taught by [Matthias Felleisen](https://felleisen.org/matthias/) at
NEU in 2020 fall semester.

## To users
- To build the project:

```
dune build
```

- To run the server:

```
./xintegration xserver <port>
```

- To launch AI clients: (`<server-ipaddr>` defaults to localhost)

```
./xintegration xclient <num-of-clients> <server-port> <server-ipaddr>
```

Please refer to [specs for each milestone](https://www.ccs.neu.edu/home/matthias/4500-f20/assignments.html) for details on the testing harness.

## To developers

- To run unit tests

```
dune runtest
```

- To run integration testing harness:

```
./xintegration <test-name> ...
```

- How to program a client in another language (the JSON communication protocol):

__Note__ this is copied from [Matthias' specs](https://www.ccs.neu.edu/home/matthias/4500-f20/remote.html)

Connecting to the Server

    server                           client (c_1) ... client (c_n)
      |                                |                 |
      |< ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ |                 | tcp connect
      |                                |                 |
      |   name as JSON string          |                 |
      |< ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ |                 | no reply
      |                                |                 |
      |                                |                 |
      | new() rpp_1                    |                 |
      |------->+                       |                 |
      |        |                       |                 |
      |        |                       |                 |
      .        |                       .                 .
      .        |                       .                 .
      .        |                       .                 .
      |< ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ | tcp connect
      |        |                       |                 |
      |   name as JSON string          |                 |
      |< ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ | no reply
      |        |                       |                 |
      |        |                       |                 |
      | new()              rpp_n       |                 |
      |-------------------->+          |                 |
      |        |            |          |                 |
      |        |            |          |                 |
      |        |            |          |                 |
      |
      | new(rpp_1,..., rpp_n)             manager
      |-------------------------------------+
      |                                     |
      |                                     |
      |                                     |

__Note__ The name consist of at least one and at most 12 alphabetical ASCII characters. 
There is no guarantee that distinct clients sign up with distinct names. It is expected within 
10s of opening a connection. The server accepts TCP connections and represents each as a remote 
player once the client has submitted a name. Once a sufficient number of players have connected 
to the server and the waiting period is over, the server signs up these players for a tournament
with the manager and asks it to run a complete tournament.In real-world settings, the server may
put players that arrive after the waiting period into a "waiting room."

Starting a Tournament

    manager <-------------- ~~~~ --  player (p_1) . . . player (p_n)
      |                                |                 | % for n <= MAX_PLAYERS
      |                                |                 |
      |                                |                 |
      |                                |                 |  
      |     start(Boolean)             |                 | % true means the tournament
      | ------------------- ~~~~ ----> |                 | % is about to start
      .                                .                 .
      .                                .                 .
      .                                .                 .
      |     start(Boolean)             |                 |
      | ------------------- ~~~~ ----------------------> |
      |                                |                 |

The protocol for running a tournament remains the same.

Terminating a Tournament

    manager                        player (p_1) . . . player (p_n)
      |                                |                 |
      |                                |                 |
      |     end(Boolean)               |                 |
      | ----------------- ~~~~ ------> |                 | % true means "winner"
      |                                |                 | % false means "loser"
      .                                .                 .
      .                                .                 .
      .                                .                 .
      .                                .                 .
      |     end(Boolean)               |                 |
      | ----------------- ~~~~ ------------------------> |
      |                                |                 |
      |                                |                 |

Starting a Game

    referee                         player (p_1) . . . player (p_n)
      |                                |                 |
      |     play_as(color)             |                 |
      | ---------------------- ~~~~ -> |                 |
      |                                |                 |
      .                                .                 .
      .                                .                 . % p_i's assigned
      .                                .                 . % color for a game
      |     play_as(color)             |                 |
      | ---------------------- ~~~~ -------------------> |
      |                                |                 |
      |     play_with(color[])         |                 |
      | ---------------------- ~~~~ -> |                 |
      |                                |                 |
      .                                .                 .
      .                                .                 . % the colors of
      .                                .                 . % other players
      .                                .                 .
      |     play_with(color[])         |                 |
      | ---------------------- ~~~~ -------------------> |
      |                                |                 |
      |                                |                 |
      |     setup(state)               |                 |
      | ---------------------- ~~~~ -> |                 |
      |     place                      |                 |
      | <===================== ~~~~ =  |                 |
      .                                .                 .
      .                                .                 . % choose a place
      .                                .                 . % for one penguin
      .                                .                 .
      |                                |                 |
      |     setup(state)               |                 |
      | ---------------------- ~~~~ -------------------> |
      |     place                      |                 |
      | <===================== ~~~~ ==================== |
      .                                .                 |  
      .                                .                 . % repeat for as
      .                                .                 . % many penguins
      .                                .                 . % as allocated   
      |     setup(state)               |                 |
      | ---------------------- ~~~~ -> |                 |
      |     place                      |                 |
      | <===================== ~~~~ =  |                 |
      .                                .                 .
      .                                .                 . % choose a place
      .                                .                 . % for one penguin
      .                                .                 .
      |                                |                 |
      |     setup(state)               |                 |
      | ---------------------- ~~~~ -------------------> |
      |     place                      |                 |
      | <===================== ~~~~ ==================== |

Playing Turns

    referee                         player (p_1) . . . player (p_n)
      |                                |                 |
      |     tt(state,actions[])        |                 | % player receives:
      | ----------------------- ~~~~ > |                 | % - current state
      |     action                     |                 | % - actions since last call
      | <====================== ~~~~   |                 | % returns a move
      .                                .                 .
      .                                .                 . % one turn per player
      .                                .                 . % skip if it cannot
      .                                .                 . % move in `this` state
      .                                .                 .
      |     tt(state,action[])          |                 |
      | ----------------------- ~~~~ ------------------> |
      |     action                     |                 |
      | <====================== ~~~~ =================== |
      |                                |                 |
      .                                .                 .
      .                                .                 . % repeat until
      .                                .                 . % no penguin is
      .                                .                 . % able to move
      .                                .                 .
      |                                |                 |
      |     tt(state,actions[])        |                 |
      | ----------------------- ~~~~ > |                 |
      |     action                     |                 |
      | <====================== ~~~~   |                 |
      .                                .                 .
      .                                .                 .
      .                                .                 .
      .                                .                 .
      |     tt(state,action[])         |                 |
      | ----------------------- ~~~~ ------------------> |
      |     action                     |                 |
      | <====================== ~~~~ =================== |
      |                                |                 |
      .                                .                 .
      .                                .                 .

Here is the general format of a function call:

```
    [ String, [Argument, ...] ]
```

Defined remote calls:

    Name                       Argument, ...                      Result                Note
    start                      Boolean                            "void" 
    playing-as                 Color                              "void"
    playing-with               [Color, ... , Color]               "void"                 &
    setup                      State                              Position               ^
    take-turn                  State, [Action, ... , Action]      Action                 *, ^
    end                        Boolean                            "void"
    
    & The array of Colors informs a player how many opponents it faces and the colors that 
      observers use to show them on the game board.

    * The array of Actions represents the penguin moves since the last time the take-turn method was
      called. It is empty if this is the first call or a player was eliminated since the last call. 
      A method may use the state to compute a response functionally xor its own private version of 
      a game tree plus the array of actions to figure out a response imperatively. The latter allows 
      caching of tree walks to some extent.

    ^ As State is used for placing penguins and moving them, it merely lives up the state specification.
      That is, it does not satisfy the post-placement-phase constraints used in past test fests 
      (a sufficient number of penguins for the given players).

Data definitions (in json):

    Color ::= "red" | "white" | "brown" | "black"

    State ::=
    {
      "players" : Player*,
      "board" : Board
    }
    
    Player* ::= [Player, Player, ..., Player]
    
    Player ::=
    {
      "color" : Color,
      "score" : Natural,
      "places" : [Position, Position, ... , Position]
    }
    
    Board ::= [[Tile, ...], ...]
    
    Tile ::= Natural in [0, 5]
    
    Position ::= [Natural, Natural]
    
    Action ::= [Position, Position]
