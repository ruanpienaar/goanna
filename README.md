
# [Goanna](https://en.wikipedia.org/wiki/Goanna) Cluster tracing library
[![Build Status](https://travis-ci.org/ruanpienaar/goanna.svg?branch=master)](https://travis-ci.org/ruanpienaar/goanna)
[![Coverage Status](https://coveralls.io/repos/github/ruanpienaar/goanna/badge.svg?branch=master)](https://coveralls.io/github/ruanpienaar/goanna?branch=master)

```
    _.-~~-.__
 _-~ _-=-_   ''-,,
('___ ~~~   0     ~''-_,,,,,,,,,,,,,,,,
 \~~~~~~--'                            '''''''--,,,,
  ~`-,_      ()                                     '''',,,
       '-,_      \                           /             '', _~/|
  ,.       \||/~--\ \_________              / /______...---.  ;  /
  \ ~~~~~~~~~~~~~  \ )~~------~`~~~~~~~~~~~( /----         /,'/ /
   |   -           / /                      \ \           /;/  /
  / -             / /                        / \         /;/  / -.
 /         __.---/  \__                     /, /|       |:|    \  \
/_.~`-----~      \.  \ ~~~~~~~~~~~~~---~`---\\\\ \---__ \:\    /  /
                  `\\\`                     ' \\' '    --\'\, /  /
                                               '\,        ~-_'''"
```
## Getting started

#### Docker container
```
docker pull ruanpienaar/goanna
```

#### From Source
Get It and compile it
```
$ git clone https://github.com/ruanpienaar/goanna && cd goanna && make
```

### !Important
ets:take is not present in erlang 17.
remove the macro 'ETS_TAKE' from rebar.config if you're on 17, and
recompile.

### Description
Goanna is a small library built on top of dbg, primarily for convenience.

goanna allows you to easily specify nodes either in the command line, or in the sys.config.
Goanna uses [hawk](https://github.com/ruanpienaar/hawk) for managing remote node connectivity.
Hawk will try and reconnect nodes that have disconnected and re-apply previous traces.
This is useful in testing/debugging scenarios where the nodes are ephemeral or short lived due to an issue.
Hawk will Only work with OTP Erlang versions with map support, and
therefore, the same for goanna.

Beam shell example:
```Erlang
1> goanna_api:add_node('somenode@some.host', oreo).
```
[System config](https://github.com/ruanpienaar/goanna/sys.config) example:
```erlang
 {goanna,[
    {nodes,[
        {NodeName, Cookig} %% 'somenode@myhost.com', oreo
    ]}
 ]}
```

So once you've added some nodes, you can enable a trace pattern with {Module}, {Module, Function}, {Module, Function, Arity} or write your own trace match spec as a String "ets:lookup(Cust, customers) when Cust==homer -> return", with help of redbug's string parsing to dbg trace match spec code.

Tracing Example:
```Erlang
1> goanna_api:trace(module).

2> goanna_api:trace(module, function).

3> goanna_api:trace(module, function, arity_integer).

4> goanna_api:trace_ms("ets:lookup(Cust, customers) when Cust==homer -> return").

```

### Goanna Forward Callback Mod
#### -behaviour(goanna_forward_callback_mod).

You can write your own trace entry handler callback module. Just follow the goanna_forward_callback_mod
behaviour convention, and do whatever you like with trace entries.
There are examples in src/, goanna_forward_shell and goanna_forward_file. 
Choose your forward module and have data_retrival_method set to push as explained in the sys.config below.

### Sys.config options
Goanna features a host of different config options to be set as defaults or can be adjust at runtime.
One of config settings deals with limiting traces by either a timed limit or trace message count limit or whichever comes first.

Application env System configuration options:

1. data_retrival_method ( push or pull )
```Erlang
 {push, WaitTime :: non_neg_integer(), GoannaForwardCallbackModule :: atom(), BatchAmount :: atom()}
 |
 pull
 ```
2. nodes ( Nodes to connect to on startup )
```Erlang
[{Node :: atom(), Cookie :: atom()}]
```
3. traces ( Traces to be applied on startup )
```Erlang
  [{Module :: atom()}],
  [{Module :: atom(), Function :: atom()}],
  [{Module :: atom(), Function :: atom(), Arity :: non_neg_integer()}]
```
4. default_trace_options ( Options regarding running traces, like time and trace message total count )
```Erlang
 {time, TimeMs :: non_neg_integer()}
 {messages, Messages :: non_neg_integer()}
 ```

#### WIP Roadmap
1. custom trace_port client
2. file trace needs to be implemented ( trace to binary file format, faster )
3. able to have multiple forward callback modules.
