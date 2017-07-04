
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
To get up and running clone goanna
```
$ git clone https://github.com/ruanpienaar/goanna
```
and compile it
```
$ make
```

Goanna is a small library built on top of dbg, primarily for convenience.

Escript example:
```
$ make
$ ./_build/default/bin/goanna -s   ( shortnames )
$ ./_build/default/bin/goanna -l   ( you guessed it, long names )
```

goanna allows you to easily specify nodes either in the command line, or in the sys.config.
Goanna uses [hawk](https://github.com/ruanpienaar/hawk) for managing remote node connectivity.
Hawk will try and reconnect nodes that have disconnected and re-apply previous traces.
This is useful in testing/debugging scenarios where the nodes are ephemeral or short lived due to an issue.
Hawk will Only work with OTP Erlang versions with map support, and
therefore, the same for goanna.

Beam shell example:
```Erlang
1> goanna_api:add_node('somenode@some.host', oreo, tcpip_port).
```
[System config](https://github.com/ruanpienaar/goanna/sys.config) example:
```erlang
 {goanna,[
    {nodes,[
        [{node,   NodeName}, %% 'somenode@myhost.com'
         {cookie, Cookie},   %% oreo
         {type,   Type}]     %% tcpip_port | file | erlang_distribution
        ]
    ]}
 ]}
```

So once you've added some nodes, you can enable a trace pattern with Module, Module Function, Module Function Arity or write your own trace match spec with help of redbug's string parsing to dbg trace match spec code.

Tracing Example:
```Erlang
1> goanna_api:trace(module).

2> goanna_api:trace(module, function).

3> goanna_api:trace(module, function, arity_integer).

4> goanna_api:trace_ms("ets:lookup(Cust, customers) when Cust==homer -> return").

```

## Goanna Forward Callback Mod
#### -behaviour(goanna_forward_callback_mod). )

You can write your own trace entry handler callback module. Just follow the goanna_forward_callback_mod
behaviour convention, and do whatever you like with trace entries.
There are examples in src/, goanna_forward_shell,
goanna_forward_os_io, goanna_forward_file. Choose your module, and update
the sys.config in data_retrival_method, with {push, X Miliseconds, ForwardModule}.

## Sys.config options
Goanna features a host of different config options to be set as defaults or can be adjust at runtime.
One of config settings deals with limiting traces by either a timed limit or trace message count limit or whichever comes first.

Application env System configuration options:

1. data_retrival_method ( push or pull )
 {push, WaitTime :: non_neg_integer(), GoannaForwardCallbackModule :: atom()}
 pull
2. push_data_batch_size ( How much entries to push - applies to data_retrival_method=push )
 non_neg_integer()
3. nodes ( All your nodes )
 [{node, Node :: atom()},
  {cookie, Cookie :: atom()},
  {type, Type :: tcpip_port | file | erlang_distribution}]
4. traces ( All the active trace patterns to apply at startup
 [{module,mod :: atom()}],
 [{module,mod :: atom()}, {function,func :: atom()}],
 [{module,mod :: atom()}, {function,func :: atom()}, {arity, Arity :: non_neg_integer()}],
5. default_trace_options ( Options regarding running traces, like time and trace message total count )
 {time, TimeMs :: non_neg_integer()}
 {messages, Messages :: non_neg_integer()}
6. dbg_p_flags ( [dbg:p/2's](http://erlang.org/doc/man/dbg.html#p-2) Flag options **still experimental** )
 call

#WIP Roadmap
1. custom trace_port client
2. file trace needs to be implemented ( trace to binary file format, faster )
3. able to have multiple forward callback modules.