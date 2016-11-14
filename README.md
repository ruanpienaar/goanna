
# [Goanna](https://en.wikipedia.org/wiki/Goanna) Cluster tracing library
[![Build Status](https://travis-ci.org/ruanpienaar/goanna.svg?branch=master)](https://travis-ci.org/ruanpienaar/goanna)

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
there are two modes of running goanna, as a erlang node, with [start-dev.sh](https://github.com/ruanpienaar/goanna/blob/master/start-dev.sh) or the goanna escript.

Escript example:
```
$ make
$ ./goanna -s   ( shortnames )
$ ./goanna -l   ( you guessed it, long names )
```

goanna allows you to easily specify nodes either in the command line, or in the sys.config.
Goanna uses [hawk](https://github.com/ruanpienaar/hawk) for managing remote node connectivity.
Hawk will try and reconnect nodes that have disconnected, and reapply the previous traces.
This is useful in testing/debugging scenarios where the nodes are ephemeral or short lived due to an issue.

Beam shell example:
```Erlang
1> goanna_api:add_node('somenode@some.host', oreo, tcpip_port).
```
[System config](https://github.com/ruanpienaar/goanna/sys.config) example:
```erlang
 {goanna,[
    {nodes,[
        [{node,   NodeName},
         {cookie, Cookie},
         {type,   Type}]   %% tcpipo_port | file | erlang_distribution
        ]
    ]}
 ]}
```

So once you've added some nodes, you can enable a trace pattern with Module, Module Function or Module Function Arity.
**Module Function Matchspec coming soon. Where you'd be able to trace functionality and have traces returned based on the mactch spec**

Tracing Example:
```Erlang
1> goanna_api:trace(module).

2> goanna_api:trace(module, function).

3> goanna_api:trace(module, function, arity_integer).

```

Goanna features a host of different config options to be set as defaults, or can be adjust at runtime.
One of which is limiting traces by either a timed limit, or trace message count limit, or whichever comes first.

Application env System configuration options:

1. data_retrival_method ( push or pull )  
 {push, WaitTime :: non_neg_integer(), Module :: atom()}
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
5. default_trace_options ( Options regarding running traces, like time, and trace message total count )  
 {time, TimeMs :: non_neg_integer()}
 {messages, Messages :: non_neg_integer()}
6. dbg_p_flags ( [dbg:p/2's](http://erlang.org/doc/man/dbg.html#p-2) Flag options **still experimental** )  
 call

#WIP Roadmap
1. escriptify getopt integration needed
2. custom trace_port client
4. be able to work with short/long names in the erlang distribution
5. Able to compile gonna on OTP 18 < ( timestamp code )
6. file trace needs to be implemented
