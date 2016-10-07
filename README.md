
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

###### This WIP is for applying traces to a cluster of nodes. It currently stores the traces in a ETS table. It does deal with remote node outages, and re-applies the traces.
###### the curent implementation handles noraml erlang RPC distribution, and trace_port.

#Roadmap
1. escriptify ( Only need to use getopt, for cmdline options, editing sys.config and starting it works )
2. custom trace_port client
4. be able to work with short/long names in the erlang distribution
5. Erlang version compatibility, currently only OTP 18+ ( time calls )
6. file trace needs to be implemented

# Setup
edit the sys.config, here's example of the application config:
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

## Usage
To run this library stand-alone, just 
Edit the sys.config, add nodes to automatically connect to, traces to be
immediately applied, and trace options for all future traces.
```bash
$ ./start-dev.sh
```