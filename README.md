
# [Goanna](https://en.wikipedia.org/wiki/Goanna) Cluster tracing library


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
1. escriptify
2. custom trace_port client
3. custom front end application to read ets tables, and format entries
4. be able to work with short/long names in the erlang distribution


#setup
edit the sys.config, here's example of the application config:
```Erlang
 {goanna,[
    {nodes, [
        % [{node,   NodeName},
        %  {cookie, Cookie},
        %  {type,   Type}]   %% tcpipo_port | file | erlang_distribution

        [{node,'node1@host'},
         {cookie, cookie},
         {type, tcpip_port}
        ],

        [{node,'node2@host'},
         {cookie, cookie},
         {type, tcpip_port}
        ]
    ]}
 ]}
```

## usage
To run this library stand-alone, just edit the sys.config, and ./start-dev.sh.
My later intetion is to use this library application, as part of another bigger piece of work.
