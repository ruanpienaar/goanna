
h1. [Goanna](https://en.wikipedia.org/wiki/Goanna)
Cluster tracing library


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

h2.
This WIP is for applying traces to a cluster of nodes. It currently stores the traces in a ETS table.
It does deal with remote node outages, and re-applies the traces.

h2. setup
edit the sys.config, here's example of the application config:
```
{goanna,[
    {nodes, [
        % {SomeNode, SomeCookie}
        {'goanna1@hostname', goanna},
        {'goanna1@hostname', goanna}
    ]}
 ]},
```

h2. usage
To run this library stand-alone, just edit the sys.config, and ./start-dev.sh.
My later intetion is to use this library application, as part of another bigger piece of work.
