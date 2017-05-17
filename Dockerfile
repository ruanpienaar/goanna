FROM erlang:19.2
RUN git clone https://github.com/ruanpienaar/goanna && cd goanna && make 
CMD epmd -daemon && epmd -names && erl -name testnode@127.0.0.1 -setcookie test -detached -noinput -noshell && cd goanna && ./start-dev.sh
EXPOSE 11000-12000