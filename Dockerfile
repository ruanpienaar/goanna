# FROM erlang:19.2
FROM alpine:latest
RUN apk --update add make git wget erlang erlang-ssl erlang-public-key erlang-asn1 erlang-crypto erlang-debugger erlang-dev erlang-dialyzer erlang-edoc erlang-erl-docgen erlang-erl-interface erlang-erts erlang-et erlang-eunit erlang-hipe erlang-ic erlang-inets erlang-runtime-tools erlang-sasl erlang-syntax-tools erlang-tools erlang-wx && rm -rf /var/cache/apk/*
RUN cd ~/ && \
    git clone https://github.com/ruanpienaar/goanna && \
    cd goanna && \
    make && \
    cp Docker_sys_config/sys.config .
CMD epmd -daemon && \
    sleep 1 && \
    epmd -names && \
    ~/goanna/start-dev.sh && \
    sleep 1 && \
    to_erl ~/goanna/pipe/
EXPOSE 11000-12000


# erlang-asn1
# erlang-common-test
# erlang-cosevent
# erlang-coseventdomain
# erlang-cosfiletransfer
# erlang-cosnotification
# erlang-cosproperty
# erlang-costime
# erlang-costransaction
# erlang-crypto
# erlang-debugger
# erlang-dev
# erlang-dialyzer
# erlang-diameter
# erlang-edoc
# erlang-eldap
# erlang-erl-docgen
# erlang-erl-interface
# erlang-erts
# erlang-et
# erlang-eunit
# erlang-hipe
# erlang-ic
# erlang-inets
# erlang-jinterface
# erlang-megaco
# erlang-mnesia
# erlang-observer
# erlang-odbc
# erlang-orber
# erlang-os-mon
# erlang-otp-mibs
# erlang-parsetools
# erlang-public-key
# erlang-reltool
# erlang-runtime-tools
# erlang-sasl
# erlang-snmp
# erlang-ssh
# erlang-ssl
# erlang-syntax-tools
# erlang-tools
# erlang-wx
# erlang-xmerl