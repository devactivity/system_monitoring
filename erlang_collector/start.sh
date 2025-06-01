#!/bin/bash

# compile the code
rebar3 compile

# build
# erlc -o ebin apps/erlang_collector/src/*.erl
erlc -o ebin src/*.erl

exec erl -pa _build/default/lib/jsx/ebin -pa ebin/ -s collector start_link -noshell
