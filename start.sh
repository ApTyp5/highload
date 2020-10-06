#!/bin/sh

rebar3 release
sudo ./_build/default/rel/highload/bin/highload console
