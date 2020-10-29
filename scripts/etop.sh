#! /bin/bash

erl -name etop-`date +%s` -hidden -s etop -s erlang halt   -output text -setcookie qwer   -tracing off -sort msg_q -interval 1