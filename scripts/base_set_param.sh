#!/bin/sh

param_name="$1"
param_val="$2"

if [ "$3" ]
then
    config_path="$3"
else
    config_path="./config/server_config.txt"
fi

sed -i "s|\(${param_name}\) \(.*\)|\1 ${param_val}|" "${config_path}"
