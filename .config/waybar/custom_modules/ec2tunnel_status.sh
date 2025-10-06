#!/bin/bash

PID_FILE="/tmp/ec2_tunnel.pid"
# if [ -f $PID_FILE ]; then
if  nc -z -w 3 $(cat /tmp/public_dns) 22 ; then
    echo '{"text":"connected","class":"connected","tooltip":"EC2 SSH Tunnel: Active"}'
else
    echo '{"text":"disconnected","class":"disconnected","tooltip":"EC2 SSH Tunnel: Inactive"}'
fi
