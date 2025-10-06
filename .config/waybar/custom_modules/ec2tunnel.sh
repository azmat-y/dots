PID_FILE="/tmp/ec2_tunnel.pid"
if [ -f $PID_FILE ]; then
    tunnel off
else
    tunnel on &
fi
