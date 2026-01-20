#! /bin/sh

set -e

if test "$#" -ne 1 || test -z "$1"; then
	echo "Usage example: $0 wg0" >&2
	exit 1
fi

IFACE_NAME=$1

ip link add dev $IFACE_NAME type wireguard
ip link set up $IFACE_NAME

## Change the ip address.
ip addr add 172.16.9.6/24 dev $IFACE_NAME

## Change the file `wg.XXX.private`.
wg set $IFACE_NAME private-key /home/wallace/wg.XXX.private peer $(cat /home/wallace/wg.xinrenfei.wang.public) endpoint xinrenfei.wang:32768 allowed-ips 0.0.0.0/0 persistent-keepalive 25
