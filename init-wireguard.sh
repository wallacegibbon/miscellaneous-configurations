#! /bin/sh

set -e

ip link add dev wg0 type wireguard
ip link set up wg0

## Change the ip address.
ip addr add 172.16.9.6/24 dev wg0

## Change the file `wg.285k.private`.
wg set wg0 private-key /home/wallace/wg.285k.private peer $(cat /home/wallace/wg.xinrenfei.wang.public) endpoint xinrenfei.wang:32768 allowed-ips 0.0.0.0/0 persistent-keepalive 25
