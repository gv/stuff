#!/bin/sh
tar c *|tar xv -C /
systemctl enable systemd-resolved
systemctl enable systemd-networkd
ln -sf /run/systemd/resolve/resolv.conf /etc/resolv.conf
