#!/bin/sh

STATUS=$(nordlayer status 2>&1 | grep Connect | tr -d ' ' | cut -d ':' -f2)

if [ "$STATUS" = "Connected" ]; then
    echo "󰒘%{F#c6a0f6}%{A1:nordlayer d:}$(nordlayer status 2>&1 | grep Gateway | cut -d ':' -f2)%{A1}%{F-}"
elif [ "$STATUS" = "Connecting" ]; then
    echo "󱆢%{F#c6a0f6}%{A1:nordlayer d:}$(nordlayer status 2>&1 | grep Gateway | cut -d ':' -f2)%{A1}%{F-}"
else
    echo "󰦞 no vpn"
fi
