#! /usr/bin/bash

vpn() {
    if openvpn3 sessions-list | grep -q 'office.ovpn'; then
        echo "VPN: Connected"
    else
        echo "VPN: Disconnected"
    fi
}

network() {
    eth="$(nmcli -t -c no c show --active | grep eth | head -n1)"
    wifi="$(nmcli -t -c no c show --active | grep wifi | head -n1)"
    if [ ! -z "$eth" ]; then
        echo "Network: Eth"
    fi

    if [ ! -z "$wifi" ]; then
        echo "Network: Wifi"
    fi

    if [ -z "$eth$wifi" ]; then
        echo "Network: Offline"
    fi
}

volume() {
    audio_status=$(pactl get-sink-volume @DEFAULT_SINK@)
    parts=($audio_status)
    echo "Vol: ${parts[4]}"
}

SLEEP_SEC=2
while :; do
    echo "$(date) | $(network) | $(vpn) | $(volume)"

    sleep $SLEEP_SEC
done
