#! /usr/bin/bash

vpn() {
    nmcli_output="$(nmcli -t -c no c show --active | grep vpn | head -n1)"
    if [ -z "$nmcli_output" ]
    then
        echo "VPN: Disconnected"
    else
        vpn_name="$(echo "$nmcli_output" | sed -e 's/:.*$//')"
        echo "VPN: $vpn_name"
    fi
}

network() {
    eth="$(nmcli -t -c no c show --active | grep eth | head -n1)"
    wifi="$(nmcli -t -c no c show --active | grep wifi | head -n1)"
    if [ ! -z "$eth" ]
    then
        echo "Network: Eth"
    fi

    if [ ! -z "$wifi" ]
    then
        echo "Network: Wifi"
    fi

    if [ -z "$eth$wifi" ]
    then
        echo "Network: Offline"
    fi
}

volume() {
    audio_status=$(pactl get-sink-volume @DEFAULT_SINK@)
    parts=( $audio_status )
    echo "Vol: ${parts[4]}"
}

SLEEP_SEC=2
while :; do
    echo "$(date) | $(network) | $(vpn) | $(volume)"

    sleep $SLEEP_SEC
done
