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
    eth="$(nmcli -t -c no c show --active | grep vpn | head -n1)"
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

SLEEP_SEC=5
while :; do
    echo "$(network) | $(vpn)"

    sleep $SLEEP_SEC
done