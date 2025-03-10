#!/usr/bin/env bash

finished_from=$(date --date='2 minutes ago' --utc +%Y-%m-%dT%H:%MZ)
api_key=$BUILDKITE_API_KEY
org=$BUILDKITE_ORG
user_id=$(curl -H "Authorization: Bearer $api_key" https://api.buildkite.com/v2/user | jq -rc '.id')

recent_builds_url=$(printf "https://api.buildkite.com/v2/organizations/%s/builds?finished_from=%s&branch[]=main&branch[]=master&creator=%s" "$org" "$finished_from" "$user_id")
messages=$(curl -H "Authorization: Bearer $api_key" "$recent_builds_url" | jq -rc '.[].message')

echo "$messages" | while read -r line
do
    notify-send "Buildkite done" "$line"
done
