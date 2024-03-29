#!/usr/bin/awk -f

BEGIN {
	dim = "\033[2m"
	cyan = "\033[36m"
	reset = "\033[0m"
}
{
	# Strip carriage returns from line
	gsub(/\r/, "", $0)

	if ($0 ~ /^On .*, .* wrote:/ || $0 ~ /^>+/) {
		print dim cyan $0 reset
	} else {
		print $0
	}
}