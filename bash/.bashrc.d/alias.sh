alias vim=nvim
alias lg=lazygit

alias ytd="yt-dlp -t aac -o \"~/Music/%(title)s.%(ext)s\" --restrict-filenames"

alias gs="git status"
alias gd="git diff"
alias gc="git commit"

alias vpnc="openvpn3 session-start --config ~/office.ovpn"
alias vpnd="openvpn3 session-manage --disconnect --config ~/office.ovpn"

function cd() {
  z "$1"
  zellij action rename-tab "$(basename "$(pwd)")"
}
