alias vim=nvim
alias lg=lazygit
alias v=nvim
alias hx=helix

alias ytd="yt-dlp -t aac -o \"~/Music/%(title)s.%(ext)s\" --restrict-filenames --embed-thumbnail --embed-metadata"

alias gs="git status"
alias gd="git diff"
alias gc="git commit"
alias gci="git commit --interactive"

alias ci="timelog clockin"
alias co="timelog clockout"

alias vpnc="openvpn3 session-start --config ~/office.ovpn"
alias vpnd="openvpn3 session-manage --disconnect --config ~/office.ovpn"

function cd() {
  z "$1"
  if [[ -n "$ZELLIJ_SESSION_NAME" ]]; then
    zellij action rename-tab "$(basename "$(pwd)")"
  fi
}

# Kakoune
function k() {
  kak -s $(basename "$(pwd)")
}
