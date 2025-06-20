alias vim=nvim
alias lg=lazygit
alias v=nvim

alias ji="jira issue list --jql=\"assignee = currentUser() AND project = MKT AND statusCategory != Done\""
alias ytd="yt-dlp -t aac -o \"~/Music/%(title)s.%(ext)s\" --restrict-filenames --embed-thumbnail --embed-metadata"

alias gs="git status"
alias gd="git diff"
alias gc="git commit"

alias ci=clockin
alias co=clockout

alias vpnc="openvpn3 session-start --config ~/office.ovpn"
alias vpnd="openvpn3 session-manage --disconnect --config ~/office.ovpn"

function cd() {
  z "$1"
  if [[ -n "$ZELLIJ_SESSION_NAME" ]]; then
    zellij action rename-tab "$(basename "$(pwd)")"
  fi
}
