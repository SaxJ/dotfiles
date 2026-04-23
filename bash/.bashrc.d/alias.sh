alias vim=nvim
alias lg=lazygit
alias v=nvim
# alias hx=helix

alias ytd="yt-dlp -x --audio-format best --audio-quality 0 --embed-metadata --embed-thumbnail -o \"~/Music/%(title)s.%(ext)s\" --restrict-filenames"

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

function prc() {
	title="$(git branch --show-current): "
	gh pr create -d -t "$title" -e
}

function serve() {
  npx http-server "$1" -o -p 8080
}

git_relative_path() {
  local file_path="$1"
  local git_root
  git_root=$(git -C "$(dirname "$file_path")" rev-parse --show-toplevel 2>/dev/null) || {
    echo "Error: Not inside a git repository" >&2
    return 1
  }

  realpath --relative-to="$git_root" "$(realpath "$file_path")"
}

function ru() {
  local root=$(git rev-parse --show-toplevel)
  local project=$(basename "$root")
  local file=$(git_relative_path "$1")

  scp "$root/$file" "minikube:/home/ubuntu/$project/$file"
}

function rd() {
  local root=$(git rev-parse --show-toplevel)
  local project=$(basename "$root")
  local file=$(git_relative_path "$1")

  scp "minikube:/home/ubuntu/$project/$file" "$root/$file"
}
