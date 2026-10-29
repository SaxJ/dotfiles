alias vim=nvim
alias lg=lazygit
alias v=nvim
# alias hx=helix

alias ytd="yt-dlp -x --audio-format best --audio-quality 0 --embed-metadata --embed-thumbnail -o \"~/Music/%(title)s.%(ext)s\" --restrict-filenames"

alias ls="ls --color"

alias gp="git push"
alias ga="git add -i"
alias gs="git status"
alias gd="git diff"
alias gc="git commit"
alias gci="git commit --interactive"
alias gl="git log --graph"
alias gmt="git mergetool"
alias gmm="git merge master"
alias gf="git fuzzy"

alias ci="timelog clockin"
alias co="timelog clockout"

alias vpnc="openvpn3 session-start --config ~/office.ovpn"
alias vpnd="openvpn3 session-manage --disconnect --config ~/office.ovpn"

title() {
  echo -ne "\033]0;$1\007"
}

function cd() {
  z "$1"
  if [[ -n "$ZELLIJ_SESSION_NAME" ]]; then
    zellij action rename-tab "$(basename "$(pwd)")"
  fi
}

# Kakoune
function k() {
  kak -C $ZELLIJ_SESSION_NAME
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

function jt() {
  local ticket=$(board_tickets)
  git co -b "$ticket"
}

# Interactive worktree switcher using fzf
wti() {
  local out key selected path
  out=$(git worktree list | fzf --height 40% --reverse \
    --header 'enter: cd | ctrl-o: open | ctrl-d: delete' \
    --expect=ctrl-o,ctrl-d) || return
  key=$(head -1 <<<"$out")
  selected=$(sed -n 2p <<<"$out")
  [ -n "$selected" ] || return
  path=$(awk '{print $1}' <<<"$selected")

  case "$key" in
  ctrl-o)
    [ "$HERDR_ENV" = 1 ] && herdr workspace create --cwd "$path" --label "$(basename "$path")" --focus
    ;;
  ctrl-d)
    if [ "$path" = "$(git worktree list | head -1 | awk '{print $1}')" ]; then
      echo "Refusing to delete main worktree" >&2
      return 1
    fi
    read -rp "Delete worktree $path? [y/N] " ans
    [[ "$ans" =~ ^[Yy]$ ]] || return
    if ! git worktree remove "$path"; then
      read -rp "Force delete? [y/N] " ans
      [[ "$ans" =~ ^[Yy]$ ]] && git worktree remove --force "$path"
    fi
    ;;
  *)
    cd "$path" || return
    ;;
  esac
}
