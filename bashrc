#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

export BROWSER=/usr/bin/firefox
export EDITOR=/usr/bin/nvim
export VISUAL=$EDITOR

### MINE
#PS1='\e[37;1m\u@\H \W \$\e[0m '

# VIM
if [ -x "$(command -v nvim)" ]; then
    alias vim="nvim"
    alias vi="nvim"
    alias vimdiff="nvim -d"
fi

# GO
export GOPATH=$HOME/go_packages

# HELM
export HELM_HOME=$HOME/helm

# RUST
export CARGO_PATH=$HOME/.cargo/bin

# DOTNET PATH
export DOTNET_CLI_TELEMETRY_OPTOUT=1

# COMPLETION
[[ $PS1 && -f /usr/share/bash-completion/bash_completion ]] && \
    . /usr/share/bash-completion/bash_completion

# PATH
export PATH="$PATH:$HOME/bin:$GOPATH/bin:$CARGO_PATH:$HOME/.local/bin:$HOME/.npm-global/bin:$HOME/.config/composer/vendor/bin"
export PATH=$DOTNET_PATH:$HOME/.dotnet:$HOME/Documents/docker-helmfile:$PATH

# HOW DO?
alias hd="howdoi"

# EXCERSISM
if [ -f ~/.config/exercism/exercism_completion.bash ]; then
    source ~/.config/exercism/exercism_completion.bash
fi

alias yt-audio="youtube-dl -x --audio-format vorbis --audio-quality 0"
alias phpqa='sudo docker run --init -it --rm -v $(pwd):/project -v $(pwd)/tmp-phpqa:/tmp -w /project jakzal/phpqa:alpine'

# GIT GOOD
alias ga='git add -i'
alias gc='git ci'
alias resolve='grep -lr "<<<<<<<" . | xargs git checkout'

# KUBE MADNESS
function resolve-pod() {
 echo $(kubectl get pods -o name | sed 's/^pod\///g' | grep "^$1" | head -1)
}

function kube() {
    if [[ "$#" < 2 ]]; then
        kube-fwd "$1"
    else
        POD=$(resolve-pod $2)
        if [ -z "$POD" ]; then
            POD=$(resolve-pod "heweb-$2")
        fi

        if [ -z "$POD" ]; then
            echo "No such pod: $2"
            return 1
        else
            kube-fwd "$1" "$POD" "${@:3}"
        fi
    fi
}

function kube-fwd() {
    COMMAND=$1
    shift

    DIRECT_FUNCTION="kube-$COMMAND"

    if type "$DIRECT_FUNCTION" | grep -q 'is a shell function'; then
        echo "$DIRECT_FUNCTION" "$@"
        $DIRECT_FUNCTION "$@"
    else
        echo kubectl "$COMMAND" "$@"
        kubectl "$COMMAND" "$@"
    fi
}

function kube-shell() {
    ${2?"You gotta specify a container too."}
    kubectl exec $1 -it -c $2 -- "bash"
}

kube-up() { minikube start; }
kube-down() { minikube stop; }
kube-status() { minikube status && kubectl get pods; }
kube-tail() { kubectl logs $1 --all-containers -f; }
kube-exec() { kubectl exec -it $1 "${@:2}"; }
kube-logs() { stern -o raw $1 -c php-fpm | egrep --line-buffered '^{' | jq .; }
kube-branch() { kubectl delete $(kubectl get pod -l release=heweb -o name); }
alias suz-update='helmfile delete --purge && helmfile apply --values local.yml'
kube-vars() { ~/Documents/k8s/tools/manage_ssm_vars.py list ci-cia; }
kube-migration() { kubectl exec $1 -c php-fpm -- /var/www/quadra/healthengine.com.au/lib/vendor/bin/phinx --configuration=/var/www/quadra/healthengine.com.au/admin/sql_scripts/phinx.yml create $2; }

# GIT PROMPT
GIT_PROMPT_ONLY_IN_REPO=1
GIT_PROMPT_FETCH_REMOTE_STATUS=0
source ~/.bash-git-prompt/gitprompt.sh
source <(kubectl completion bash)

# HASKELL
export cabal_helper_libexecdir=/home/saxonj/Documents/haskell-ide-engine/submodules/cabal-helper
export libexecdir=/home/saxonj/Documents/haskell-ide-engine/submodules/cabal-helper

# TASKBOOK
alias tb=taskbook
alias pods="kubectl get pods"
export SCRIPT_DIR=$HOME/.config/i3blocks

# HABITICA
pass habitica/cli 2>/dev/null 1>/dev/null
if [ $? -eq 0 ]; then
    source <(pass habitica/cli)
fi

# DOCKER
alias docker_clean_images='docker rmi $(docker images -a --filter=dangling=true -q)'
alias docker_clean_ps='docker rm $(docker ps --filter=status=exited --filter=status=created -q)'

# Noise Generation
alias silence='play  -n synth brownnoise vol'

# Secret manager
function pw() { bw list items --search $1 | jq; }
alias orphans='sudo pacman -Rns $(pacman -Qtdq)'

# Run something, muting output or redirecting it to the debug stream
# depending on the value of _ARC_DEBUG.
__python_argcomplete_run() {
    if [[ -z "$_ARC_DEBUG" ]]; then
        "$@" 8>&1 9>&2 1>/dev/null 2>&1
    else
        "$@" 8>&1 9>&2 1>&9 2>&1
    fi
}

_python_argcomplete() {
    local IFS=$'\013'
    local SUPPRESS_SPACE=0
    if compopt +o nospace 2> /dev/null; then
        SUPPRESS_SPACE=1
    fi
    COMPREPLY=( $(IFS="$IFS" \
                  COMP_LINE="$COMP_LINE" \
                  COMP_POINT="$COMP_POINT" \
                  COMP_TYPE="$COMP_TYPE" \
                  _ARGCOMPLETE_COMP_WORDBREAKS="$COMP_WORDBREAKS" \
                  _ARGCOMPLETE=1 \
                  _ARGCOMPLETE_SUPPRESS_SPACE=$SUPPRESS_SPACE \
                  __python_argcomplete_run "$1") )
    if [[ $? != 0 ]]; then
        unset COMPREPLY
    elif [[ $SUPPRESS_SPACE == 1 ]] && [[ "$COMPREPLY" =~ [=/:]$ ]]; then
        compopt -o nospace
    fi
}
complete -o nospace -o default -F _python_argcomplete utt

# TODO LIST
function pkglist() { rpm -qa --qf "%{NAME}\n" | sort > ~/packages; }

# JIRA
export JIRA_PROJECT="BI"

# GITHUB
eval "$(hub alias -s)"

# VOLUME
alias vol_down="amixer set Master 5.0%-"
alias vol_up="amixer set Master 5.0%+"

export GPG_TTY=$(tty)

alias pq='pacman -Ss'
export JAVA_HOME=/usr/lib/jvm/java-8-openjdk
alias todo=todo.sh
complete -F _todo todo
