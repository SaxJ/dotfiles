#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

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

# YARN
export YARN_INSTALL_PATH=$HOME/.yarn/bin

# NPM
export NPM_INSTALL=/home/saxonj/npm-global

# DOTNET PATH
export DOTNET_CLI_TELEMETRY_OPTOUT=1

# PYTHON PACKAGES
export PIP_INSTALL=$HOME/global-pip/bin

# COMPOSER PACKAGES
export COMPOSER_BINS=$HOME/.config/composer/vendor/bin

export FLUTTER_BIN=$HOME/flutter/bin

# COMPLETION
[[ $PS1 && -f /usr/share/bash-completion/bash_completion ]] && \
    . /usr/share/bash-completion/bash_completion

# PATH
export PATH="$PATH:/usr/local/bin:$HOME/bin:$GOPATH/bin:$CARGO_PATH:$HOME/.local/bin:$HOME/npm-global/bin:$PIP_INSTALL"
export PATH=$FLUTTER_BIN:$DOTNET_PATH:$HOME/.dotnet:$HOME/Documents/docker-helmfile:$PATH

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

# GITHUB
eval "$(hub alias -s)"

# VOLUME
alias vol_down="amixer set Master 5.0%-"
alias vol_up="amixer set Master 5.0%+"

export GPG_TTY=$(tty)

export JAVA_HOME=/usr/lib/jvm/java-8-openjdk

# TOOLS
alias urldecode='python3 -c "import sys, urllib.parse as ul; \
    print(ul.unquote_plus(sys.argv[1]))"'

alias urlencode='python3 -c "import sys, urllib.parse as ul; \
    print (ul.quote_plus(sys.argv[1]))"'

#######################################################################################################

[[ $- != *i* ]] && return

colors() {
	local fgc bgc vals seq0

	printf "Color escapes are %s\n" '\e[${value};...;${value}m'
	printf "Values 30..37 are \e[33mforeground colors\e[m\n"
	printf "Values 40..47 are \e[43mbackground colors\e[m\n"
	printf "Value  1 gives a  \e[1mbold-faced look\e[m\n\n"

	# foreground colors
	for fgc in {30..37}; do
		# background colors
		for bgc in {40..47}; do
			fgc=${fgc#37} # white
			bgc=${bgc#40} # black

			vals="${fgc:+$fgc;}${bgc}"
			vals=${vals%%;}

			seq0="${vals:+\e[${vals}m}"
			printf "  %-9s" "${seq0:-(default)}"
			printf " ${seq0}TEXT\e[m"
			printf " \e[${vals:+${vals+$vals;}}1mBOLD\e[m"
		done
		echo; echo
	done
}

[ -r /usr/share/bash-completion/bash_completion ] && . /usr/share/bash-completion/bash_completion

# Change the window title of X terminals
case ${TERM} in
	xterm*|rxvt*|Eterm*|aterm|kterm|gnome*|interix|konsole*)
		PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/\~}\007"'
		;;
	screen*)
		PROMPT_COMMAND='echo -ne "\033_${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/\~}\033\\"'
		;;
esac

use_color=true

# Set colorful PS1 only on colorful terminals.
# dircolors --print-database uses its own built-in database
# instead of using /etc/DIR_COLORS.  Try to use the external file
# first to take advantage of user additions.  Use internal bash
# globbing instead of external grep binary.
safe_term=${TERM//[^[:alnum:]]/?}   # sanitize TERM
match_lhs=""
[[ -f ~/.dir_colors   ]] && match_lhs="${match_lhs}$(<~/.dir_colors)"
[[ -f /etc/DIR_COLORS ]] && match_lhs="${match_lhs}$(</etc/DIR_COLORS)"
[[ -z ${match_lhs}    ]] \
	&& type -P dircolors >/dev/null \
	&& match_lhs=$(dircolors --print-database)
[[ $'\n'${match_lhs} == *$'\n'"TERM "${safe_term}* ]] && use_color=true

if ${use_color} ; then
	# Enable colors for ls, etc.  Prefer ~/.dir_colors #64489
	if type -P dircolors >/dev/null ; then
		if [[ -f ~/.dir_colors ]] ; then
			eval $(dircolors -b ~/.dir_colors)
		elif [[ -f /etc/DIR_COLORS ]] ; then
			eval $(dircolors -b /etc/DIR_COLORS)
		fi
	fi

	if [[ ${EUID} == 0 ]] ; then
		PS1='\[\033[01;31m\][\h\[\033[01;36m\] \W\[\033[01;31m\]]\$\[\033[00m\] '
	else
		PS1='\[\033[01;32m\][\u@\h\[\033[01;37m\] \W\[\033[01;32m\]]\$\[\033[00m\] '
	fi

	alias ls='ls --color=auto'
	alias grep='grep --colour=auto'
	alias egrep='egrep --colour=auto'
	alias fgrep='fgrep --colour=auto'
else
	if [[ ${EUID} == 0 ]] ; then
		# show root@ when we don't have colors
		PS1='\u@\h \W \$ '
	else
		PS1='\u@\h \w \$ '
	fi
fi

unset use_color safe_term match_lhs sh

alias cp="cp -i"                          # confirm before overwriting something
alias df='df -h'                          # human-readable sizes
alias free='free -m'                      # show sizes in MB
alias np='nano -w PKGBUILD'
alias more=less
alias timesync="sudo ntpdate -u pool.ntp.org"

xhost +local:root > /dev/null 2>&1

complete -cf sudo

# Bash won't get SIGWINCH if another process is in the foreground.
# Enable checkwinsize so that bash will check the terminal size when
# it regains control.  #65623
# http://cnswww.cns.cwru.edu/~chet/bash/FAQ (E11)
shopt -s checkwinsize

shopt -s expand_aliases

# export QT_SELECT=4

# Enable history appending instead of overwriting.  #139609
shopt -s histappend

#
# # ex - archive extractor
# # usage: ex <file>
ex ()
{
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1   ;;
      *.tar.gz)    tar xzf $1   ;;
      *.bz2)       bunzip2 $1   ;;
      *.rar)       unrar x $1     ;;
      *.gz)        gunzip $1    ;;
      *.tar)       tar xf $1    ;;
      *.tbz2)      tar xjf $1   ;;
      *.tgz)       tar xzf $1   ;;
      *.zip)       unzip $1     ;;
      *.Z)         uncompress $1;;
      *.7z)        7z x $1      ;;
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}
