#!/bin/sh
. /etc/os-release
if [ "$NAME" = "Void" ]; then
	exit
fi

printf "Install paru...\n"
if ! command -v paru &> /dev/null
then
    sudo pacman -S --needed base-devel
    git clone https://aur.archlinux.org/paru.git ~/.paru
    cd ~/.paru || return
    makepkg -si
else
    printf "  Already installed\n"
fi

printf "\n\nInstall my dev packages..."
paru -S --needed \
    aerc \
    alacritty \
    aspell \
    aspell-en \
    aspnet-runtime \
    aspnet-targeting-pack \
    atool \
    autoconf \
    automake \
    aws-cli-v2-bin \
    aws-session-manager-plugin \
    base-devel \
    bash-completion \
    bash-language-server \
    bat \
    bitwarden-cli \
    bitwarden-rofi \
    calc \
    ccls \
    chezmoi \
    cmake \
    composer \
    devtools \
    direnv \
    discord \
    docker \
    docker-compose \
    dotnet-runtime \
    dotnet-sdk \
    dropbox \
    dunst \
    editorconfig-core-c \
    efibootmgr \
    elm-bin \
    elm-format-bin \
    elm-language-server \
    fd \
    firefox-developer-edition \
    fzf \
    ghcup-hs-bin \
    git \
    github-cli \
    glslang \
    gnome-keyring \
    go-jira \
    go-tools \
    gobuster-bin \
    gomodifytags \
    gopass \
    gopls \
    gore \
    gotests \
    gotop-bin \
    gparted \
    graphviz \
    helix \
    heroku-cli-bin \
    hoogle \
    htop \
    hub \
    i3-gaps \
    i3blocks \
    i3lock \
    i3status \
    i3status-rust \
    insomnia-bin \
    isync \
    jdk-openjdk \
    jq \
    js-beautify \
    keynav \
    kitty \
    lazygit \
    make \
    man-db \
    marked \
    neovim \
    nerd-fonts-complete \
    net-tools \
    netstandard-targeting-pack-bin \
    networkmanager \
    nitrogen \
    nmap \
    nnn \
    nodejs-intelephense \
    nodejs-lts-gallium \
    npm \
    nvm \
    nyxt \
    omnisharp-roslyn \
    openvpn3 \
    p7zip \
    pacmanfile \
    paru-bin \
    pass \
    pass-import \
    pass-otp \
    pdfjs \
    php \
    php-gd \
    php-imagick \
    php-imap \
    php-intl \
    php-sodium \
    php7 \
    php7-gd \
    php7-igbinary \
    php7-imagick \
    php7-imap \
    php7-intl \
    php7-memcache \
    php7-memcached \
    php7-pgsql \
    php7-phpdbg \
    php7-sodium \
    php7-sqlite \
    picom \
    pkgconf \
    polybar \
    procs \
    psysh \
    pulseaudio-alsa \
    pyright \
    python-adblock \
    python-lsp-server \
    python-pip \
    qutebrowser \
    rcm \
    reflector \
    ripgrep \
    rofi \
    roswell \
    rsync \
    rust \
    rust-analyzer \
    sbcl \
    scrot \
    sed \
    sfdx-cli \
    shellcheck \
    slack-desktop \
    spectrwm \
    spotify \
    sshfs \
    starship \
    stylelint \
    syncthing-gtk-python3 \
    task \
    taskwarrior-tui \
    terraform \
    texinfo \
    tidy \
    tor \
    unison \
    unzip \
    vim \
    visual-studio-code-bin \
    wget \
    which \
    xclip \
    xdg-utils \
    xdotool \
    yarn \
    zellij \
    zip \
    zoom \
    zoxide
