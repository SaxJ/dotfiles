#!/bin/bash
. /etc/os-release
read -r -e -p "Install dev packages?" -N 1 install

if [[ "$NAME" = "Arch Linux" && "$install" == [Yy]* ]]; then
    printf "\n\nInstall my dev packages...\n\n"
    paru -S --needed \
    broot \
    cronie \
    pnpm \
    graphite-cli-git\
    racket \
    starship \
    taplo \
    typescript-language-server \
    vscode-css-languageserver \
    vscode-html-languageserver \
    yaml-language-server \
    aspell \
    aspell-en \
    aspnet-runtime \
    aspnet-targeting-pack \
    aspnet-targeting-pack-6.0 \
    atool \
    autoconf \
    automake \
    aws-cli-v2 \
    aws-session-manager-plugin \
    base-devel \
    bash-completion \
    bash-language-server \
    bat \
    calc \
    ccls \
    chezmoi \
    cmake \
    composer \
    devtools \
    direnv \
    dotnet-runtime \
    dotnet-sdk \
    dotnet-sdk-6.0 \
    dropbox \
    editorconfig-core-c \
    efibootmgr \
    eslint \
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
    hoogle \
    hub \
    hurl-bin \
    hyprland \
    hyprpaper \
    hyprshot \
    i3lock \
    isync \
    jdk-openjdk \
    jq \
    js-beautify \
    lazygit \
    lua-language-server \
    make \
    mako \
    man-db \
    marked \
    mermaid-cli \
    moar \
    neovim \
    net-tools \
    netcoredbg \
    netstandard-targeting-pack-bin \
    networkmanager \
    newsboat \
    nmap \
    nodejs-intelephense \
    npm \
    nvm \
    omnisharp-roslyn-bin \
    openvpn3 \
    p7zip \
    pdfjs \
    php \
    php-gd \
    php-imagick \
    php-pgsql \
    php-sodium \
    php-sqlite \
    pkgconf \
    procs \
    psysh \
    pyright \
    python-adblock \
    python-lsp-server \
    python-pip \
    reflector \
    ripgrep \
    rsync \
    rust \
    rust-analyzer \
    sbcl \
    sed \
    shellcheck \
    slack-desktop \
    stylelint \
    stylua \
    terraform \
    texinfo \
    tidy \
    unzip \
    vim \
    wget \
    which \
    yarn \
    zip \
    zoxide
fi
