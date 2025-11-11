function fetch-aur() {
  git clone --depth 1 --branch "$1" https://github.com/archlinux/aur.git "$1"
}
