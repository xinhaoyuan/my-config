# Dependencies

## all

stow (>= 2.2.0)
tmux
git
zsh (oh-my-zsh)
emacs

## Linux

build-essential
consolekit (ck-launch-session)
xdotool
xclip
wmctrl
libx11      (xorg-dev)
libxinerama (xorg-dev)
php         (php5-cli)
pkg-config
awesome-git
openbox (not used for now)
cmake

sudo apt-get install stow tmux git zsh emacs build-essential consolekit xdotool xclip wmctrl xorg-dev php5-cli pkg-config

For building awesome git, extra development libs are needed

`CMAKE_ARGS="-DCMAKE_INSTALL_PREFIX=/usr" make package`

## Mac
brew
reattach-to-user-namespace
iterm2

# Manual Setup

change default shell to zsh
oh-my-zsh:

git clone https://github.com/robbyrussell/oh-my-zsh.git $HOME/.oh-my-zsh
cp $HOME/.oh-my-zsh/templates/zshrc.zsh-template $HOME/.zshrc
echo 'source $HOME/.zshrc.user' >> $HOME/.zshrc

Xorg:
In /etc/X11/Xwrapper.config, change allowed_users to anybody

iTerm2:

change setting directory

dvtm terminfo issue:

change the kbs entry parent term info to ^H if backspace keep sending ^H inside dvtm

tic -s <(infocmp -1 | sed -e 's/kbs=\\177/kbs=^H/g')

Cygwin:

To change default shell to zsh:
mkpasswd -c | sed 's/bash/zsh/g' >> /etc/passwd

To fix tmux highlighting:
```
mkdir $HOME/.terminfo/
screen_terminfo="screen-256color"
infocmp "$screen_terminfo" | sed \
          -e 's/^screen[^|]*|[^,]*,/screen-256color|screen-256color with italics support,/' \
          -e 's/%?%p1%t;3%/%?%p1%t;7%/' \
          -e 's/smso=[^,]*,/smso=\\E[7m,/' \
          -e 's/rmso=[^,]*,/rmso=\\E[27m,/' \
          -e '$s/$/ sitm=\\E[3m, ritm=\\E[23m,/' > /tmp/screen.terminfo
tic -o $HOME/.terminfo /tmp/screen.terminfo
```
Optional packages (one per line in .extra-packages):

linux-x

# GMail Setting

`surf -c $HOME/.config/conky/gm-cookies.txt https://mail.google.com/mail`

# HiDPI

In .xsessionrc, set variable HIDPI to non-empty value.
