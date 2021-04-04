
cdr=/home/$USER/.dotfiles
udr=/home/$USER

# bash
ln -sfn $cdr/.bashrc $udr/.bashrc

# xmonad
ln -sfn $cdr/xmonad/xmonad.hs $udr/.xmonad/xmonad.hs

# xmobar
ln -sfn $cdr/xmobar/xmobarc.hs $udr/.xmonad/xmobarc.hs

# qutebrowser
ln -sfn $cdr/qutebrowser/config.py $udr/.config/qutebrowser
# mpd
ln -sfn $cdr/
# ncmpcpp
ln -sfn $cdr
# kitty
ln -sfn $cdr
# dunst
ln -sfn $cdr/dunst/dunstrc $udr/.config/dunst
# zathura
ln -sfn $cdr
