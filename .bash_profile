source ~/.profile
# Add `~/bin` to the `$PATH`
export PATH=/usr/local/bin:$PATH
export PATH="$HOME/bin:$PATH";

# Load the shell dotfiles, and then some:
# * ~/.path can be used to extend `$PATH`.
# * ~/.extra can be used for other settings you don’t want to commit.
for file in ~/.{path,bash_prompt,exports,aliases,functions,extra}; do
	[ -r "$file" ] && [ -f "$file" ] && source "$file";
done;
unset file;

# Case-insensitive globbing (used in pathname expansion)
shopt -s nocaseglob;

# Append to the Bash history file, rather than overwriting it
shopt -s histappend;

# Autocorrect typos in path names when using `cd`
shopt -s cdspell;

# Enable some Bash 4 features when possible:
# * `autocd`, e.g. `**/qux` will enter `./foo/bar/baz/qux`
# * Recursive globbing, e.g. `echo **/*.txt`
for option in autocd globstar; do
	shopt -s "$option" 2> /dev/null;
done;

# Add tab completion for many Bash commands
if which brew &> /dev/null && [ -f "$(brew --prefix)/share/bash-completion/bash_completion" ]; then
	source "$(brew --prefix)/share/bash-completion/bash_completion";
elif [ -f /etc/bash_completion ]; then
	source /etc/bash_completion;
fi;

# Enable tab completion for `g` by marking it as an alias for `git`
complete -o default -o nospace -F _git g;

# Add tab completion for SSH hostnames based on ~/.ssh/config, ignoring wildcards
[ -e "$HOME/.ssh/config" ] && complete -o "default" -o "nospace" -W "$(grep "^Host" ~/.ssh/config | grep -v "[?*]" | cut -d " " -f2- | tr ' ' '\n')" scp sftp ssh;

# Add tab completion for `defaults read|write NSGlobalDomain`
# You could just use `-g` instead, but I like being explicit
complete -W "NSGlobalDomain" defaults;

# Add `killall` tab completion for common apps
complete -o "nospace" -W "Contacts Calendar Dock Finder Mail Safari iTunes SystemUIServer Terminal Twitter" killall;

if [ -f $(brew --prefix)/etc/bash_completion ]; then
	. $(brew --prefix)/etc/bash_completion
fi

[[ -s $(brew --prefix)/etc/profile.d/autojump.sh ]] && . $(brew --prefix)/etc/profile.d/autojump.sh

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm

export CLICOLOR=1
export TERM=xterm-kitty
export COLORFGBG="15;0"

# Java configs
#export JAVA_HOME=$(/Library/Java/JavaVirtualMachines/jdk-9.0.4.jdk/Contents/Home)
#export PATH=${PATH}:/Library/Java/JavaVirtualMachines/jdk-9.0.4.jdk/Contents/Home/bin
#export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home
export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_191.jdk/Contents/Home/
export PATH=${PATH}:/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/bin
export PATH=${PATH}:~/bin/mongodb/bin

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/zenhomes/bin/google-cloud-sdk/path.bash.inc' ]; then source '/Users/zenhomes/bin/google-cloud-sdk/path.bash.inc'; fi

if [ -z "$TMUX" ]; then
  neofetch --kitty wallpaper
else
  neofetch --ascii ~/Documents/doge_ascii
fi

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/Users/user/.sdkman"
[[ -s "/Users/user/.sdkman/bin/sdkman-init.sh" ]] && source "/Users/user/.sdkman/bin/sdkman-init.sh"
if [ -e /Users/user/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/user/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
export PATH="$HOME/.jenv/bin:$PATH"
eval "$(jenv init -)"
source ~/.git-completion.bash
