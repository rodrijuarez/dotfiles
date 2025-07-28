# Homebrew setup
eval "$(/opt/homebrew/bin/brew shellenv)"

# Bash completion
[[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && . "/usr/local/etc/profile.d/bash_completion.sh"

# Source profile
source ~/.profile

# Add `~/bin` to the `$PATH`
export PATH=/usr/local/bin:$PATH
export PATH="$HOME/bin:$PATH";

# Load the shell dotfiles, and then some:
# * ~/.path can be used to extend `$PATH`.
# * ~/.extra can be used for other settings you don't want to commit.
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

# Additional bash completion
if [ -f $(brew --prefix)/etc/bash_completion ]; then
	. $(brew --prefix)/etc/bash_completion
fi

# Autojump
[[ -s $(brew --prefix)/etc/profile.d/autojump.sh ]] && . $(brew --prefix)/etc/profile.d/autojump.sh

# NVM setup
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm
[ -s "$HOMEBREW_PREFIX/opt/nvm/nvm.sh" ] && \. "$HOMEBREW_PREFIX/opt/nvm/nvm.sh" # This loads nvm (homebrew)
[ -s "$HOMEBREW_PREFIX/opt/nvm/etc/bash_completion.d/nvm" ] && \. "$HOMEBREW_PREFIX/opt/nvm/etc/bash_completion.d/nvm" # This loads nvm bash_completion

# Use Node 18.17.0 by default
nvm use 18.17.0 &>/dev/null || nvm install 18.17.0 &>/dev/null

# Terminal colors
export CLICOLOR=1
export COLORFGBG="15;0"

# Java configuration (active)
export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_191.jdk/Contents/Home/
export PATH=${PATH}:/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/bin
export PATH=${PATH}:~/bin/mongodb/bin

# Google Cloud SDK (update path to current user)
if [ -f "$HOME/bin/google-cloud-sdk/path.bash.inc" ]; then 
    source "$HOME/bin/google-cloud-sdk/path.bash.inc"; 
fi

# Neofetch display
if [ -z "$TMUX" ]; then
  neofetch --kitty wallpaper
else
  neofetch --ascii ~/Documents/doge_ascii
fi

# SDKMAN (update path to current user)
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

# Nix installer (update path to current user)
if [ -e "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then 
    . "$HOME/.nix-profile/etc/profile.d/nix.sh"; 
fi

# Git completion
source ~/.git-completion.bash

# API Keys and secrets should be in ~/.extra or environment variables
# Example: export OPENAI_API_KEY="your-key-here" in ~/.extra

# Initialize modern CLI tools (if installed)
command -v zoxide > /dev/null && eval "$(zoxide init bash)"
command -v fzf > /dev/null && eval "$(fzf --bash)"

# Setup git to use delta for diffs (if installed)
if command -v delta > /dev/null; then
    git config --global core.pager delta
    git config --global interactive.diffFilter 'delta --color-only'
    git config --global delta.navigate true
    git config --global delta.light false
    git config --global merge.conflictstyle diff3
    git config --global diff.colorMoved default
fi