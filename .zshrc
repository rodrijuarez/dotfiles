# Modern Zsh Configuration with Enhanced Theming
# =============================================================================

# Performance: Enable Powerlevel10k instant prompt
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Environment Setup
# =============================================================================

# Homebrew
eval "$(/opt/homebrew/bin/brew shellenv)"

# History Configuration
HISTFILE=~/.zsh_history
HISTSIZE=50000
SAVEHIST=50000

# History Options
setopt HIST_IGNORE_DUPS      # Don't record duplicates
setopt HIST_IGNORE_ALL_DUPS  # Delete old duplicate entries
setopt HIST_IGNORE_SPACE     # Don't record entries starting with space
setopt HIST_SAVE_NO_DUPS     # Don't write duplicates to history file
setopt HIST_VERIFY           # Show command with history expansion before running
setopt INC_APPEND_HISTORY    # Write to history file immediately
setopt SHARE_HISTORY         # Share history between sessions
setopt EXTENDED_HISTORY      # Record timestamp of command

# Zsh Enhancement Options
setopt AUTO_CD               # cd into directory just by typing name
setopt CORRECT               # spelling correction for commands
setopt AUTO_LIST             # automatically list choices on ambiguous completion
setopt AUTO_MENU             # automatically use menu completion
setopt ALWAYS_TO_END         # move cursor to end if word had one match
setopt COMPLETE_IN_WORD      # allow completion in middle of word
setopt GLOB_DOTS             # include dotfiles in globbing
setopt EXTENDED_GLOB         # enable extended globbing
setopt PROMPT_SUBST          # enable parameter expansion in prompts

# Key Bindings
# =============================================================================

bindkey -e  # Emacs mode
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down
bindkey '^[[1;5C' forward-word                     # Ctrl+Right
bindkey '^[[1;5D' backward-word                    # Ctrl+Left
bindkey '^[[3~' delete-char                        # Delete key
bindkey '^[[H' beginning-of-line                   # Home key
bindkey '^[[F' end-of-line                         # End key

# Load Dotfiles
# =============================================================================

for file in ~/.{path,exports,aliases,functions,extra}; do
	[ -r "$file" ] && [ -f "$file" ] && source "$file"
done
unset file

# Completion System
# =============================================================================

# Initialize completion system
autoload -Uz compinit
if [[ -n ~/.zcompdump(#qN.mh+24) ]]; then
    compinit
else
    compinit -C
fi

# Completion styling
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*' group-name ''
zstyle ':completion:*:descriptions' format '%F{yellow}-- %d --%f'
zstyle ':completion:*:warnings' format '%F{red}No matches found%f'
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*:cd:*' ignore-parents parent pwd
zstyle ':completion:*' special-dirs true

# Git completion improvements
zstyle ':completion:*:git-checkout:*' sort false
zstyle ':completion:*:git-branch:*' sort false

# Modern CLI Tools Integration
# =============================================================================

# Autojump (provides 'j' command)
[[ -s $(brew --prefix)/etc/profile.d/autojump.sh ]] && . $(brew --prefix)/etc/profile.d/autojump.sh

# Zoxide (better cd - provides 'z' command)
command -v zoxide > /dev/null && eval "$(zoxide init zsh)"

# FZF (fuzzy finder)
if command -v fzf > /dev/null; then
    source <(fzf --zsh)
    
    # Custom FZF configuration with Gruvbox colors
    export FZF_DEFAULT_OPTS="
        --height 50% 
        --layout=reverse 
        --border=rounded
        --margin=1
        --padding=1
        --preview 'bat --style=numbers --color=always --line-range :500 {}'
        --preview-window 'right:50%:wrap'
        --bind 'ctrl-u:preview-page-up,ctrl-d:preview-page-down'
        --color=bg+:#3c3836,bg:#282828,spinner:#fb4934,hl:#928374
        --color=fg:#ebdbb2,header:#928374,info:#8ec07c,pointer:#fb4934
        --color=marker:#fb4934,fg+:#ebdbb2,prompt:#fb4934,hl+:#fb4934
        --color=border:#504945
    "
    
    # Enhanced FZF functions
    export FZF_CTRL_T_OPTS="--preview 'bat --style=numbers --color=always {}'"
    export FZF_ALT_C_OPTS="--preview 'eza --tree --color=always {} | head -200'"
fi

# Starship prompt
command -v starship > /dev/null && eval "$(starship init zsh)"

# Plugin System
# =============================================================================

# Zsh Autosuggestions
if [[ -f /opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh ]]; then
    source /opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh
    ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#585858,underline"
    ZSH_AUTOSUGGEST_STRATEGY=(history completion)
    ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
fi

# Enhanced Functions
# =============================================================================

# Smart directory listing
function l() {
    if command -v eza > /dev/null; then
        eza --long --header --icons --git --group-directories-first "$@"
    else
        ls -la "$@"
    fi
}

# Interactive git checkout with FZF
function gco() {
    local branches branch
    branches=$(git --no-pager branch -vv) &&
    branch=$(echo "$branches" | fzf +m) &&
    git checkout $(echo "$branch" | awk '{print $1}' | sed "s/.* //")
}

# Quick project navigation
function proj() {
    local dir
    dir=$(find ~/Documents/Projects ~/Documents -maxdepth 2 -type d 2>/dev/null | fzf) && cd "$dir"
}

# Enhanced grep with ripgrep and fzf
function search() {
    if command -v rg > /dev/null && command -v fzf > /dev/null; then
        rg --color=always --line-number --no-heading --smart-case "${1:-}" |
        fzf --ansi \
            --color "hl:-1:underline,hl+:-1:underline:reverse" \
            --delimiter : \
            --preview 'bat --color=always {1} --highlight-line {2}' \
            --preview-window 'up,60%,border-bottom,+{2}+3/3,~3'
    else
        echo "Please install ripgrep and fzf for enhanced search"
    fi
}

# NVM Integration
# =============================================================================

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"

# Performance optimizations
if [[ -f "$NVM_DIR/nvm.sh" ]]; then
    # Lazy load nvm to improve shell startup time
    nvm() {
        unset -f nvm
        [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
        nvm "$@"
    }
fi

# Final Setup
# =============================================================================

# Load local .fzf.zsh if it exists
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Syntax highlighting (must be at the end)
if [[ -f /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]]; then
    source /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
    
    # Customize highlighting colors
    ZSH_HIGHLIGHT_STYLES[command]='fg=green,bold'
    ZSH_HIGHLIGHT_STYLES[alias]='fg=magenta,bold'
    ZSH_HIGHLIGHT_STYLES[builtin]='fg=yellow,bold'
    ZSH_HIGHLIGHT_STYLES[function]='fg=blue,bold'
    ZSH_HIGHLIGHT_STYLES[command-substitution]='fg=cyan'
    ZSH_HIGHLIGHT_STYLES[path]='fg=cyan,underline'
fi

# Welcome message for new sessions
if [[ -o interactive ]] && [[ -z "$TMUX" ]]; then
    echo "🚀 Welcome to your enhanced Zsh environment!"
    echo "📁 Use 'proj' to navigate projects, 'search' for advanced file search"
    echo "🔍 FZF integrated: Ctrl+T (files), Ctrl+R (history), Alt+C (directories)"
fi