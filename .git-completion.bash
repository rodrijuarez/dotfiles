# Git command completion script for Bash

_git()
{
    # Define the current word being completed
    local current_word="${COMP_WORDS[COMP_CWORD]}"

    # Define the previous word
    local previous_word="${COMP_WORDS[COMP_CWORD-1]}"

    # Define the Git command being completed
    local git_command="$(basename "${COMP_WORDS[0]}")"

    # Define the list of possible Git commands
    local git_commands="add bisect branch checkout clone commit diff fetch grep init log merge mv pull push rebase reset rm show status tag"

    # Define the list of Git commands that require a branch or tag argument
    local git_branch_commands="checkout branch merge tag show"

    # Define the list of Git commands that require a file argument
    local git_file_commands="add checkout commit diff log mv reset rm show"

    # Define the list of Git commands that require a remote argument
    local git_remote_commands="clone fetch pull push"

    # Define the list of Git commands that require a revision argument
    local git_revision_commands="bisect cherry-pick merge rebase reset show"

    # Define the list of possible Git branches and tags
    local git_refs="$(git for-each-ref --format='%(refname:short)' refs/heads refs/tags)"

    # Define the list of possible Git files
    local git_files="$(git ls-files)"

    # Define the list of possible Git remotes
    local git_remotes="$(git remote)"

    # Define the list of possible Git revisions
    local git_revisions="$(git rev-list --all)"

    # Determine the list of possible completions based on the current word and command
    case "${git_command}" in
        add)
            COMPREPLY=( $(compgen -W "${git_files}" -- ${current_word}) )
            ;;
        bisect|branch|checkout|merge|rebase|reset|show|tag)
            COMPREPLY=( $(compgen -W "${git_refs}" -- ${current_word}) )
            ;;
        clone|fetch|pull|push)
            COMPREPLY=( $(compgen -W "${git_remotes}" -- ${current_word}) )
            ;;
        cherry-pick)
            COMPREPLY=( $(compgen -W "${git_revisions}" -- ${current_word}) )
            ;;
        commit)
            if [[ "${previous_word}" =~ ^(-m|--message|-c|--reuse-message)$ ]]; then
                COMPREPLY=()
            else
                COMPREPLY=( $(compgen -W "${git_files}" -- ${current_word}) )
            fi
            ;;
        diff|log)
            COMPREPLY=( $(compgen -W "${git_files}" -- ${current_word}) )
            ;;
        grep)
            COMPREPLY=( $(compgen -W "--and --or --not --author --committer --grep --all-match --invert-grep --perl-regexp --regexp-ignore-case --word-regexp" -- ${current_word}) )
            ;;
        init)
            COMPREPLY=()
            ;;
        mv|rm)
            COMPREPLY=( $(compgen -W "${git_files}" -- ${current_word}) )
            ;;
        *)
            COMPREPLY=( $(compgen -W "${git_commands}" -- ${current_word}) )
            ;;
    esac

    return 0
}

# Set Git command completion for Bash
complete -o bashdefault -o default -F _git git
