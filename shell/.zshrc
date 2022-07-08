# https://scriptingosx.com/2019/06/moving-to-zsh/
# https://jonasjacek.github.io/colors/
PROMPT='
%F{94}~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%f
〔%B%F{94}%~%f%b %# 〕' # %F{94} is orange4

# Shortcuts
alias cd..="cd .."
alias b="cd .."
alias bb="cd ../.."

alias l="ls -lFGh"
alias la="l -a"
alias lp="ls -p"
alias h="history -100"          # show last 100 records
alias ha="history 1"            # show all records
alias treed="find . -print | sed -e 's;[^/]*/;|____;g;s;____|; |;g'"

# https://unix.stackexchange.com/questions/273861/unlimited-history-in-zsh
HISTFILE="$HOME/.zsh_history"
HISTSIZE=10000000
SAVEHIST=10000000
setopt BANG_HIST                 # Treat the '!' character specially during expansion.
setopt EXTENDED_HISTORY          # Write the history file in the ":start:elapsed;command" format.
setopt INC_APPEND_HISTORY        # Write to the history file immediately, not when the shell exits.
setopt SHARE_HISTORY             # Share history between all sessions.
setopt HIST_EXPIRE_DUPS_FIRST    # Expire duplicate entries first when trimming history.
setopt HIST_IGNORE_DUPS          # Don't record an entry that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS      # Delete old recorded entry if new entry is a duplicate.
setopt HIST_FIND_NO_DUPS         # Do not display a line previously found.
setopt HIST_IGNORE_SPACE         # Don't record an entry starting with a space.
setopt HIST_SAVE_NO_DUPS         # Don't write duplicate entries in the history file.
setopt HIST_REDUCE_BLANKS        # Remove superfluous blanks before recording entry.
setopt HIST_VERIFY               # Don't execute immediately upon history expansion.
setopt HIST_BEEP                 # Beep when accessing nonexistent history.

# export binary path and set env variable for path home
# example: export_bin_path ~ HOME
export_bin_path() {
    bin_path=$1
    bin_home_name=$2
    if [ -z "$bin_path" ]; then
        echo "export bin path is not supplied, exit."
        exit 1
    else
        if [ ! -z "$bin_home_name" ]; then
            export "${bin_home_name}"="${bin_path}"
        fi
        export PATH="${bin_path}/bin:${PATH}"
    fi
}

# export_bin_path $(go env GOPATH) GOPATH
export_bin_path "-Xms512m -Xmx1G" MAVEN_OPTS # options for JVM running maven

# Homebrew
export_bin_path /usr/local/opt/openjdk@11 JAVA_HOME
export_bin_path /usr/local/opt/python/libexec
export_bin_path /usr/local/opt/mysql@5.7  # mysql

# standalone installation pkg
# export_bin_path $(/usr/libexec/java_home) JAVA_HOME
# export_bin_path /opt/local/share/java/maven3 MVN_HOME

# go back one level and change to directory (cd to same level directory)
back_and_cd() {
    dir=$1
    if [ -z "$dir" ]; then
        cd ..
    else
        cd ..; cd "${dir}"
    fi
}
alias bd=back_and_cd

# Misc
if [ -r ~/.git_profile ]; then
    source ~/.git_profile
fi

export HOMEBREW_NO_AUTO_UPDATE=1

# zsh plugins
source /usr/local/share/zsh-autosuggestions/zsh-autosuggestions.zsh
if type brew &>/dev/null; then
    FPATH=$(brew --prefix)/share/zsh-completions:$FPATH
    autoload -Uz compinit
    compinit
fi
source "/usr/local/opt/zsh-git-prompt/zshrc.sh"

# proxy
# export http_proxy=127.0.0.1:1087
# export https_proxy=127.0.0.1:1087
alias setproxy="export http_proxy=127.0.0.1:1087;export https_proxy=127.0.0.1:1087"
