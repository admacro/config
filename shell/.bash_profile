# Command prompt
PS1="~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n〔\u@\W〕"

# Shortcuts
alias cd..="cd .."
alias b="cd .."
alias bb="cd ../.."

alias l="ls -lFGh"
alias la="l -a"
alias lp="ls -p"
alias h="history"
alias treed="find . -print | sed -e 's;[^/]*/;|____;g;s;____|; |;g'"

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

export_bin_path $(go env GOPATH) GOPATH
export_bin_path $(/usr/libexec/java_home) JAVA_HOME
export_bin_path /usr/local/opt/python/libexec # Python (Homebrew)

# Misc
if [ -r ~/.git_profile ]; then
   source ~/.git_profile
fi
if [ -r ~/.tumblr_profile ]; then
   source ~/.tumblr_profile
fi

export HOMEBREW_NO_AUTO_UPDATE=1
