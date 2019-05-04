# Command prompt
PS1="\u@\W$ "


# Shortcuts
alias cd..="cd .."
alias b="cd .."
alias bb="cd ../.."

alias l="ls -lFG"
alias la="l -a"
alias lp="ls -p"
alias h="history"


# Proxy
export https_proxy=http://127.0.0.1:6152
export http_proxy=http://127.0.0.1:6152
export all_proxy=socks5://127.0.0.1:6153


# ENV variables
export JAVA_HOME=$(/usr/libexec/java_home)

PYTHON_BIN=/Users/james/Library/Python/2.7/bin
MYSQL_BIN=/usr/local/mysql/bin

export PATH=$JAVA_HOME/bin:$PYTHON_BIN:$MYSQL_BIN:$PATH


# Programming
eval "$(rbenv init -)"
export PATH="$HOME/.rbenv/bin:$PATH"


# Misc
source ~/.git_profile
source ~/.tumblr_profile
