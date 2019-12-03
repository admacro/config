# Command prompt
PS1="\u@\W$ "


# Shortcuts
alias cd..="cd .."
alias b="cd .."
alias bb="cd ../.."

alias l="ls -lFGh"
alias la="l -a"
alias lp="ls -p"
alias h="history"


# ENV variables
export JAVA_HOME=$(/usr/libexec/java_home)
export GO_HOME=/usr/local/go
export GOPATH=$HOME/go

PYTHON_BIN=/Users/james/Library/Python/2.7/bin
MYSQL_BIN=/usr/local/mysql/bin

export PATH=$JAVA_HOME/bin:$PYTHON_BIN:$MYSQL_BIN:$GO_HOME/bin:$GOPATH/bin:$PATH


# Programming
eval "$(rbenv init -)"
export PATH="$HOME/.rbenv/bin:$PATH"


# Misc
if [ -r ~/.git_profile ]; then
   source ~/.git_profile
fi
if [ -r ~/.tumblr_profile ]; then
   source ~/.tumblr_profile
fi


# # Proxy
# export https_proxy=http://127.0.0.1:6152
# export http_proxy=http://127.0.0.1:6152
# export all_proxy=socks5://127.0.0.1:6153

