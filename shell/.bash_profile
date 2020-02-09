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
alias treed="find . -print | sed -e 's;[^/]*/;|____;g;s;____|; |;g'"


# Java
export JAVA_HOME=$(/usr/libexec/java_home)
export PATH=$JAVA_HOME/bin:$PATH

# Go
export GOPATH=$(go env GOPATH)
export PATH=$GOPATH/bin:$PATH

# Ruby
eval "$(rbenv init -)"
export PATH="$HOME/.rbenv/shims:$PATH"


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

