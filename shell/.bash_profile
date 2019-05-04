# Shell prompt
PS1='
$PWD
\$_\$ > '

## shell aliases
alias cd..="cd .."
alias b="cd .."
alias bb="cd ../.."

alias l="ls -lFG"
alias la="l -a"
alias lp="ls -p"
alias h="history"

## git shortcuts
alias gs="git status"
alias gb="git branch"
alias gc="git checkout"
alias gp="git pull"

# .git-completion.bash
source ~/.git-completion.bash

# git log
# Usage
#   gl "cool feature" cool-feature-branch
#   gl "cool feature"
#     - will use current branch
gl() {
    if [[ -z "$2" ]]; then
        branch=`git rev-parse --abbrev-ref HEAD`
    else
        branch=$2
    fi
    author=`git config user.name`
    echo "Showing commits of $author on branch $branch"
    command git log --pretty=format:"%h | %ci | %s" --author "$author" --grep "$1" $branch
}

#===============================================================================
# ENV variables

export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_172.jdk/Contents/Home
export MAVEN_HOME=~/Applications/apache-maven-3.6.0
export ANDROID_HOME=~/Library/Android/sdk
export PATH=$JAVA_HOME/bin:$MAVEN_HOME/bin:$ANDROID_HOME/platform-tools:$ANDROID_HOME/tools/bin:$PATH

# proxy
export https_proxy=http://127.0.0.1:6152
export http_proxy=http://127.0.0.1:6152
export all_proxy=socks5://127.0.0.1:6153

#===============================================================================
### Development ###

## Ruby
alias cop=rubocop
alias sp="git status | grep spec.rb | cut -d':' -f2 | xargs rspec"

eval "$(rbenv init -)"

# Nodejs
export PATH="$HOME/.ndenv/bin:$PATH"
eval "$(ndenv init -)"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

#===============================================================================
source ~/.tumblr_profile
