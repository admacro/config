#!/bin/sh

# install sleepwatcher via home brew
#  > brew install sleepwatcher
# start sleepwatcher as a service
#  > brew services start sleepwatcher
# copy this file to home folder and save it as .wakeup

# restart dumng-sync on wakeup
pgrep -f dumang-sync | xargs kill
/usr/local/bin/dumang-sync > /dev/null 2>&1 &
