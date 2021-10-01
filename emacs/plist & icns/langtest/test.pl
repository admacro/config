#!/usr/bin/perl
use warnings;
use strict;

my $cmd;
print("Enter a command, enter exit to quit.\n");       
do {
  print(">");       
  chomp($cmd = <STDIN>);  
  $cmd = lc($cmd);
  print("You entered:$cmd\n");
}until($cmd eq "exit");
