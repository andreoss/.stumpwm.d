#!/bin/sh
while :
do
   sbcl --non-interactive --load ~/.stumpwm.d/start.lisp || xterm
done
