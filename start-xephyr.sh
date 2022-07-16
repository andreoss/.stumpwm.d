#!/bin/sh
XDISPLAY_NUM=3
XDISPLAY=":${XDISPLAY_NUM}"
export XDISPLAY
Xephyr -fp ~/.fonts/ -ac -br ":${XDISPLAY_NUM}" -resizeable &
XEPHYR_PID=$!
sleep 1
DISPLAY=$XDISPLAY xrdb "$HOME/.Xresources"
DISPLAY=$XDISPLAY STUMPWM_INNER="$XDISPLAY_NUM" "$HOME/.stumpwm.d/init.ros"
wait "$XEPHYR_PID"
