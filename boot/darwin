#!/usr/bin/env bash

install_launch_agent() {
  mkdir -p ~/Library/LaunchAgents
  local plist="${DIR}/../LaunchAgents/ink.codes.manger-update.plist"
  cp "$plist"  ~/Library/LaunchAgents
  launchctl load -w ~/Library/LaunchAgents/ink.codes.manger-update.plist
}

darwin() {
  install_launch_agent
}
