#!/usr/bin/env bash

set -o xtrace

SOURCE="${BASH_SOURCE[0]}"
if [[ -h $SOURCE ]]; then
  SOURCE="$(readlink "$SOURCE")"
fi

DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
PROFILE=/root/.bashrc
SMF_ROOT=/var/svc/manifest/network
SVC_ROOT=/opt/podest/manger

copy() {
  if [ -n "$(svcs manger | grep -sq online)" ]; then
    echo "manger appears to be online"
    exit 1
  fi
  mkdir -p $SVC_ROOT
  cp "${DIR}/package.json" $SVC_ROOT
  cp "${DIR}/*.js" $SVC_ROOT
  cp -rf "${DIR}/node_modules" $SVC_ROOT
}

import_manifest() {
  cp "${DIR}/smf/manifests/manger.xml" "$SMF_ROOT"
  svcadm restart manifest-import
  svcadm enable manger
}

schedule_updates() {
  local job="0 * * * * curl -s -X PUT localhost/feeds >/dev/null 2>&1"
  if [ "$( crontab -l | grep -sq localhost/feeds )" ]; then
    echo "** Updates already scheduled"
  else
    (crontab -l; echo "$job" ) | crontab
  fi
}

main() {
  if [ $(uname -s) == "Darwin" ]; then
    mkdir -p ~/Library/LaunchAgents
    cp "${DIR}/LaunchAgents/ink.codes.manger-update.plist" ~/Library/LaunchAgents
    launchctl load -w ~/Library/LaunchAgents/ink.codes.manger-update.plist
    exit 0
  fi
  copy
  import_manifest
  schedule_updats
}

main
