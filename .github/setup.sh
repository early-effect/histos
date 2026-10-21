#!/bin/sh
# Tools the engine tests need on a GitHub runner. A local machine already has them.
set -eu
if ! command -v apt-get >/dev/null 2>&1; then
  exit 0
fi
sudo apt-get update
sudo apt-get install -y clang lld libstdc++-dev libgc-dev xz-utils
if ! command -v node >/dev/null 2>&1 || ! node -e 'process.exit(Number(process.versions.node.split(".")[0]) >= 20 ? 0 : 1)'; then
  curl -fsSL https://nodejs.org/dist/v22.21.0/node-v22.21.0-linux-x64.tar.xz -o /tmp/node.tar.xz
  sudo tar -xJf /tmp/node.tar.xz -C /usr/local --strip-components=1
fi
