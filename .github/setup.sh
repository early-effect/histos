#!/bin/sh
# Tools the engine tests need on a GitHub runner. A local machine already has them.
set -eu
if ! command -v apt-get >/dev/null 2>&1; then
  exit 0
fi
sudo apt-get update
sudo apt-get install -y clang lld build-essential xz-utils
if ! command -v node >/dev/null 2>&1 || ! node -e 'process.exit(Number(process.versions.node.split(".")[0]) >= 20 ? 0 : 1)'; then
  case $(uname -m) in
    x86_64) node_arch=x64 ;;
    aarch64|arm64) node_arch=arm64 ;;
    *) echo "histos setup: no node build for $(uname -m)" >&2; exit 1 ;;
  esac
  node_version=v22.20.0
  curl -fsSL "https://nodejs.org/dist/${node_version}/node-${node_version}-linux-${node_arch}.tar.xz" -o /tmp/node.tar.xz
  sudo tar -xJf /tmp/node.tar.xz -C /usr/local --strip-components=1
fi
