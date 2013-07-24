#!/usr/bin/env bash
set -e
echo -e "  \033[32mrunning git pull\033[39m"
git pull
echo -e "  \033[32mrunning git submodule foreach git checkout master\033[39m"
git submodule foreach git checkout master
echo -e "  \033[32mrunning git submodule foreach git pull\033[39m"
git submodule foreach git pull
