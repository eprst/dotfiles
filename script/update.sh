#!/usr/bin/env bash
set -e
echo -e "  \e[32mrunning git pull\e[39m"
git pull
echo -e "  \e[32mrunning git submodule update --recursive\e[39m"
git submodule update --recursive
echo -e "  \e[32mrunning git submodule foreach git checkout master\e[39m"
git submodule foreach git checkout master
