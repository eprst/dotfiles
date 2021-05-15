mkcd() {mkdir $1 && cd $1}
# true recursive grep, rgrep "string" "filename pattern"
rgrep() {find . -name "$2" -type f -print | xargs egrep -n "$1" 2>/dev/null}
# like rgrep, but filename pattern is "*"
rgre() {find . -type f -print | xargs egrep -n "$1" 2>/dev/null}
# find any files in the current directory that matches the pattern, ignoring the case
ft() {find . -maxdepth 1 -type f | grep -i "$1"}
# like ft but recursively
ftr() {find . -type f -iregex ".*$1.*"}
# cd and ls
cdl() {cd $1 && ls}
# grep and kill
gkill() {psg $1 | grep -v grep | awk -e '{print $2}' | xargs kill}

# function to make regexp from IDEA-like class name abbreviations {{{2
# requires grep -P support (USE="pcre" emerge grep)
function makeClassRegexp {
    local word=$1
    local res=""
    for (( i=1; $i<=${#word}; i++ )) {
        local char=$word[$i]
        if [[ $char == *[[:upper:]]* ]] {
            res=${res}"([[:lower:]]*)"
        }
        res=${res}${char}
    }
    echo $res"(.*)\.java$"
}

#function to choose a file from a result of a 'find' command
function chooseFile {
    local filelist
    filelist=( $@ )
    rm /tmp/k.fname >/dev/null 2>&1
    if [[ $#filelist = 0 ]] {
        return
    } elif [[ $#filelist = 1 ]] {
        echo $filelist[1] > /tmp/k.fname
        return
    }
    local dlg
    dlg=(dialog --menu "choose file" 0 0 0)
    local i=1
    foreach file ($filelist) {
        dlg=($dlg $i "$file")
        ((i += 1))
    }
    $dlg 2> /tmp/k.tag
    local tag=`cat /tmp/k.tag`
    rm /tmp/k.tag >/dev/null 2>&1
    if [[ $tag != "" ]] {
        echo $filelist[$tag] > /tmp/k.fname
    }
}

function editChosenFile {
    if [[ -a /tmp/k.fname ]] {
        $EDITOR `cat /tmp/k.fname`
    }
}

# edit a file matching the pattern
vtr() {
    chooseFile `find . -type f -iregex ".*$1.*"`
    editChosenFile
}
# find classes using IDEA-like name abbrevs
fcl() {
    regexp=`makeClassRegexp $1`
    find . -type f | GREPP $regexp
}
# edit classes using IDEA-like name abbrevs
vcl() {
    regexp=`makeClassRegexp $1`
    chooseFile `find . -type f | GREPP $regexp`
    editChosenFile
}

# pstop -- ps with top-like output
pstop() {
  ps -eo pid,user,pri,ni,vsz,rsz,stat,pcpu,pmem,time,comm --sort -pcpu | head "${@:--n 20}"
}

# gradle invocation helper
grd() {
  argn=$#
  if [ $argn -lt 2 ]; then
    echo "too few parameters"
    return -1
  fi
  keys=${@: 1: $(($argn-2))}
  projects=${@: -2: 1}
  tasks=${@: -1: 1}
  cmd="./gradlew $keys"
  for p in $(echo $projects | sed "s/,/ /g")
  do
    for t in $(echo $tasks | sed "s/,/ /g")
    do
      cmd="$cmd :$p:$t"
    done
  done
  echo "Running $cmd"
  eval $cmd
}
