if status is-interactive
    # Commands to run in interactive sessions can go here
    eval "$(brew shellenv)"
end

set -U fish_greeting
set -g tide_right_prompt_items status cmd_duration context jobs direnv node python rustc java php pulumi ruby go gcloud distrobox toolbox terraform aws nix_shell crystal elixir time
set -gx EDITOR nvim
set -x GOROOT $HOME/sdk/go1.19.6
set -x PATH $GOROOT/bin:$PATH
set -x GOPATH $HOME/observe/code/go/src

set -x CLOUD_INSTANCE i-0d4adb011188b1eaf
alias istart="aws ec2 start-instances --instance-ids $CLOUD_INSTANCE"
alias istat="aws ec2 describe-instances --instance-ids $CLOUD_INSTANCE --output json | jq '.Reservations[0].Instances[0].State.Name'"
alias istata="aws ec2 describe-instances --instance-ids $CLOUD_INSTANCE --output json | jq | less"
alias istop="aws ec2 stop-instances --instance-ids $CLOUD_INSTANCE"
alias issh="ssh -A -D 8989 -L 5006:localhost:5006 -L15432:localhost:15432 -L8385:localhost:8384 konstantin@52.33.219.229"

abbr -a g git
abbr -a st 'git status'
abbr -a gl "git --no-pager log --pretty=format:'%C(yellow)%h %Cred%ad %Cblue%<(20)%an%Cgreen%d %Creset%s' --date=short"
abbr -a br 'git branch -v | cat -'
abbr -a gpr 'git push review'

abbr -a --position anywhere G '| grep'
abbr -a --position anywhere L '| less'
abbr -a --position anywhere psg 'ps auxw | grep'

abbr -a v 'nvim'
abbr -a ... 'cd ../..'
abbr -a .... 'cd ../../..'

switch (uname)
    case Linux
        abbr -a cal 'TERM="xterm-256color" cal -m -3'
        abbr -a caly 'TERM="xterm-256color" cal -m -y'
    case Darwin
        abbr -a cal 'gcal -m .'
end
