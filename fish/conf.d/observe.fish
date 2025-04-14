# set -g CLOUD_INSTANCE i-0d4adb011188b1eaf
# set -g CLOUD_IP 52.33.219.229
set -g CLOUD_INSTANCE i-04321e03f1250d2ae
set -g CLOUD_IP 35.165.219.213

function observe_check_venv
    if ! test "$VIRTUAL_ENV_PROMPT" = "(observe-venv)"
        if ! test -d ~/observe-venv
            printf "set up observe venv first:\n" > /dev/stderr
            printf "python3 -m venv ~/observe-venv\n" > /dev/stderr
            printf ". ~/observe-venv/bin/activate.fish\n" > /dev/stderr
            printf "pip3 install pyopenssl click britive\n" > /dev/stderr
            return 1
        else
            source ~/observe-venv/bin/activate.fish
            return 0
        end
    end
end

function istart
    observe_check_venv && ~/observe/s/cloud-instance start $CLOUD_INSTANCE
end

function istop
    observe_check_venv && ~/observe/s/cloud-instance stop $CLOUD_INSTANCE
end

function istat
    observe_check_venv && ~/observe/s/cloud-instance list
end

alias ce="s/aws-creds checkout eng"
alias ihib="aws ec2 stop-instances --instance-ids $CLOUD_INSTANCE --hibernate"
alias issh="ssh -A -D 8989 -L 5006:localhost:5006 -L15432:localhost:15432 -L15433:localhost:15433 -L8385:localhost:8384 -L22001:localhost:22000 -R22001:localhost:22000 konstantin@$CLOUD_IP"

set -x GOROOT $HOME/.gvm/gos/go1.23.7
set -x GOPATH $HOME/observe/code/go/src
fish_add_path -P -p $GOROOT/bin

ssh-add -q ~/.ssh/observeinc-konstantin
