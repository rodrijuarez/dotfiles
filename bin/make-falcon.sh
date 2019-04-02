#!/bin/bash
tmux has-session -t dev
if [ $? != 0 ]
then
    tmux new-session -s dev -n "Falcon" -d

    tmux split-window -h -t dev:0
    tmux split-window -v -t dev:0.1
    tmux send-keys -t dev:0.0 'j falcon && yarn storybook' C-m
    tmux send-keys -t dev:0.1 'j falcon' C-m
    tmux send-keys -t dev:0.2 'j falcon' C-m

    tmux new-window -n "Vim" -t dev
    tmux send-keys -t dev:1.0 'j falcon && v package.json' C-m

    tmux new-window -n "Tests" -t dev
    tmux send-keys -t dev:2.0 'j falcon && yarn test' C-m

    tmux select-window -t dev:0
fi
tmux attach -t dev
