#!/bin/bash
tmux has-session -t dev
if [ $? != 0 ]
then
    tmux new-session -s dev -n "Cauldron" -d

    tmux split-window -h -t dev:0
    tmux split-window -v -t dev:0.1
    tmux send-keys -t dev:0.0 'j cauldron && yarn storybook' C-m
    tmux send-keys -t dev:0.1 'j cauldron' C-m
    tmux send-keys -t dev:0.2 'j cauldron' C-m

    tmux new-window -n "Vim" -t dev
    tmux send-keys -t dev:1.0 'j cauldron && v package.json' C-m

    tmux new-window -n "Tests" -t dev
    tmux send-keys -t dev:2.0 'j cauldron && yarn test' C-m

    tmux select-window -t dev:0
fi
tmux attach -t dev
