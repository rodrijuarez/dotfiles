#!/bin/bash
tmux has-session -t cauldron
if [ $? != 0 ]
then
    tmux new-session -s cauldron -n "Cauldron" -d

    tmux split-window -h -t cauldron:0
    tmux split-window -v -t cauldron:0.1
    tmux send-keys -t cauldron:0.0 'j cauldron && yarn storybook' C-m
    tmux send-keys -t cauldron:0.1 'j cauldron' C-m
    tmux send-keys -t cauldron:0.2 'j cauldron' C-m

    tmux new-window -n "Vim" -t cauldron
    tmux send-keys -t cauldron:1.0 'j cauldron && v package.json' C-m

    tmux new-window -n "Tests" -t cauldron
    tmux send-keys -t cauldron:2.0 'j cauldron && yarn test' C-m

    tmux select-window -t cauldron:0
fi
tmux attach -t cauldron
