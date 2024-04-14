# Simple Tmux Settings

## File Paths
.tmux.conf(file): $HOME or $XDG_CONFIG_HOME/tmux/.tmux.conf (~/.config)
.tmux(dir): $HOME

## Installing TPM(Tmux Plugin Manager)

1. Git clone TPM to a HOME directory (~/.tmux/plugins/tpm)

```shell
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
```

2. Don't forget to precede it with set -g @plugin 'tmux-plugins/tpm'.
3. Point the run command to the TPM repository location (by default it points to ~/.tmux/tpm/tpm).
