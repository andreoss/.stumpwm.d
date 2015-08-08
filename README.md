# Stumpwm Configuration


## Requirements

```shell
sbcl --non-interactive --eval "(ql:quickload :bt-semaphore)"
sbcl --non-interactive --eval "(ql:quickload :external-program)"
sbcl --non-interactive --eval "(ql:quickload :swank)"
sbcl --non-interactive --eval "(ql:quickload :stumpwm)"
```

## Installation

```shell
    git clone https://github.com/andreoss/.stumpwm.d ~/.stumpwm.d
    echo "exec ~/.stumpwm.d/start.sh" > ~/.xsession
```
