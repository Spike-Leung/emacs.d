# My emacs configuration.

Base on [Purcell’s Emacs](https://github.com/purcell/emacs.d).

# Installation
To install, clone this repo to `~/.emacs.d`, i.e. ensure that the `init.el` contained in this repo ends up at `~/.emacs.d/init.el`:

```bash
git clone --recurse-submodules -j8 https://github.com/Spike-Leung/emacs.d.git ~/.emacs.d
```

# Other requirements
  - **gcc**: Org-roam need emacsql which need gcc
  - **beancount**: For beancount to work, you'd better install [beancount](https://github.com/beancount/beancount) and [fava](https://github.com/beancount/fava)
