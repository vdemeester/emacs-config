# emacs-constant-theme

[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![MELPA](https://melpa.org/packages/constant-theme-badge.svg)](https://melpa.org/#/constant-theme)
[![MELPA Stable](http://stable.melpa.org/packages/constant-theme-badge.svg)](http://stable.melpa.org/#/constant-theme)

A calm, almost monochrome color theme for Emacs with a dark and light variant.

*NOTE:* I am not an Emacs expert, so if you find that color definitions are missing
for an Emacs feature or package, please don't hesitate to let me know. Pull requests
are always appreciated!

## Installation

Using [MELPA](http://melpa.milkbox.net/#/getting-started):
```
M-x package-install RET constant-theme
```

Without MELPA: Ensure that a directory containing `constant-theme.el` and
`constant-light-theme.el` is in your `load-path`.

Use `M-x customize-themes` to activate the theme, or add the following to your
Emacs configuration file:
```
(load-theme 'constant t)
(load-theme 'constant-light t)
```

## Screenshots

### constant

![Dark](screenshots/constant.png)

### constant-light

![Light](screenshots/constant-light.png)

## Copyright

Copyright (c) 2018 Jannis Pohlmann.

Licensed under the [GNU General Public License, Version 3](LICENSE).
