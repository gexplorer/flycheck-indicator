# flycheck-indicator

[![MELPA](https://melpa.org/packages/flycheck-indicator-badge.svg)](https://melpa.org/#/flycheck-indicator)

An Emacs minor-mode to get a fancy mode line indicator
for [Flycheck](https://github.com/flycheck/flycheck).

# Configuration
Put this code in your init file:

    (require 'flycheck-indicator)

    (eval-after-load "flycheck"
      '(add-hook 'flycheck-mode-hook 'flycheck-indicator-mode))

# Customization

The icons and faces can be customized in the customization group `flycheck-indicator`:

    M-x customize-group
    flycheck-indicator
    
# Examples

Here are some screenshots of the indicator in all the possible statuses
with [Solarized theme](https://ethanschoonover.com/solarized/).

![fancy indicator with solarized theme](doc/screenshots.png)
