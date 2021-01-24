# Sniem

Sniem is **S**imple u**ni**ted **e**dition **m**ethod. This edition method included some good ideas from `evil` and `meow`.  

## Installation

```emacs-lisp
(use-package sniem
    :init (global-sniem-mode t)
    :config
    (sniem-set-keyboard-layout 'qwerty))
```

## Introduce

### Modes

Sniem includes 3 modes:

- `NORMAL`: This mode is the default mode for the editing buffers.
- `INSERT`: This mode will be used when you executed `sniem-insert` in `NORMAL` mode. It's used to edit. In this mode, there's only one binding for `sniem`, that is `<C-TAB>` which will make you exit `INSERT` mode back to `NORMAL` mode.
- `MOTION`: This mode is used in special modes, there's only has one binding for `sniem`, that is `<SPC>` which can let you use leader keys.

### Keyboard Layout

You can use `(sniem-set-keyboard-layout)` to set your keyboard layout for `sniem`, then `sniem` will bind the default keys for the keyboard layout.  

e.g.:
```emacs-lisp
(sniem-set-keyboard-layout 'colemak)
```

The dvorak layout will be added later.

## LICENSE
GPL-3.0
