# Nova: Emacs SVG Child Frames

This is very preliminary work, nova may not work for you.
You might need to tweek the code to make it work on your screen
and font configuration.

![nova-vertico in action](images/nova-vertico-example.png "nova-vertico in action")

![nova-corfu in action](images/nova-corfu-example.png "nova-corfu in action")

![nova-eldoc in action](images/nova-eldoc-example.png "nova-eldoc in action")

## Overview

Nova provides a visually enhanced way to display
child frames in Emacs by leveraging an SVG-based posframe
wrapped around [posframe](https://github.com/tumashu/posframe) (or a regular child frame).
Instead of modifying an existing child frame, this
package creates a secondary frame that draws a
customizable SVG background—complete with rounded
corners, shadows, or potentially any other decorative elements,
before placing the actual frame content on top of it.
The result is a seamless, modern-looking child frame that
integrates smoothly with your Emacs setup.

You can customize the SVG to your liking,
tailoring the shape, size, and aesthetics of your child frames without
changing how you normally interact with them.
This makes it a straightforward way to beautify tooltips,
pop-up windows, or any other child frames without
sacrificing ease of use.

Doing it this way of course means the nova frame doesn't
really have a rounded corner, but in vast majority of cases
it isn't noticeable (as it is configured to have the same
background as the main frame). To make it truly rounded, Emacs'
c code will have to be modified, maybe in the future.

## Installation

### Using Straight/Elpaca

As Nova is not on Elpa/Melpa (yet), the easiest way to install Nova
is by using straight/elpaca:

Straight:

```elisp

(straight-use-package
   '(nova :type git :host github :repo "thisisran/nova"))

```

Elpaca (with use-package integration):

```elisp

(use-package nova
  :ensure (:host github :repo "thisisran/nova"))

```

### Manual install

To install Nova manually, clone [[https://github.com/thisisran/nova-emacs][this]] repo and then add the necessary path:

```elisp

(add-to-list 'load-path <path_to_nova_dir>)

```

For the Nova library only:

```elisp

(require 'nova)

```

For nova-vertico (showing vertico in a floating nova frame):

```elisp

(require 'nova-vertico)

;; Then load using (nova-vertico 1)

```

For nova-corfu/nova-corfu-popupup-info (showing corfu and corfu-popupinfo in a nova frame):

```elisp

(require 'nova-corfu)
(require 'nova-corfu-popupinfo)

;; Then load using:
;; (nova-corfu 1)
;; (nova-corfu-popupinfo 1)

```

For nova-eldoc (showing eldoc-box-hover in a nova frame):

```elisp

(require 'nova-eldoc)

;; Then load using (nova-eldoc 1)

```

## Usage (users)

Once the package is loaded correctly (whether by using straight/elpaca,
or the manual install), you can use the following to enable nova with
vertico, corfu, or eldoc (more modes to come):

### nova-vertico

nova-vertico is similar to [[https://github.com/tumashu/vertico-posframe][vertico-posframe]] (make sure to have it installed),
and relies on it, but also wraps it in a (side-left) nova frame:

```elisp

(nova-vertico-mode 1)

```

### nova-corfu/nova-corfu-popupinfo

nova-corfu (nova-corfu-popupinfo) shows corfu in-buffer completions
(or corfu-popupinfo) in a (side-left) nova frame:

```elisp

(nova-corfu 1)
(nova-corfu-popupinfo 1)

```

### nova-eldoc

nova-eldoc shows eldoc-box-hover (make sure to have eldoc installed),
in a (top-center) nova frame:

```elisp

(nova-eldoc 1)

```

## Developers

 To develop a new nova style, you will need to implement 2 functions:

### nova--render--<name_of_style>

nova--render functions' job is to draw the actual SVG,
representing the frame's aesthetics.

Please look at nova--render-side-left or nova--render-top-center
(in nova-side-left.el or nova-top-center.el, accordingly) for
examples on how to accomplish that.

**Note**: make sure the coordinates you use to calculate where to
place shapes/etc. are screen agnostic, so that the frame will
be shown correctly on any screen's resolution.

nova--render functions must accept 2 arguments:

- name

  `name' is the name given to the nova frame itself, so that
  you can get the local values of the nova library's variables,
  like nova-title-color, nova-radius-x, etc. using the library's
  internal function called nova--get-local.
  
- svg-object

  `svg-object' is the svg object on which you do the actual
  SVG drawings on to (it is passed to nova--render by the
  main nova library functions.


### nova--show--<name_of_style>

nova--show functions' job is to set the posframe representing
the nova frame in correct coordinates/size.

For example, the side-left style adds a bar to the left of the
frame to show the title on the left side, and so requires a
different configuration than the top-center style, which shows
the title at the top center, without requiring any extra width
to the frame (but does require an extra height to show the title).

Please take a look at nova--show-side-left or nova--show-top-center
(in nova-side-left.el or nova-top-center.el, accordingly) for
examples on how to accomplish that.

## Known issues

- Under Mac Sequoia, with the window placements enabled, sometimes
  just moving the window causes the nova frame to stay in-spot, while
  the underlying frame moves with the window (as it should). Maximizing
  the window seems to work
