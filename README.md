# I O T Λ

> **ι • ο • τ • α** — Not one iota more than needed.

A minimal terminal interface framework for Emacs with box-drawing modeline, theme-aware styling, and transparent terminal support.

<img src="https://img.shields.io/badge/Emacs-30.0%2B-blueviolet.svg" alt="Emacs 30.0+">
<img src="https://img.shields.io/badge/License-GPL--3.0-blue.svg" alt="License GPL-3.0">

## Features

- **Box-Drawing Modeline**: Header-line modeline with Unicode box-drawing characters (╭─╮ │ ╰─╯)
- **Theme Transparency**: Automatic background removal for transparent terminals
- **Window Management**: Active/inactive window distinction with optional animations
- **Footer Line**: Separator above minibuffer for visual clarity
- **Splash Screen**: Minimal startup screen with IOTA branding

## Installation

### Using use-package (recommended)

```elisp
(use-package iota
  :load-path "~/path/to/iota"
  :custom
  (iota-window-divider-style 'hidden)  ; or 'plain for visible dividers
  :hook
  (emacs-startup-hook . iota-splash-screen))
```

### Manual

```elisp
(add-to-list 'load-path "~/path/to/iota")
(require 'iota)
```

IOTA automatically enables its modes when loaded:
- `iota-modeline-mode` — Box-drawing modeline
- `iota-footerline-mode` — Minibuffer separator
- `iota-window-mode` — Window tracking and divider styling

## Configuration

### Window Dividers

```elisp
;; Hide window dividers (transparent)
(setq iota-window-divider-style 'hidden)

;; Show window dividers with box-drawing character
(setq iota-window-divider-style 'plain)
```

### Box Styles

```elisp
;; Available styles: single, double, rounded, heavy, heavy-rounded, ascii
(setq iota-box-default-style 'rounded)
(setq iota-modeline-box-style 'rounded)
```

| Style | Preview | Description |
|-------|---------|-------------|
| `single` | `┌─┐ │ └─┘` | Single-line borders |
| `double` | `╔═╗ ║ ╚═╝` | Double-line borders |
| `rounded` | `╭─╮ │ ╰─╯` | Rounded corners (default) |
| `heavy` | `┏━┓ ┃ ┗━┛` | Heavy/bold lines |
| `ascii` | `+-+ \| +-+` | ASCII fallback |

### Modeline Position

```elisp
;; Position: 'header (default), 'mode, or 'both
(setq iota-modeline-position 'header)
```

### Theme Transparency

For transparent terminals, IOTA automatically removes background colors from faces:

```elisp
;; Enable/disable terminal transparency (enabled by default in terminal)
(setq iota-theme-transparent-in-terminal t)

;; Diagnose terminal capabilities
M-x iota-theme-transparent-diagnose
```

### Footer Line

```elisp
;; Configure footer line triggers
(setq iota-footerline-show-for-minibuffer t)  ; Show when minibuffer active
(setq iota-footerline-show-for-which-key t)   ; Show for which-key popups
(setq iota-footerline-show-for-warnings t)    ; Show for warning messages
```

### Animations

```elisp
;; Enable window transition animations
(setq iota-window-animate-transitions t)
(setq iota-window-transition-duration 0.15)

;; Disable all animations
(setq iota-animate-enabled nil)
```

### Splash Screen

```elisp
;; Show splash screen on startup
(add-hook 'emacs-startup-hook #'iota-splash-screen)

;; Show hints on splash screen
(setq iota-splash-show-hints t)
```

### Line Numbers

IOTA can prevent line numbers from being enabled:

```elisp
;; Prevent line numbers (default: t)
(setq iota-prevent-line-numbers t)
```

## Commands

| Command | Description |
|---------|-------------|
| `M-x iota-setup` | Interactive setup wizard |
| `M-x iota-quickstart` | Enable all features with defaults |
| `M-x iota-version` | Show version |
| `M-x iota-reload` | Reload package (development) |
| `M-x iota-modeline-mode` | Toggle modeline |
| `M-x iota-footerline-mode` | Toggle footer line |
| `M-x iota-window-mode` | Toggle window tracking |
| `M-x iota-window-cycle-divider-style` | Cycle between hidden/plain dividers |
| `M-x iota-splash-screen` | Show splash screen |
| `M-x iota-config-info` | Show current configuration |

## Project Structure

```
iota/
├── iota.el                  # Main entry point
├── iota-animate.el          # Animation framework
├── iota-box.el              # Box-drawing primitives
├── iota-cache.el            # Caching utilities
├── iota-config.el           # Configuration and presets
├── iota-faces.el            # Face definitions
├── iota-footerline.el       # Footer line separator
├── iota-logos.el            # ASCII art logos
├── iota-modeline.el         # Modeline implementation
├── iota-splash.el           # Splash screen
├── iota-theme.el            # Theme introspection
├── iota-theme-transparent.el # Terminal transparency
├── iota-timers.el           # Timer utilities
├── iota-update.el           # Update utilities
├── iota-utils.el            # General utilities
└── iota-window.el           # Window management
```

## Requirements

- Emacs 30.0+
- Terminal with Unicode support (for box-drawing characters)

## Philosophy

- **Minimalism**: Every feature justified
- **Terminal-first**: Designed for terminal Emacs
- **Theme-agnostic**: Works with any theme
- **Performance**: Lightweight with debounced updates

---

_I O T Λ — Minimal Terminal Interface for Emacs_
