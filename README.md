# I O T Λ

> **ι • ο • τ • α** — Not one iota more than needed.

A minimal terminal interface framework for Emacs with box-drawing modeline, theme-aware styling, and transparent terminal support.

<img src="https://img.shields.io/badge/Emacs-30.0%2B-blueviolet.svg" alt="Emacs 30.0+">
<img src="https://img.shields.io/badge/License-GPL--3.0-blue.svg" alt="License GPL-3.0">

## Features

- **Box-Drawing Modeline**: Header-line modeline with Unicode box-drawing characters
- **Mode Enhancements**: Visual decorations for major modes (markdown code blocks)
- **Theme Transparency**: Automatic background removal for transparent terminals
- **Window Management**: Active/inactive window distinction with dimming
- **Popup Handling**: TUI decorations for which-key, transient, and other popups
- **Splash Screen**: Minimal startup screen with IOTA branding
- **Screen Savers**: Matrix, pipes, clock, life, plasma, stars, earth, lava
- **Copilot Usage**: GitHub Copilot premium request tracking (Pro+ plans)
- **Icon Support**: Nerd-icons with automatic Unicode/ASCII fallback

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
- `iota-modeline-mode` — Box-drawing modeline and separator lines
- `iota-popup-mode` — Popup window decorations (which-key, transient, etc.)
- `iota-window-mode` — Window tracking and divider styling
- `iota-modes-mode` — Mode-specific visual enhancements (markdown, etc.)

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

### Popup Windows

IOTA detects popup windows like which-key and transient, applying appropriate styling:

```elisp
;; Configure popup decoration style
(setq iota-popup-decoration-style 'bottom-line)  ; 'bottom-line or 'none

;; Use inactive face for popup decorations (recommended)
(setq iota-popup-use-inactive-face t)

;; Add custom popup patterns
(add-to-list 'iota-popup-buffer-patterns "\\`\\*my-popup\\*")
```

Supported popup types:
- which-key popup windows
- transient popup windows
- Embark action/collect windows
- Consult popup windows
- Completions buffer

### Inactive Window Dimming

IOTA can dim inactive windows while preserving syntax highlighting:

```elisp
;; Enable dimming
(iota-dimmer-mode 1)

;; Use a preset (see table below)
(setq iota-dimmer-saturation-fraction 0.50
      iota-dimmer-luminance-fraction 0.30)  ; "washed" preset

;; Or use balanced defaults derived from fraction
(setq iota-dimmer-fraction 0.40
      iota-dimmer-saturation-fraction nil   ; uses fraction * 0.6
      iota-dimmer-luminance-fraction nil)   ; uses fraction * 0.5

;; Optional: disable dimming all windows when frame loses focus (default: t)
;; (setq iota-dimmer-watch-frame-focus nil)
```

**Dimming Presets:**

| Preset | Saturation | Luminance | Effect |
|--------|------------|-----------|--------|
| subtle | 0.15 | 0.10 | Barely noticeable |
| balanced | nil | nil | Default, uses fraction |
| desaturated | 0.50 | 0.00 | Gray out, keep brightness |
| fade-only | 0.00 | 0.40 | Keep colors, fade to background |
| washed | 0.60 | 0.30 | Pastel/washed look |
| strong | 0.50 | 0.50 | Very noticeable |
| grayscale | 0.80 | 0.40 | Nearly gray |
| muted | 0.40 | 0.15 | Less vibrant |
| high-contrast | 0.20 | 0.50 | Keep colors, fade brightness |

You can also apply presets interactively with `M-x iota-dimmer-apply-preset`.

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

### Mode Enhancements

IOTA provides visual enhancements for specific major modes:

**Markdown Mode:**

Code blocks are decorated with horizontal line borders showing the language:

```
── elisp ─────────────────────────────────────────
(use-package iota
  :load-path "~/path/to/iota")
──────────────────────────────────────────────────
```

```elisp
;; Enable/disable markdown code block decorations (default: t)
(setq iota-modes-markdown-box-code-blocks t)

;; Toggle mode enhancements
(iota-modes-mode 1)   ; Enable
(iota-modes-mode -1)  ; Disable
```

Features:
- Shows language identifier in the top border
- Uses overlays (does not modify buffer content)
- Does not affect cursor movement or line numbers
- Works with any color theme

### Icons

IOTA provides an icon system with automatic fallback when `nerd-icons` is unavailable:

```elisp
;; Use a predefined icon (falls back to Unicode/ASCII if nerd-icons unavailable)
(iota-icon-get 'modified)           ; ● or * depending on availability
(iota-icon-get 'arrow-up)           ; ↑ or ^
(iota-icon-get 'git-branch)         ; ⎇ or B

;; Icon with text label
(iota-icon-get-with-text 'arrow-up "Taller")  ; "↑ Taller"

;; Direct nerd-icons access with fallback
(iota-icon 'codicon "nf-cod-file" "📄")

;; Get icon for major mode
(iota-icon-for-mode 'emacs-lisp-mode)  ; λ

;; Configure fallback style
(setq iota-icons-fallback-style 'unicode)  ; or 'ascii

;; Check availability
(iota-icons-available-p)  ; t if nerd-icons is working

;; Diagnose icon support
M-x iota-icons-diagnose
```

Available icon families: `codicon`, `devicon`, `faicon`, `flicon`, `mdicon`, `octicon`, `pomicon`, `powerline`, `sucicon`, `wicon`.

## Dispatch Interface

IOTA provides a unified transient popup interface for all features via `M-x iota-dispatch`:

```
ι • ο • τ • α
Not one iota more than needed.

Configuration              Display
c Config                   m Modeline
t Theme                    d Dimmer

Windows                    Assistant
w Window                   g Copilot
p Popup                    
                           Packages
Extras                     l Packages
s Screens                  
? Splash
```

### Binding with use-package and general.el

```elisp
(use-package iota
  :load-path "~/path/to/iota"
  :general
  ("C-c i" 'iota-dispatch)  ; Main dispatch at C-c i
  ;; Optional: direct access to sub-menus
  (:prefix "C-c i"
   "c" 'iota-config-transient
   "s" 'iota-screens-transient
   "m" 'iota-modeline-transient
   "d" 'iota-dimmer-transient
   "t" 'iota-theme-transient
   "p" 'iota-popup-transient
   "w" 'iota-window-transient
   "g" 'iota-copilot-transient
   "l" 'iota-package-transient
   "?" 'iota-splash-transient)
  :custom
  (iota-window-divider-style 'hidden)
  :hook
  (emacs-startup-hook . iota-splash-screen))
```

### Binding with vanilla Emacs

```elisp
(global-set-key (kbd "C-c i") 'iota-dispatch)
```

## Commands

| Command | Description |
|---------|-------------|
| `M-x iota-dispatch` | **Main dispatch interface** |
| `M-x iota-config-transient` | Configuration popup |
| `M-x iota-screens-transient` | Screen savers popup |
| `M-x iota-modeline-transient` | Modeline popup |
| `M-x iota-dimmer-transient` | Dimmer popup |
| `M-x iota-theme-transient` | Theme popup |
| `M-x iota-popup-transient` | Popup windows settings |
| `M-x iota-window-transient` | Window settings popup |
| `M-x iota-splash-transient` | Splash screen popup |
| `M-x iota-package-transient` | Package management popup |
| `M-x iota-setup` | Interactive setup wizard |
| `M-x iota-quickstart` | Enable all features with defaults |
| `M-x iota-version` | Show version |
| `M-x iota-reload` | Reload package (development) |
| `M-x iota-modeline-mode` | Toggle modeline |
| `M-x iota-dimmer-mode` | Toggle inactive window dimming |
| `M-x iota-dimmer-refresh` | Refresh dimming (after theme change) |
| `M-x iota-popup-mode` | Toggle popup decorations |
| `M-x iota-popup-cycle-style` | Cycle popup decoration styles |
| `M-x iota-window-mode` | Toggle window tracking |
| `M-x iota-window-cycle-divider-style` | Cycle between hidden/plain dividers |
| `M-x iota-modes-mode` | Toggle mode enhancements (markdown, etc.) |
| `M-x iota-splash-screen` | Show splash screen |
| `M-x iota-config-info` | Show current configuration |

## Project Structure

```
iota/
├── iota.el                    # Main entry point
├── iota-box.el                # Box-drawing primitives
├── iota-cache.el              # Caching utilities
├── iota-config.el             # Configuration and presets
├── iota-copilot.el            # GitHub Copilot usage tracking
├── iota-dimmer.el             # Inactive window dimming
├── iota-dispatch.el           # Transient dispatch interface
├── iota-faces.el              # Face definitions
├── iota-icons.el              # Icon support with nerd-icons fallback
├── iota-logos.el              # ASCII art logos
├── iota-modes.el              # Mode-specific enhancements
├── iota-modes-info.el         # Info mode text reflow commands
├── iota-modes-markdown.el     # Markdown code block decorations
├── iota-modeline.el           # Modeline and separator lines
├── iota-popup.el              # Popup window decorations
├── iota-screens.el            # Screen saver system
├── iota-screens-*.el          # Screen implementations
├── iota-splash.el             # Splash screen
├── iota-special-buffer.el     # Special buffer handling
├── iota-theme.el              # Theme introspection
├── iota-theme-transparent.el  # Terminal transparency
├── iota-timers.el             # Timer management
├── iota-update.el             # Centralized update system
├── iota-utils.el              # General utilities
└── iota-window.el             # Window management
```

## Requirements

- Emacs 30.0+
- Terminal with Unicode support (for box-drawing characters)

**Optional:**
- `nerd-icons` — For enhanced icons in transients and modeline
- `gh` CLI — For Copilot usage tracking (requires `user` scope)

## Philosophy

- **Minimalism**: Every feature justified
- **Terminal-first**: Designed for terminal Emacs
- **Theme-agnostic**: Works with any theme
- **Performance**: Lightweight with centralized update batching

---

_I O T Λ — Minimal Terminal Interface for Emacs_
