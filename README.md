# I O T Λ

> **ι • ο • τ • α** — Not one iota more than needed.

A minimal terminal interface (TUI) framework for Emacs with box-drawing characters, dynamic segments, and theme-aware styling.

<img src="https://img.shields.io/badge/Emacs-30.0%2B-blueviolet.svg" alt="Emacs 30.0+">
<img src="https://img.shields.io/badge/License-GPL--3.0-blue.svg" alt="License GPL-3.0">

## Features

### Modeline
- Box-drawing characters (╭─╮ │ ╰─╯) with multiple styles: single, double, rounded, heavy, ASCII
- Header-line or mode-line positioning
- Dynamic width calculation

### Theme Integration
- Automatic color extraction from any theme
- Light/dark detection with luminance calculation
- WCAG contrast compliance utilities

### Segments
- Buffer name, position, major/minor modes
- VCS branch and status (Git)
- Time, date, battery
- Flycheck/Flymake integration
- Custom segment support

### Animations
- Color transitions (RGB & HSL interpolation)
- Easing functions (linear, quad, cubic, elastic, bounce)
- Pulse effects

### Widgets
- Progress bars, tables, dialogs, forms
- Status banners, sparklines, spinners, badges

## Installation

### Using straight.el and use-package

```elisp
(use-package iota
  :straight (:host github :repo "jwintz/iota")
  :custom
  (iota-window-divider-style 'hidden)
  (iota-window-animate-modeline t)
  (iota-window-transition-duration 0.8)
  (iota-splash-project-prefix "C-c p")
  (iota-splash-show-key-bindings t)
  (iota-splash-show-hints t)
  :config
  (iota-modeline-mode 1)
  (iota-window-mode 1)
  :hook
  (emacs-startup-hook . iota-splash-screen))
```

### Manual Installation

```elisp
;; Clone the repository
git clone https://github.com/jwintz/iota ~/Development/iota

;; Add to your init.el
(use-package iota
  :load-path "~/Development/iota"
  :custom
  (iota-window-divider-style 'hidden)
  (iota-window-animate-modeline t)
  (iota-window-transition-duration 0.8)
  (iota-splash-project-prefix "C-c p")
  (iota-splash-show-key-bindings t)
  (iota-splash-show-hints t)
  :config
  (iota-modeline-mode 1)
  (iota-window-mode 1)
  :hook
  (emacs-startup-hook . iota-splash-screen))
```

## Quick Start

Enable IOTA modeline:
```elisp
(iota-modeline-mode 1)
```

Interactive setup wizard:
```elisp
M-x iota-setup
```

Feature demo:
```elisp
M-x iota-demo
```

## Configuration

### Modeline Options

```elisp
(setq iota-modeline-position 'header)              ; 'header, 'mode, or 'both
(setq iota-modeline-box-style 'rounded)            ; 'single, 'double, 'rounded, 'heavy, 'ascii
(setq iota-modeline-segments-preset 'standard)     ; 'minimal, 'standard, 'full, 'custom
(setq iota-modeline-update-debounce 0.1)           ; Update debounce (seconds)
(setq iota-modeline-show-in-inactive nil)          ; Show in inactive windows
```

### Window & Splash Options

```elisp
(setq iota-window-divider-style 'hidden)           ; Window divider style
(setq iota-window-animate-modeline t)              ; Animate modeline transitions
(setq iota-window-transition-duration 0.8)         ; Transition duration (seconds)
(setq iota-splash-project-prefix "C-c p")          ; Project key prefix
(setq iota-splash-show-key-bindings t)             ; Show project.el key bindings
(setq iota-splash-show-hints t)                    ; Show rotating hints
```

### Terminal Emacs Configuration

For optimal IOTA experience in terminal Emacs, consider these settings:

```elisp
;; UI cleanup
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Cursor and startup
(setq visible-cursor nil)
(setq inhibit-startup-screen t)

;; Clipboard integration (terminal)
(use-package clipetty
  :straight t
  :hook (after-init . global-clipetty-mode))

(use-package xclip
  :straight t
  :init (xclip-mode 1))

;; Mouse support in terminal
(add-hook 'tty-setup-hook #'xterm-mouse-mode)

;; Window navigation with arrow keys
(use-package windmove
  :ensure nil
  :bind*
  (("C-x <left>" . windmove-left)
   ("C-x <right>" . windmove-right)
   ("C-x <up>" . windmove-up)
   ("C-x <down>" . windmove-down)))
```

### Custom Segments

```elisp
;; Create a custom segment
(defun my-custom-segment ()
  (iota-segment-create
   :id 'my-segment
   :text (lambda () (format "Custom: %s" (current-time-string)))
   :face 'iota-accent-face
   :align 'left
   :priority 60))

;; Use custom segments
(setq iota-modeline-segments-preset 'custom)
(setq iota-modeline-custom-segments
      (list (iota-segment-buffer-name)
            (my-custom-segment)
            (iota-segment-position)))
```

### Box Styles

| Style | Preview | Description |
|-------|---------|-------------|
| `single` | `┌─┐ │ └─┘` | Single-line borders |
| `double` | `╔═╗ ║ ╚═╝` | Double-line borders |
| `rounded` | `╭─╮ │ ╰─╯` | Rounded corners (default) |
| `heavy` | `┏━┓ ┃ ┗━┛` | Heavy/bold lines |
| `ascii` | `+-+ | +-+` | ASCII fallback |

## Usage Examples

### TUI Components

```elisp
;; Render a box
(iota-render-box
  :content "Hello IOTA"
  :style 'rounded
  :align 'center)

;; Progress bar
(iota-widget-progress-bar 75 100
  :width 40
  :style 'blocks
  :label "Progress")
;; => "Progress ████████████████████████████████░░░░░░░░ 75%"

;; Table
(iota-widget-table
  :headers '("Name" "Status" "Time")
  :rows '(("Build" "+" "2.3s")
          ("Test" "*" "1.1s")
          ("Deploy" "o" "0.0s"))
  :border 'rounded)
```

### Animations

```elisp
;; Pulse animation
(iota-animate-pulse 'mode-line
  :intensity 0.3
  :duration 0.5)

;; Color transition
(iota-animate-face 'mode-line :background
  "#000000" "#39bae6"
  :duration 1.0
  :easing #'iota-animate-ease-in-out-cubic)

;; Value animation
(iota-animate-value 0 100
  (lambda (val) (message "Progress: %.0f%%" val))
  :duration 2.0)
```

### Theme Integration

```elisp
;; Get theme colors
(iota-theme-get-accent-color)     ; => "#39bae6"
(iota-theme-get-background 'default) ; => "#1c1c1c"
(iota-theme-dark-p)               ; => t

;; Color manipulation
(iota-theme-color-lighten "#39bae6" 0.2)
(iota-theme-color-darken "#39bae6" 0.2)
(iota-theme-color-blend "#fff" "#000" 0.5)

;; Ensure contrast
(iota-theme-ensure-contrast "#888" "#000" 4.5)
```

## Interactive Commands

| Command | Description |
|---------|-------------|
| `M-x iota-setup` | Interactive setup wizard |
| `M-x iota-demo` | Feature demo |
| `M-x iota-splash-screen` | Display splash screen |
| `M-x iota-version` | Show version |
| `M-x iota-reload` | Reload package (dev) |
| `M-x iota-modeline-mode` | Toggle IOTA modeline |
| `M-x iota-modeline-refresh` | Refresh modeline |
| `M-x iota-modeline-toggle-position` | Toggle header/mode-line |
| `M-x iota-modeline-cycle-preset` | Cycle through presets |
| `M-x iota-modeline-cycle-style` | Cycle through box styles |
| `M-x iota-diagnose` | Show diagnostics info |
| `M-x iota-refresh-faces` | Refresh faces from theme |
| `M-x iota-theme-info` | Show theme details |
| `M-x iota-theme-test-contrast` | Test theme contrast |

## API Reference

### Core System
- **`iota.el`** — Main entry point, setup wizard, and initialization
- **`iota-tui.el`** — High-level TUI library and component rendering
- **`iota-theme.el`** — Theme introspection, color extraction, and semantic faces
- **`iota-faces.el`** — Face definitions and theme integration

### Drawing & Layout
- **`iota-box.el`** — Low-level box drawing engine and style definitions
- **`iota-segment.el`** — Segment protocol, caching, and layout algorithms

### Modeline & Display
- **`iota-modeline.el`** — Modeline implementation and overlay management
- **`iota-segments.el`** — Library of built-in segments (buffer, git, time, etc.)
- **`iota-window.el`** — Window management and transitions
- **`iota-splash.el`** — Splash screen implementation

### Features
- **`iota-widgets.el`** — UI components (progress bars, tables, dialogs)
- **`iota-animate.el`** — Animation framework and color interpolation
- **`iota-demo.el`** — Feature demonstrations
- **`iota-logos.el`** — Branding and logos

## Performance

- <0.1ms per modeline update
- <10% CPU during animations
- Aggressive caching for expensive operations
- Debounced updates
- Disable animations: `(setq iota-animate-enabled nil)`

## Compatibility

- Emacs: 30.0+
- Themes: All themes (automatic adaptation)
- Terminal and GUI Emacs
- Platforms: macOS, Linux, Windows
- Integrations: Flycheck, Flymake, VC, all-the-icons

## Development

### Project Structure

```
iota/
├── iota.el              # Main entry point
├── iota-animate.el      # Animation framework
├── iota-box.el          # Box drawing
├── iota-demo.el         # Feature demonstrations
├── iota-faces.el        # Face definitions
├── iota-logos.el        # Branding and logos
├── iota-modeline.el     # Modeline implementation
├── iota-segment.el      # Segment system
├── iota-segments.el     # Built-in segments
├── iota-splash.el       # Splash screen
├── iota-theme.el        # Theme utilities
├── iota-tui.el          # Core TUI library
├── iota-widgets.el      # Widget library
└── iota-window.el       # Window management
```

## Philosophy

- Minimalism: every feature justified
- Terminal-inspired aesthetics
- Theme-agnostic design
- Sensible defaults
- Performance-focused

---

_I O T Λ — Minimal Terminal Interface for Emacs_
