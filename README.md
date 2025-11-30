# I O T Λ

> **ι • ο • τ • α** — Not one iota more than needed.

A minimal terminal interface (TUI) framework for Emacs with box-drawing characters, dynamic segments, and theme-aware styling.

<img src="https://img.shields.io/badge/Emacs-30.0%2B-blueviolet.svg" alt="Emacs 30.0+">
<img src="https://img.shields.io/badge/License-GPL--3.0-blue.svg" alt="License GPL-3.0">

## Features

### Modal Editing System
- **Native Semantic Modal Editing**: Ergonomic modal editing preserving Emacs semantics
  - Built on modalka for reliable key translation
  - Single-key commands mapped to standard Emacs bindings (n→C-n, w→M-w, etc.)
  - **Leader Key Framework**: Hierarchical command menu via general.el
  - Smart prefix simulation for C-x, C-h with multi-key sequence support
  - Visual feedback with mode-aware cursor shapes and modeline indicator
  - Automatic activation in text/programming buffers
  - Terminal cursor shape management

### Syntax Theme Transparency
- **Syntax Theme Transparency**: Automatic background removal in terminal
  - Preserves IOTA UI element backgrounds while making syntax transparent
  - Smart face classification and selective preservation
  - Terminal capability detection

### Modern Box-Drawing
- Extended Unicode character sets: single, double, rounded, heavy, heavy-rounded, modern-thin, modern-thick
- Automatic style fallback based on font/terminal capabilities
- Monaspace font optimization
- ASCII fallback for maximum compatibility

### Enhanced Widget Library
- **Modern Progress Bars**: Multiple styles (blocks, circles, squares, arrows, diamonds, braille)
- **Status Indicators**: Success, warning, error, info, pending states
- **Badges**: Rounded, styled badges with custom colors
- **Spinners**: Animated spinners (braille, dots, line, arc, box styles)
- **Smart Glyphs**: Automatic glyph selection based on font capabilities

### Modeline
- Box-drawing characters (╭─╮ │ ╰─╯) with multiple styles
- Header-line or mode-line positioning
- Dynamic width calculation
- Active/inactive window distinction
- Modal state indicator with customizable styles
- Keycast display for active windows

### Theme Integration
- Automatic color extraction from any theme
- Light/dark detection with luminance calculation
- WCAG contrast compliance utilities
- Color manipulation (darken, lighten, blend, saturation)

### Segments
- Buffer name, position, major/minor modes
- VCS branch and status (Git)
- Time, date, battery
- Flycheck/Flymake integration
- Custom segment support
- **Responsive fitting**: Segments collapse or hide when window is narrow
  - Segments have optional short-text for compact display
  - Lower priority segments removed first when space is tight

### Animations
- Color transitions (RGB & HSL interpolation)
- Easing functions (linear, quad, cubic, elastic, bounce)
- Pulse effects on state changes
- Configurable duration and intensity

### Configuration Presets
- **Minimal**: ASCII only (maximum compatibility)
- **Standard**: Single-line box with square corners (┌─┐)
- **Modern**: Rounded corners with smooth style (╭─╮)
- **Cyberpunk**: Heavy lines with bold appearance (┏━┓)
- **Custom**: Manual configuration

### Performance Monitoring
- Built-in performance measurement
- Benchmarking tools for modeline and widgets
- Operation timing reports

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

Feature demos:
```elisp
M-x iota-demo                    ; Main demo interface
M-x iota-demo-theme-system       ; Theme system showcase
M-x iota-demo-box-styles         ; Box-drawing styles
M-x iota-demo-all-features       ; Sequential demo
```

Run tests:
```elisp
M-x iota-run-tests               ; Run test suite
```

Configuration:
```elisp
M-x iota-config-choose-preset    ; Interactive preset selection
M-x iota-config-info             ; View current settings
```

## Configuration

### Quick Configuration with Presets

```elisp
;; Choose a preset: minimal, standard, modern, cyberpunk, custom
(setq iota-config-preset 'modern)
M-x iota-config-choose-preset  ; Interactive preset selection
M-x iota-config-info           ; View current configuration
```

### Theme Transparency (Terminal)

```elisp
;; Automatic background removal in terminal
(setq iota-theme-transparent-in-terminal t)

;; Note: Background preservation disabled for IOTA faces
;; Only syntax highlighting faces will have backgrounds removed
;; IOTA UI elements use foreground colors only

;; Check terminal capabilities
M-x iota-theme-transparent-diagnose

;; Note: iota-theme-transparent-mode available but optional
;; Most users won't need explicit transparency mode
```

### Modeline Options

```elisp
(setq iota-modeline-position 'header)              ; 'header, 'mode, or 'both
(setq iota-modeline-box-style 'rounded)            ; See box styles below
(setq iota-modeline-segments-preset 'standard)     ; 'minimal, 'standard, 'full, 'custom
(setq iota-modeline-update-debounce 0.1)           ; Update debounce (seconds)
(setq iota-modeline-show-in-inactive nil)          ; Show in inactive windows

;; Box drawing styles (auto-detected based on capabilities)
;; 'single, 'double, 'rounded, 'heavy, 'heavy-rounded,
;; 'modern-thin, 'modern-thick, 'ascii

;; Or let IOTA choose the best style
(setq iota-box-default-style (iota-box-select-best-style 'heavy-rounded))
```

### Window & Splash Options

```elisp
(setq iota-window-divider-style 'hidden)           ; Window divider style
(setq iota-window-animate-modeline t)              ; Animate modeline transitions
(setq iota-window-transition-duration 0.8)         ; Transition duration (seconds)
(setq iota-splash-project-prefix "C-c p")          ; Project key prefix
(setq iota-splash-show-key-bindings t)             ; Show project.el key bindings
(setq iota-splash-show-hints t)                    ; Show rotating hints
;; Splash screen also displays Emacs init time
```

### Modal Editing Integration

IOTA provides native semantic modal editing built on `modalka`, eliminating modifier chord pain while preserving Emacs semantics:

```elisp
;; Enable IOTA modal editing
(iota-modal-mode 1)
```

**Key Bindings (COMMAND mode):**

| Category | Key | Command | Emacs Equivalent |
|----------|-----|---------|------------------|
| **Navigation** | `n/p` | next/previous line | C-n/C-p |
| | `f/b` | forward/backward char | C-f/C-b |
| | `F/B` | forward/backward word | M-f/M-b |
| | `a/e` | beginning/end of line | C-a/C-e |
| | `v/V` | page down/up | C-v/M-v |
| | `</> ` | beginning/end of buffer | M-</M-> |
| | `[/]` | backward/forward sexp | - |
| | `{/}` | backward/forward paragraph | - |
| | `l` | recenter | C-l |
| **Editing** | `w` | copy (kill-ring-save) | M-w |
| | `y` | paste (yank) | C-y |
| | `W` | cut (kill-region) | C-w |
| | `Y` | yank-pop | M-y |
| | `k` | kill line | C-k |
| | `d` | delete char | C-d |
| | `D` | kill word | M-d |
| | `u/U` | undo/redo | C-//C-? |
| | `t` | transpose chars | C-t |
| | `/` | completion (dabbrev) | M-/ |
| | `SPC` | set mark | C-SPC |
| | `o/O` | open line below/above | - |
| **Search** | `s/r` | isearch forward/backward | C-s/C-r |
| **Prefixes** | `x` | C-x prefix (smart) | C-x |
| | `h` | C-h prefix (help) | C-h |
| | `c` | Leader key menu | - |
| | `g` | keyboard-quit | C-g |
| **Buffer** | `q` | kill current buffer | - |
| **Mode** | `i` | enter INSERT mode | — |
| | `ESC` | enter COMMAND mode | — |

**Smart Prefix Keys:**

The `x` prefix provides convenient access to common C-x commands:
- `x f` → C-x C-f (find-file)
- `x s` → C-x C-s (save-buffer)
- `x w` → C-x C-w (write-file)
- `x e` → C-x C-e (eval-last-sexp)
- `x c` → C-x C-c (quit)
- `x <other>` → C-x <other> (all other C-x bindings including custom ones)

**Leader Key Framework:**

The `c` key activates the leader menu providing organized access to common operations:

```elisp
;; Leader key is automatically enabled with iota-modal-mode
;; Press 'c' in COMMAND mode to see all options
```

| Prefix | Category | Example Commands |
|--------|----------|-----------------|
| `c f` | Files | `c f f` find-file, `c f s` save, `c f r` recent files |
| `c b` | Buffers | `c b b` switch, `c b k` kill, `c b i` ibuffer |
| `c w` | Windows | `c w /` split right, `c w -` split below, `c w d` delete |
| `c p` | Projects | `c p f` find-file, `c p p` switch project |
| `c v` | Magit | `c v v` status, `c v l` log, `c v c` commit |
| `c h` | Help | `c h f` describe-function, `c h k` describe-key |
| `c s` | Search | `c s s` search forward, `c s R` replace |
| `c t` | Toggle | `c t l` line numbers, `c t i` modal indicator style |
| `c i` | IOTA | `c i d` demo, `c i s` setup, `c i c` config |
| `c q` | Quit | `c q q` save & quit, `c q r` restart |
| `c c` | C-c prefix | Access mode-specific commands |

**Modal Indicator Styles:**
```elisp
(setq iota-modal-indicator-style 'both)   ; "● COMMAND" (default)
(setq iota-modal-indicator-style 'glyph)  ; "●" only (for narrow windows)
(setq iota-modal-indicator-style 'label)  ; "COMMAND" only

;; Cycle through styles
M-x iota-modal-cycle-indicator-style
;; Or use leader: c t i
```

**Visual Feedback:**
- COMMAND mode: Box cursor (█), green indicator (● COMMAND)
- INSERT mode: Bar cursor (|), gray indicator (○ INSERT)
- Cursor updates automatically when switching buffers
- Terminal cursor shapes are properly managed

**Global Key Bindings:**

IOTA modal mode also sets up convenient global prefixes:
- `C-c v` → Magit prefix (similar to `C-c p` for projects)
  - `C-c v v` → magit-status
  - `C-c v l` → magit-log
  - `C-c v c` → magit-commit
  - And more (see `iota-leader.el` for full list)

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

### Core Commands

| Command | Description |
|---------|-------------|
| `M-x iota-setup` | Interactive setup wizard |
| `M-x iota-demo` | Feature demo |
| `M-x iota-splash-screen` | Display splash screen |
| `M-x iota-version` | Show version |
| `M-x iota-reload` | Reload package (dev) |

### Modeline Commands

| Command | Description |
|---------|-------------|
| `M-x iota-modeline-mode` | Toggle IOTA modeline |
| `M-x iota-modeline-refresh` | Refresh modeline |
| `M-x iota-modeline-toggle-position` | Toggle header/mode-line |
| `M-x iota-modeline-cycle-preset` | Cycle through presets |
| `M-x iota-modeline-cycle-style` | Cycle through box styles |

### Modal Editing Commands

| Command | Description |
|---------|-------------|
| `M-x iota-modal-mode` | Toggle modal editing system (global) |
| `M-x iota-modal-toggle` | Toggle between COMMAND and INSERT |
| `M-x iota-modal-enter-command-mode` | Enter COMMAND mode |
| `M-x iota-modal-enter-insert-mode` | Enter INSERT mode |
| `M-x iota-modal-cycle-indicator-style` | Cycle indicator styles (both/glyph/label) |
| `M-x iota-modal-toggle-debug` | Toggle keystroke debugging |
| `M-x iota-leader-mode` | Toggle leader key framework |
| `M-x iota-leader-show-menu` | Show leader key menu (requires which-key) |

### Theme Commands

| Command | Description |
|---------|-------------|
| `M-x iota-refresh-faces` | Refresh faces from theme |
| `M-x iota-theme-transparent-diagnose` | Diagnose terminal transparency support |

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

### Modal Editing
- **`iota-modal.el`** — Native semantic modal editing (modalka-based)
- **`iota-leader.el`** — Hierarchical leader key framework (general.el-based)

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

- **Emacs**: 30.0+
- **Themes**: All themes (automatic adaptation)
- **Display**: Terminal and GUI Emacs
- **Platforms**: macOS, Linux, Windows
- **Modal Editing**: Native modalka-based system with leader key support
- **Dependencies**: modalka (0.1.5+), general.el (0.1+) for modal editing
- **Integrations**: Flycheck, Flymake, VC, Magit, all-the-icons, project.el, which-key

## Development

### Project Structure

```
iota/
├── iota.el              # Main entry point and initialization
├── iota-animate.el      # Animation framework and color interpolation
├── iota-box.el          # Box drawing engine and style definitions
├── iota-demo.el         # Feature demonstrations
├── iota-faces.el        # Face definitions
├── iota-leader.el       # Leader key framework (general.el-based)
├── iota-logos.el        # Branding and logos
├── iota-modal.el        # Native semantic modal editing (modalka-based)
├── iota-modeline.el     # Modeline implementation and overlay management
├── iota-segment.el      # Segment protocol and caching system
├── iota-segments.el     # Built-in segments library
├── iota-splash.el       # Splash screen implementation
├── iota-theme.el        # Theme introspection and color utilities
├── iota-tui.el          # Core TUI library and component rendering
├── iota-widgets.el      # Widget library (progress bars, tables, etc.)
└── iota-window.el       # Window management and transitions
```

## Philosophy

- Minimalism: every feature justified
- Terminal-inspired aesthetics
- Theme-agnostic design
- Sensible defaults
- Performance-focused

---

_I O T Λ — Minimal Terminal Interface for Emacs_
