#!/usr/bin/env python3
"""
IOTA Demo Automation using iTerm2's Python API + QuickTime Recording

Runs the IOTA terminal demo scenario (Acts 1-7)

Dependencies:
    - pip3 install iterm2
    - QuickTime Player (built-in)
    - Permissions: 'Screen Recording' and 'Accessibility' for Terminal/iTerm2
"""

import asyncio
import iterm2
import time
import subprocess
import sys

# Configuration
TYPING_DELAY = 0.05  # Speed of typing (slower for readability)
ACTION_DELAY = 3.0   # Pause between distinct actions
LONG_PAUSE = 5.0     # Longer pause for viewer to read/observe
VERY_LONG_PAUSE = 8.0  # Extra long pause for complex visuals

# --- Text Automation ---

async def type_text(session, text, delay=TYPING_DELAY):
    """Type text character by character for natural appearance."""
    for char in text:
        await session.async_send_text(char)
        await asyncio.sleep(delay)


async def type_comment(session, text, delay=TYPING_DELAY, pause=1.0):
    """
    Type a comment for the viewer to read, pause, then delete it.
    The comment is displayed but never executed.
    """
    # Type the comment
    for char in text:
        await session.async_send_text(char)
        await asyncio.sleep(delay)

    # Pause so viewer can read
    await asyncio.sleep(pause)

    # Delete the comment (send backspace for each character)
    for _ in text:
        await session.async_send_text("\x7f")  # DEL/Backspace
        await asyncio.sleep(0.01)

    await asyncio.sleep(0.3)


async def wait_for_text(session, text, timeout=120, poll_interval=1.0):
    """
    Wait for specific text to appear in the terminal output.
    Returns True if found, False if timeout.
    """
    start_time = asyncio.get_event_loop().time()
    while (asyncio.get_event_loop().time() - start_time) < timeout:
        screen_contents = await session.async_get_screen_contents()
        full_text = "\n".join(
            screen_contents.line(i).string
            for i in range(screen_contents.number_of_lines)
        )
        if text in full_text:
            return True
        await asyncio.sleep(poll_interval)
    return False


async def wait_for_emacs_ready(session, timeout=300, poll_interval=3.0):
    """
    Wait for Emacs to be fully loaded by screen scraping.
    Looks for the splash screen tagline and absence of installation activity.
    """
    installing_keywords = ["Cloning", "Queuing", "Linking", "elpaca-process"]
    ready_text = "Not one iota more than needed."
    start_time = asyncio.get_event_loop().time()

    while (asyncio.get_event_loop().time() - start_time) < timeout:
        screen_contents = await session.async_get_screen_contents()
        full_text = "\n".join(
            screen_contents.line(i).string
            for i in range(screen_contents.number_of_lines)
        )

        # Check if still installing
        is_installing = any(kw in full_text for kw in installing_keywords)

        if is_installing:
            print(f"    Still installing packages...")
        elif ready_text in full_text:
            print(f"    Splash screen detected - Emacs is ready!")
            return True
        else:
            print(f"    Waiting for Emacs...")

        await asyncio.sleep(poll_interval)

    return False


# --- Main Demo Sequence ---

async def main(connection):
    app = await iterm2.async_get_app(connection)

    # Reuse the current window
    window = app.current_terminal_window
    if not window:
        print("No active iTerm2 window found. Please open iTerm2 first.")
        return

    print("   ✨ Using current window for demo...")
    tab = window.current_tab
    session = tab.current_session

    current_profile = await session.async_get_profile()
    profile_name = current_profile.name

    # Clean up previous IOTA state before starting
    print("   🧹 Cleaning up previous IOTA state...")
    await session.async_send_text("rm -rf ~/Development/iota.d/iota-*\n")
    await asyncio.sleep(0.5)

    # Ensure clear start
    await session.async_send_text("clear\n")
    await asyncio.sleep(1)

    print("🎬 IOTA Demo Starting...")

    try:
        # ========================================================================
        # ACT 1: The Shell Alias
        # ========================================================================
        print("  Act 1: Shell Alias")

        await type_comment(session, "# Create the IOTA alias", pause=1.5)
        # The alias runs Emacs; we'll signal ready via a separate mechanism
        await type_text(session, "alias iota='emacs -nw --init-directory ~/Development/iota.d'")
        await session.async_send_text("\n")
        await asyncio.sleep(ACTION_DELAY)

        await type_comment(session, "# Launch IOTA (first run bootstraps packages)", pause=2.0)
        await type_text(session, "iota")
        await session.async_send_text("\n")

        # Wait for Emacs to fully load (including package installation if needed)
        # Note: Normal launch is sub-second, but first run needs to install packages
        print("    Waiting for Emacs to load (this may take a while if packages are installing)...")
        found = await wait_for_emacs_ready(session, timeout=180, poll_interval=2.0)
        if not found:
            # Fallback: just wait a bit longer
            print("    Warning: Did not detect IOTA splash, continuing anyway...")
            await asyncio.sleep(LONG_PAUSE * 2)
        else:
            print("    Emacs is ready!")
            await asyncio.sleep(ACTION_DELAY)  # Brief pause to let user see it

        # ========================================================================
        # ACT 2: Splash Screen (observe the animated splash)
        # ========================================================================
        print("  Act 2: Splash Screen")
        await asyncio.sleep(VERY_LONG_PAUSE)  # Let viewer appreciate the animated splash

        # Dismiss splash with 'q'
        await session.async_send_text("q")
        await asyncio.sleep(ACTION_DELAY)

        # ========================================================================
        # ACT 3: Explore IOTA Codebase with Window Splits
        # ========================================================================
        print("  Act 3: Explore IOTA Codebase")

        # Open iota-splash.el using C-x C-f
        await session.async_send_text("\x18")  # C-x
        await asyncio.sleep(0.2)
        await session.async_send_text("\x06")  # C-f
        await asyncio.sleep(ACTION_DELAY)

        await type_text(session, "~/Development/iota/iota-splash.el")
        await asyncio.sleep(ACTION_DELAY)
        await session.async_send_text("\n")
        await asyncio.sleep(LONG_PAUSE)

        # Split window vertically: C-x 3
        await session.async_send_text("\x18")  # C-x
        await asyncio.sleep(0.2)
        await session.async_send_text("3")
        await asyncio.sleep(ACTION_DELAY)

        # Balance windows: C-x +
        await session.async_send_text("\x18")  # C-x
        await asyncio.sleep(0.2)
        await session.async_send_text("+")
        await asyncio.sleep(ACTION_DELAY)

        # Move to other window: C-x o
        await session.async_send_text("\x18")  # C-x
        await asyncio.sleep(0.2)
        await session.async_send_text("o")
        await asyncio.sleep(ACTION_DELAY)

        # Open iota-modeline.el
        await session.async_send_text("\x18")  # C-x
        await asyncio.sleep(0.2)
        await session.async_send_text("\x06")  # C-f
        await asyncio.sleep(ACTION_DELAY)

        await type_text(session, "~/Development/iota/iota-modeline.el")
        await asyncio.sleep(ACTION_DELAY)
        await session.async_send_text("\n")
        await asyncio.sleep(LONG_PAUSE)  # Show side-by-side with dimming

        # ========================================================================
        # ACT 4: Dimmer - Show inactive window dimming
        # ========================================================================
        print("  Act 4: Dimmer Presets")

        # M-x iota-dispatch then d (Dimmer transient)
        await session.async_send_text("\x1b")  # M-x (Escape then x)
        await asyncio.sleep(0.1)
        await session.async_send_text("x")
        await asyncio.sleep(ACTION_DELAY)
        await type_text(session, "iota-dispatch")
        await session.async_send_text("\n")
        await asyncio.sleep(ACTION_DELAY)

        await session.async_send_text("d")  # Dimmer menu
        await asyncio.sleep(LONG_PAUSE)

        # Apply Subtle preset
        await session.async_send_text("1")  # Subtle
        await asyncio.sleep(ACTION_DELAY)

        # Switch to other window to see dimming effect on current window
        await session.async_send_text("\x18")  # C-x
        await asyncio.sleep(0.2)
        await session.async_send_text("o")
        await asyncio.sleep(LONG_PAUSE)  # Observe subtle dimming

        # Switch back
        await session.async_send_text("\x18")  # C-x
        await asyncio.sleep(0.2)
        await session.async_send_text("o")
        await asyncio.sleep(ACTION_DELAY)

        # Apply Strong preset
        await session.async_send_text("\x1b")  # M-x
        await asyncio.sleep(0.1)
        await session.async_send_text("x")
        await asyncio.sleep(ACTION_DELAY)
        await type_text(session, "iota-dispatch")
        await session.async_send_text("\n")
        await asyncio.sleep(ACTION_DELAY)
        await session.async_send_text("d")
        await asyncio.sleep(ACTION_DELAY)

        await session.async_send_text("3")  # Strong
        await asyncio.sleep(ACTION_DELAY)

        # Switch to other window to see strong dimming effect
        await session.async_send_text("\x18")  # C-x
        await asyncio.sleep(0.2)
        await session.async_send_text("o")
        await asyncio.sleep(LONG_PAUSE)  # Observe strong dimming

        # Switch back
        await session.async_send_text("\x18")  # C-x
        await asyncio.sleep(0.2)
        await session.async_send_text("o")
        await asyncio.sleep(ACTION_DELAY)

        # Apply Balanced preset
        await session.async_send_text("\x1b")  # M-x
        await asyncio.sleep(0.1)
        await session.async_send_text("x")
        await asyncio.sleep(ACTION_DELAY)
        await type_text(session, "iota-dispatch")
        await session.async_send_text("\n")
        await asyncio.sleep(ACTION_DELAY)
        await session.async_send_text("d")
        await asyncio.sleep(ACTION_DELAY)

        await session.async_send_text("2")  # Balanced
        await asyncio.sleep(ACTION_DELAY)

        # Switch windows to show balanced dimming
        await session.async_send_text("\x18")  # C-x
        await asyncio.sleep(0.2)
        await session.async_send_text("o")
        await asyncio.sleep(LONG_PAUSE)

        await session.async_send_text("\x18")  # C-x
        await asyncio.sleep(0.2)
        await session.async_send_text("o")
        await asyncio.sleep(ACTION_DELAY)

        # ========================================================================
        # ACT 5: Modeline Styles
        # ========================================================================
        print("  Act 5: Modeline Styles")

        # M-x iota-dispatch m (Modeline transient)
        await session.async_send_text("\x1b")  # M-x
        await asyncio.sleep(0.1)
        await session.async_send_text("x")
        await asyncio.sleep(ACTION_DELAY)
        await type_text(session, "iota-dispatch")
        await session.async_send_text("\n")
        await asyncio.sleep(ACTION_DELAY)

        await session.async_send_text("m")  # Modeline menu
        await asyncio.sleep(LONG_PAUSE)

        # Cycle box style
        await session.async_send_text("s")  # Cycle style
        await asyncio.sleep(LONG_PAUSE)

        # Reopen modeline transient
        await session.async_send_text("\x1b")  # M-x
        await asyncio.sleep(0.1)
        await session.async_send_text("x")
        await asyncio.sleep(ACTION_DELAY)
        await type_text(session, "iota-dispatch")
        await session.async_send_text("\n")
        await asyncio.sleep(ACTION_DELAY)
        await session.async_send_text("m")
        await asyncio.sleep(ACTION_DELAY)

        await session.async_send_text("s")  # Cycle again
        await asyncio.sleep(LONG_PAUSE)

        # Reopen modeline transient
        await session.async_send_text("\x1b")  # M-x
        await asyncio.sleep(0.1)
        await session.async_send_text("x")
        await asyncio.sleep(ACTION_DELAY)
        await type_text(session, "iota-dispatch")
        await session.async_send_text("\n")
        await asyncio.sleep(ACTION_DELAY)
        await session.async_send_text("m")
        await asyncio.sleep(ACTION_DELAY)

        await session.async_send_text("s")  # And again
        await asyncio.sleep(LONG_PAUSE)

        # Exit modeline menu
        await session.async_send_text("\x07")  # C-g
        await asyncio.sleep(ACTION_DELAY)

        # ========================================================================
        # ACT 6: Screen Savers with Window Splits
        # ========================================================================
        print("  Act 6: Screen Savers")

        # Split window vertically: C-x 3
        await session.async_send_text("\x18")  # C-x
        await asyncio.sleep(0.2)
        await session.async_send_text("3")
        await asyncio.sleep(ACTION_DELAY)

        # Balance windows: C-x +
        await session.async_send_text("\x18")  # C-x
        await asyncio.sleep(0.2)
        await session.async_send_text("+")
        await asyncio.sleep(ACTION_DELAY)

        # Move to other window: C-x o
        await session.async_send_text("\x18")  # C-x
        await asyncio.sleep(0.2)
        await session.async_send_text("o")
        await asyncio.sleep(ACTION_DELAY)

        # Split horizontally: C-x 2
        await session.async_send_text("\x18")  # C-x
        await asyncio.sleep(0.2)
        await session.async_send_text("2")
        await asyncio.sleep(ACTION_DELAY)

        # Balance windows: C-x +
        await session.async_send_text("\x18")  # C-x
        await asyncio.sleep(0.2)
        await session.async_send_text("+")
        await asyncio.sleep(LONG_PAUSE)  # Show the layout

        # Start Matrix in current window
        await session.async_send_text("\x1b")  # M-x
        await asyncio.sleep(0.1)
        await session.async_send_text("x")
        await asyncio.sleep(ACTION_DELAY)
        await type_text(session, "iota-dispatch")
        await session.async_send_text("\n")
        await asyncio.sleep(ACTION_DELAY)
        await session.async_send_text("s")  # Screens menu
        await asyncio.sleep(ACTION_DELAY)
        await session.async_send_text("m")  # Matrix
        await asyncio.sleep(VERY_LONG_PAUSE)  # Let it rain!

        # Quit Matrix
        await session.async_send_text("q")
        await asyncio.sleep(ACTION_DELAY)

        # Move to other window and start Pipes
        await session.async_send_text("\x18")  # C-x
        await asyncio.sleep(0.2)
        await session.async_send_text("o")
        await asyncio.sleep(ACTION_DELAY)

        await session.async_send_text("\x1b")  # M-x
        await asyncio.sleep(0.1)
        await session.async_send_text("x")
        await asyncio.sleep(ACTION_DELAY)
        await type_text(session, "iota-dispatch")
        await session.async_send_text("\n")
        await asyncio.sleep(ACTION_DELAY)
        await session.async_send_text("s")
        await asyncio.sleep(ACTION_DELAY)
        await session.async_send_text("p")  # Pipes
        await asyncio.sleep(VERY_LONG_PAUSE)

        # Quit Pipes
        await session.async_send_text("q")
        await asyncio.sleep(ACTION_DELAY)

        # Delete other windows: C-x 1
        await session.async_send_text("\x18")  # C-x
        await asyncio.sleep(0.2)
        await session.async_send_text("1")
        await asyncio.sleep(ACTION_DELAY)

        # ========================================================================
        # ACT 7: Productivity - Fuzzy Finding & Ripgrep
        # ========================================================================
        print("  Act 7: Productivity")

        # M-x consult-find
        await session.async_send_text("\x1b")  # M-x
        await asyncio.sleep(0.1)
        await session.async_send_text("x")
        await asyncio.sleep(ACTION_DELAY)
        await type_text(session, "consult-find")
        await session.async_send_text("\n")
        await asyncio.sleep(ACTION_DELAY)

        await type_text(session, "dispatch")
        await asyncio.sleep(LONG_PAUSE)

        # Select result
        await session.async_send_text("\n")
        await asyncio.sleep(LONG_PAUSE)

        # M-x consult-ripgrep
        await session.async_send_text("\x1b")  # M-x
        await asyncio.sleep(0.1)
        await session.async_send_text("x")
        await asyncio.sleep(ACTION_DELAY)
        await type_text(session, "consult-ripgrep")
        await session.async_send_text("\n")
        await asyncio.sleep(ACTION_DELAY)

        await type_text(session, "transient-define-prefix")
        await asyncio.sleep(LONG_PAUSE)

        # Cancel
        await session.async_send_text("\x07")  # C-g
        await asyncio.sleep(LONG_PAUSE)  # Wait for minibuffer to fully close

        # ========================================================================
        # ACT 8: Theme Cycling
        # ========================================================================
        print("  Act 8: Theming")

        # M-x iota-dispatch t (Theme transient)
        await session.async_send_text("\x1b")  # M-x
        await asyncio.sleep(0.1)
        await session.async_send_text("x")
        await asyncio.sleep(ACTION_DELAY)
        await type_text(session, "iota-dispatch")
        await session.async_send_text("\n")
        await asyncio.sleep(ACTION_DELAY)

        await session.async_send_text("t")  # Theme menu
        await asyncio.sleep(LONG_PAUSE)

        # Exit theme menu - send multiple C-g to ensure clean state
        await session.async_send_text("\x07")  # C-g
        await asyncio.sleep(1.0)
        await session.async_send_text("\x07")  # C-g again to ensure clean
        await asyncio.sleep(LONG_PAUSE)

        # Switch to scratch buffer first to avoid inserting into source file
        await session.async_send_text("\x18")  # C-x
        await asyncio.sleep(0.2)
        await session.async_send_text("b")     # b for switch-buffer
        await asyncio.sleep(ACTION_DELAY)
        await type_text(session, "*scratch*")
        await session.async_send_text("\n")
        await asyncio.sleep(ACTION_DELAY)

        # Browse dark themes with M-x consult-theme
        await session.async_send_text("\x1b")  # M-x
        await asyncio.sleep(0.1)
        await session.async_send_text("x")
        await asyncio.sleep(ACTION_DELAY)
        await type_text(session, "consult-theme")
        await session.async_send_text("\n")
        await asyncio.sleep(ACTION_DELAY)

        # Filter to show only modus/ef dark themes
        await type_text(session, "modus-vivendi")
        await asyncio.sleep(LONG_PAUSE)

        # Navigate through dark themes
        await session.async_send_text("\x1b[B")  # Down arrow
        await asyncio.sleep(ACTION_DELAY)
        await session.async_send_text("\x1b[B")  # Down arrow
        await asyncio.sleep(ACTION_DELAY)

        # Select theme
        await session.async_send_text("\n")
        await asyncio.sleep(LONG_PAUSE)

        # ========================================================================
        # CLOSING: Back to Splash
        # ========================================================================
        print("  Closing: Return to Splash")

        # M-x iota-dispatch ? s (show splash)
        await session.async_send_text("\x1b")  # M-x
        await asyncio.sleep(0.1)
        await session.async_send_text("x")
        await asyncio.sleep(ACTION_DELAY)
        await type_text(session, "iota-dispatch")
        await session.async_send_text("\n")
        await asyncio.sleep(ACTION_DELAY)

        await session.async_send_text("?")
        await asyncio.sleep(LONG_PAUSE)  # Wait for submenu to render

        await session.async_send_text("s")
        await asyncio.sleep(VERY_LONG_PAUSE)  # Final splash view

        # Exit gracefully: C-x C-c
        await session.async_send_text("\x18")  # C-x
        await asyncio.sleep(0.2)
        await session.async_send_text("\x03")  # C-c
        await asyncio.sleep(ACTION_DELAY)

        # Confirm exit if prompted
        await session.async_send_text("y")
        await asyncio.sleep(1)

        print("✅ Demo Complete!")

    except KeyboardInterrupt:
        print("\n⚠️ Interrupted by user!")
    finally:
        print("Done.")

# Run the script
iterm2.run_until_complete(main)
