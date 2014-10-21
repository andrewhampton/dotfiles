--- === core.keycodes ===
--- Functionality for converting between key-strings and key-codes.

--- core.keycodes.map = {...}
--- A mapping from string representation of a key to its keycode, and vice versa.
--- For example: keycodes[1] == "s", and keycodes["s"] == 1, and so on.
--- This is primarily used by the core.eventtap and core.hotkey extensions.
---
--- Valid strings are any single-character string, or any of the following strings:
---
---     f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15,
---     f16, f17, f18, f19, f20, pad, pad*, pad+, pad/, pad-, pad=,
---     pad0, pad1, pad2, pad3, pad4, pad5, pad6, pad7, pad8, pad9,
---     padclear, padenter, return, tab, space, delete, escape, help,
---     home, pageup, forwarddelete, end, pagedown, left, right, down, up

local keycodes = require "ext.core_keycodes.internal"
keycodes.map = keycodes.cachemap()

--- core.keycodes.inputsourcechanged()
--- Called when your input source (i.e. qwerty, dvorak, colemac) changes.
--- Default implementation does nothing; you may override this to rebind your hotkeys or whatever.
function keycodes.inputsourcechanged()
end

function keycodes._callback()
  keycodes.map = keycodes.cachemap()
  keycodes.inputsourcechanged()
end

return keycodes
