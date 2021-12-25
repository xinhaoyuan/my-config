# Clientextual (Contextual Client) - an AwesomeWM module that provides per-tag/layout client order and properties.

## Usage

To use the module, simply load it in the AwesomeWM config file. This assumes that you've checked out the code into the `clientextual` directory in the AwesomeWM config directory.

```lua
local clientextual = require("clientextual")
```

## Configuration

After loading the module, there are two flags available in the returned table:

  * `clientextual.ignore_tag`: boolean. When set, shares client states across tags.
  * `clientextual.ignore_layout`: boolean. When set, shares client states across layouts.

When both flags are set, Clientextual is effectively disabled.
