# Chameleon - an AwesomeWM module that provides per-tag/layout client order and properties.

## Usage

To use the module, simply load it in the AwesomeWM config file. This assumes that you've checked out the code into the `chameleon` directory in the AwesomeWM config directory.

```lua
local chameleon = require("chameleon")
```

## Configuration

After loading the module, there are two flags available in the returned table:

  * `chameleon.ignore_tag`: boolean. When set, shares client states across tags.
  * `chameleon.ignore_layout`: boolean. When set, shares client states across layouts.

When both flags are set, Chameleon is effectively disabled.
