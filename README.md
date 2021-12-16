# hs-cmake
_CMake script-mode interpreter_

**Builds with Haskell-Stack:**
```sh
stack build
```

**Runs similarly:**
```sh
stack run -- path/to/script.cmake
```

### Supported features

- Control flow: **DONE**
- Variable references: **DONE*** (CMP0054 `OLD` behavior)
- Functions: **DONE*** (not macros)
- `set`: **DONE**
- `unset`: **DONE**
- `math`: **DONE**
- `string`: partial
- `file`: partial

### Non goals
- Policies
- Cache
