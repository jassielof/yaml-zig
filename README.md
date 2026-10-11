# YAML library for Zig

This library provides de/serialization for YAML.

## Usage and examples

Usage examples live as doctests next to the public API in [`lib/yaml/root.zig`](lib/yaml/root.zig). Generated API documentation is linked from the repository description.

### As library

Import it however you want, the only module exposed by the library is `yaml`.

## Testing

Tested against the official YAML test suite tag `data-2022-01-17`.

### Coverage of *YAML Test Suite*

Check GitHub Actions for tests summaries.

## Benchmarking

Requires Zig **0.17.0**. Benchmarks use Sierra (`dependencies/sierra`) and keep **libfyaml** as the permanent comparison rival.

```sh
git submodule update --init --recursive
zig build bench
zig build bench -- --filter=tiny --time=500ms --seed=1
zig build bench -- path/to/file.yaml
```

Output is a compact paired table (yaml then fy per workload). Progress is on stderr; in Actions the same table is written to `GITHUB_STEP_SUMMARY`. fy allocation columns are `n/a` (C malloc), not zero. See `dependencies/sierra/docs/sierra.md`.
