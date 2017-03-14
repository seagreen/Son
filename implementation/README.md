# Son reference implementation

## Goals

(in order)

1. Be correct.

2. Be clear.

3. Be fast.

## Notes

This is  a reference implementation, not a production library. Parsing is currently 3x slower and serializing is 6x slower than `aeson`. Additionally, I've only run a few tests so it probably fares even worse for large values.
