# Notes about TS

## Issues

### Identifying leaf nodes

Whether or not the leaf nodes will be marked as named or not depends on how the grammar is written.
In the elm grammar, all tokens have a named rule such as `left_parenthesis : ($) => '('`.
The javascript grammar does not do this.
Currently i can't see how to tell the difference between literals and tokens in elm.
One solution might substitute all of these occurrences, possibly automated by inspecting the grammar.json and eliminating rules with type STRING, or semi automated by vim.
This has the cost of having to maintain separate versions of parsers that has this issue.

