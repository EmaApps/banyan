---
date: 2021-12-27T14:46:16.103Z
parent: iKwIO5DOS3UaB
---

# File-format brainstorming

- How to represent relationships? Candidates:
    1. Filesystem hierarchy
       - Too complex to deal with
    1. YAML metadata `parent` in each ${node}.md
       - Adding new nodes is single-file write operation
       - Organizing nodes en masse involves fiddling with multiple files
    1. Separate file, eg: [DOT](https://www.graphviz.org/doc/info/lang.html), for graphs
        - Automatically allows multi-parent nodes
            - Visually, we present one parent, while demoting others.
        - Organizing nodes *en masse* is easier 
        - Possibility to add relationship [metadata](https://graphviz.org/docs/attrs/xlabel/)

