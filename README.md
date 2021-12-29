# banyan

WIP: Tree of time. 

Like [Wind of change](https://windofchange.me), but using the tree model, and as a static site -- so there is no social aspect, for which you should still use WOC. I probably will begin using banyan primarily, and then cross-post its content to WOC for enabling discussions.

Fun facts:

- banyan is the first project to use Emanote [as a library](https://github.com/srid/banyan/commit/869991888349190855b3c963493f9ff352d250d0), so as to provide an Emanote-like experience without being tied to its specific domain area (Zettelkasten, notebook, wiki).

Design:

- Tree of nodes: a 'node' correponds to a post -- which may be micro blogpost sized or tweet sized -- that is uniquely positioned in its context as identified by its location in the tree. 
- Child nodes indicate posts made *over time* (hence tree-of-time) under its parent. 
- Each node is identified by an unique ID, that appears in the URL. We use [Nano ID](https://github.com/ai/nanoid) in place of UUID. It is short and sufficient.
- "Next ID" is displayed as CLI to create new files in all pages. We could improve the workflow further here.
- File-format 
  - `.md` files for nodes
  - Tree relationships are defined in `graph.dot` file[^relbrain].

Todo:

Right now,

- [x] Dogfooding: start using `./content` for tracking banyan's evolution
  - [x] Store (UTC) `time` in metadata
    - [x] At this point: we need a CLI tool to 'add' content? Or have UI show unix CLI to create it as one-liner? `echo "$(date..)" >$ID.md; cat >>$ID.md`
- [x] Parse and render Markdown
- [ ] Tree structure...
  - [x] Parse `graph.dot`
  - [x] HTML for tree breadcrumbs 
  - [ ] HTML for child listing and count
    - Supress leave nodes from sidebar listing 
- [ ] Nice HTML and publish
  - [ ] Docker image
  - [ ] Build `./content` using Docker image

## Developing

```
bin/run
```

[^relbrain]: File-format brainstorming:
    - How to represent relationships? Candidates:
      1. Filesystem hierarchy
        - Too complex to deal with
      1. YAML metadata `parent` in each ${node}.md
        - Organizing nodes en masse involves fiddling with multiple files
      1. Separate file, eg: [DOT](https://www.graphviz.org/doc/info/lang.html), for graphs
        - Automatically allows multi-parent nodes
          - Visually, we present one parent, while demoting others.
        - Organizing nodes en masse is easier 
        - Possibility to add relationship [metadata](https://graphviz.org/docs/attrs/xlabel/)

