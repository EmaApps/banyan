# banyan

WIP: Tree of time. 

Like [Wind of change](https://windofchange.me), but using the tree model, and as a static site -- so there is no social aspect, for which you should still use WOC. I probably will begin using banyan primarily, and then cross-post its content to WOC for enabling discussions (until WOC gets tree model as well as exports).

## Developing

```
bin/run
```

## Notes

Fun facts:

- banyan is the first project to use Emanote [as a library](https://github.com/srid/banyan/commit/869991888349190855b3c963493f9ff352d250d0), so as to provide an Emanote-like experience without being tied to its specific domain area (Zettelkasten, notebook, wiki).

Design:

- Tree of nodes: a 'node' correponds to a post -- which may be micro blogpost sized or tweet sized -- that is uniquely positioned in its context as identified by its location in the tree. 
- Child nodes indicate posts made *over time* (hence tree-of-time) under its parent. 
- Each node is identified by an unique ID, that appears in the URL. We use [Nano ID](https://github.com/ai/nanoid) in place of UUID. It is short and sufficient.
- "Next ID" is displayed as CLI to create new files in all pages. We could improve the workflow further here.
- File-format 
  - `.md` files for nodes
  - Tree relationships are defined in YAML `parent` key.[^relbrain].
- VSCode extension handles the "edit" links

Todo:

Right now,

- [x] Dogfooding: start using `./content` for tracking banyan's evolution
  - [x] Store (UTC) `time` in metadata
    - [x] At this point: we need a CLI tool to 'add' content? Or have UI show unix CLI to create it as one-liner? `echo "$(date..)" >$ID.md; cat >>$ID.md`
- [x] Parse and render Markdown
- [ ] Tree structure...
  - [x] Parse `graph.dot`
  - [x] Sidebar
  - [ ] Breadcrumb instead of title
  - [x] HTML for child listing and count
    - Supress leave nodes from sidebar listing 
- Bugs/issues
  - Nodes not in `graph.dot` must appear in sidebar
  - dfsForest: creates non-parents. use bfsForest?
- [ ] Nice HTML and publish
  - [ ] Docker image
  - [ ] Build `./content` using Docker image
  - [ ] Or: run websocket server in DigitalOcean with syncthing

Compensating for lack of interactive web client:

- Features to 'map' to Ema-workflow
  - [ ] Quick-view of posts
    - Run prod Banyan as daemon locally; and pin 'chrome app'
      - [x] nix-darwin & `launchd`
    - [ ] Test it on private banyan
  - [x] Quick-post or Quick-edit under a card
    - [x] The newfile cli for new/edit can change depending on route
      - [x] Replace graph.dot with YAML parent specification. Makes it easy to create one-run CLI for adding child nodes.
    - [x] "One-click" modifications: Have browser open text editor?
    - Custom URI handler for creating new posts: https://developer.apple.com/documentation/xcode/defining-a-custom-url-scheme-for-your-app
      - [x] Even better, as VSCode extension: https://code.visualstudio.com/api/advanced-topics/remote-extensions#callbacks-and-uri-handlers
      - [x] Include base url in URI
  - [ ] Rich-text copy-paste (eg: AFT to WOC)
    - Use 'copy as markdown' or 'paste as markdown'?
    - https://euangoddard.github.io/clipboard2markdown/
- Add these 'patterns' to Ema guide.

[^relbrain]: File-format brainstorming:
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

