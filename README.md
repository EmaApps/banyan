# banyan

WIP: Tree of time. 

Like [wind of change](https://windofchange.me) (WOC), but using the tree model (and as a static site).

Banyan has no no social element, for which you should still use WOC. Once ready I probably will begin using banyan primarily, and then cross-post its content to WOC for enabling discussions[^woc]

[^woc]: Until WOC gets tree model, exports & optional client-side encryption. Then there would be no reason for Banyan to exist.

Think of it as a more structured Zettelkasten with the notion of 'time streams' for each node, and [other notions yet to be devised](https://banyan.srid.ca/WV0z0FUpk09RL).

## Developing

```
bin/run
```

You should install the VSCode extension at `./vscode-extension` for edit/new links to work.

## Notes

Fun facts:

- banyan is the first project to use Emanote [as a library](https://github.com/srid/banyan/commit/869991888349190855b3c963493f9ff352d250d0), so as to provide an Emanote-like experience without being tied to its specific domain area (Zettelkasten, notebook, wiki).

Design:

- Tree of nodes: a 'node' correponds to a post -- which may be micro blogpost sized or tweet sized -- that is uniquely positioned in its context as identified by its location in the tree. 
- Child nodes indicate posts made *over time* (hence tree-of-time) under its parent. 
- Each node is identified by an unique ID, that appears in the URL. We use [Nano ID](https://github.com/ai/nanoid) in place of UUID. It is short and sufficient.
- VSCode extension handles the "edit" (and "new") links (live-server only)
- File-format 
  - `.md` files for nodes
  - Tree relationships are defined in YAML `parent` key.[^relbrain].

Todo:

Untriaged (as issues):

- [ ] Breadcrumbs
- [ ] Wikilink support (for referencing other nodes.)

Compensating for lack of interactive web client:

- Features to 'map' to Ema-workflow
  - [x] Quick-view of posts
    - Run prod Banyan as daemon locally; and pin 'chrome app'
      - [x] nix-darwin & `launchd`
    - [x] Test it on private banyan
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

