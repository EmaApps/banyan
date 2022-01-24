# banyan

WIP: Tree of time. 

---
*Warning*: Banyan is more of an experiment, and is not meant for general use (at least not right now).

---


Like [wind of change](https://windofchange.me) (WOC), but using the tree model (and as a static site).

Banyan has no social element, for which you should still use WOC. Once ready I probably will begin using banyan primarily, and then cross-post[^api] its content to WOC for enabling discussions[^woc]

[^woc]: Until WOC gets tree model, exports & optional client-side encryption. Then there would be no reason for Banyan to exist.
[^api]: Sync if there is API.

Think of it as a more structured Zettelkasten with the notion of 'time streams' for each node, and [other notions yet to be devised][othermod].

## Use it

https://github.com/srid/banyan-template

## Developing

```
bin/run
```

You should install the VSCode extension at `./vscode-extension` for the "edit" and "new" links to work.

## Notes

Fun facts:

- banyan is the first project to use Emanote [as a library](https://github.com/srid/banyan/commit/869991888349190855b3c963493f9ff352d250d0), so as to provide an Emanote-like experience without being tied to its specific domain area (Zettelkasten, notebook, wiki).

Design:

- Tree of nodes: a 'node' correponds to a post -- which may be micro blogpost sized or tweet sized -- that is uniquely positioned in its context as identified by its location in the tree. 
- Child nodes indicate posts made *over time* (hence tree-of-time) under its parent, but [other models][othermod] may be possible.
- Each node is identified by an unique ID, that appears in the URL. We use [Nano ID](https://github.com/ai/nanoid) in place of UUID. It is short and sufficient.
- VSCode extension handles the "edit" (and "new") links (live-server only)
- File-format 
  - `.md` files for nodes
  - Tree relationships are defined in YAML frontmatter's `parent` key.

Todo:

Untriaged (as issues):

- [ ] Breadcrumbs
- [ ] Wikilink support (for referencing other nodes.)
- [ ] [Folding, etc.][othermod]

Compensating for lack of interactive web client:


    
[othermod]: https://banyan.srid.ca/WV0z0FUpk09RL