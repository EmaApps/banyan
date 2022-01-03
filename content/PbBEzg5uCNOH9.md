---
date: 2022-01-03T16:08:20.719Z
parent: Snlr2yxMG61oz
---

node's `concurrently` is a decent package that mimick's `Procfile`.

```json
  "scripts": {
    "dev": "concurrently npm:banyan:*",
    "banyan:tailwind": "cd tailwind && make",
    "banyan:haskell": "bin/run-haskell"
  }
```

I use this to spawn both `ghcid` and `tailwindcss` watcher in the same process, with output interleaved. Though `concurrently` seems to have trouble with actually shutting down ghcid. I have to manually kill a stale ghcid spawned process to make the port available.