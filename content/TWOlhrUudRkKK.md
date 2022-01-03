---
date: 2022-01-03T15:44:01.489Z
parent: Snlr2yxMG61oz
---

Keeping generated CSS in source control. 

So banyan user will not have to be running Tailwind watcher. For apps like Emanote we want the user to be able to do this, though. But other Ema apps don't need to. So there are two models here:

1. Dev-time Tailwind watcher
2. Run-time Tailwind watcher

No. 1 is simpler to implement, and should be sufficient for most Ema apps. But Emanote will need No. 2, which also brings in a long-running nodeJS process into user's machine.