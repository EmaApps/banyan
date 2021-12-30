---
date: 2021-12-30T21:22:27
---

VSCode supports URL handler to open files, eg: `vscode://file<path>` - clicking which will open that file in VSCode. This is enough to add 'edit this node' links in Banyon.

Shortcomings:
- It doesn't automatically open the parent project folder
- Have to use Brave, rather than Safari, to prevent future "Open?" prompts for `vscode:`
  - On the plus side, this would allow Banyon to be made a 'chrome app' 
