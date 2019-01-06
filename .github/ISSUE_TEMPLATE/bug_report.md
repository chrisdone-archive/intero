---
name: Bug report
about: Create a report to help us improve

---

## Problem

Description of problem here.

- [ ] I checked the issue tracker for existing issues about this problem.
- [ ] I'm using the latest version of intero.
- [ ] I am not using Nix, which is notorious for breaking stack, intero and lots of other things.
- [ ] I have checked that my error is not GHC's fault by testing my project on standard GHCi.
- [ ] I am using calm, polite language.
- [ ] I am not trying to use intero on bleeding edge GHC (head or nightly); Intero is only developed against full GHC releases.

### Input program/project

Link to sample project here. It helps to paste something on
https://gist.github.com/

* If the problem occurs on any Haskell file, you can just paste a small sample Haskell file.
* If the problem occurs on a specific type of project, plase consider pasting all your stack.yaml, proj.cabal and any relevant .hs files into the Gist too.

That way, a maintainer can easily clone down your test case and reproduce the behavior. Issues that the maintainers can't reproduce **will probably not get fixed**.

If you pressed specific Emacs keypresses, what were they? It also helps to enable debugging in Emacs with `M-x intero-toggle-debug` which will print a log of everything in `*Messages*`, which you can include in the "Actual behavior" section below in a gist.

### Expected behavior

Describe what you expected.

### Actual behavior

What actually happened. Include any https://gist.github.com/ links of output logs. Don't fill the issue description with output. :+1+

### Workarounds

Feel free to include any workarounds that might help other people until the issue is fixed.
