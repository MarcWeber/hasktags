# hasktags
A tool to generate tag files for vim and emacs

What is it for? A tag file is a index containing key items of your project such as
- function names
- module names
- data declarations
- ...

So that you can find / jump to them fast.

## HOWTO (GENERATING TAG FILES):
Build hasktags (standard cabal build)

Ctag format:
```bash
hasktags --ignore-close-implementation --ctags .
```

Etag format (used by emacs):
```bash
hasktags --ignore-close-implementation --etags .
```

## HOWTO (USING TAG FILES):
### ViM
```viml
let tags+=tagfile " tags,TAGS is the default setting so probably you don't have to do anything
```
`:tjump foo<tab>` or such. See `:h` tags

### NEdit
Load the "tags" file using File/Load Tags File.
Use "Ctrl-D" to search for a tag.

### XEmacs/Emacs
Load the "TAGS" file using "visit-tags-table"
Use "M-." to search for a tag.

### jedit
There is a plugin.

### Tests
To run the tests, do `cd testcases; sh test.sh`.

## History
In the past this tool was distributed with ghc. I forked and added some
features.  hasktags itself was moved out of the ghc repository. Then I only
verified that my fork finds at least as much tags as the one forked by Igloo.

## Future
Things which could be done in the future:
- make json support optional
- Marco Túlio Pimenta Gontijo proposed replacing json by aeson because it might
  be faster

## Maintainers
See cabal file

## Comments about literate haskell ([lhs][]):
Alex no longer supports bird style ">", so should we drop support, too?

## Contributors
- Tsuru Capital (github/liyang)
- Marco Túlio Pimenta Gontijo (github/marcotmarcot)

## TODO
Add all people having contributed before Oct 2012
This includes people contributing to the darcs repository as well as people
having contributed when this repository has been part of ghc

# Related work
List taken from announce of lushtags.
- https://github.com/bitc/lushtags
- http://hackage.haskell.org/package/hasktags
- http://kingfisher.nfshost.com/sw/gasbag/
- http://hackage.haskell.org/package/hothasktags
- http://majutsushi.github.com/tagbar/

And probably much more

[lhs]: http://www.haskell.org/haskellwiki/Literate_programming
