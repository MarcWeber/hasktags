# Tags generator using proper GHC API

# Related work
- [ghc-tags](https://hackage.haskell.org/package/ghc-tags) 
  Uses proper GHC API thus not missing some tags
- https://github.com/bitc/lushtags
- https://github.com/elaforge/fast-tags
- http://kingfisher.nfshost.com/sw/gasbag/
- http://hackage.haskell.org/package/hothasktags
- http://majutsushi.github.com/tagbar/

# hasktags
A tool to generate tag files for vim and emacs

What is it for? A tag file is a index containing key items of your project such as
- function names
- module names
- data declarations
- ...

Old but eventually useful tool using its own fuzzy parsing which has advantages
and disadvantages. See [ghc-tags](https://hackage.haskell.org/package/ghc-tags)
to learn about alternatives.

## Install
1. clone this repo
2. `cd` into the repo directory
3. run `cabal install`. You should get output similar to `Symlinking 'hasktags'
   to '/Users/user/.cabal/bin/hasktags'`
4. source your shell profile. Alternatively you can open a new terminal window

## HOWTO (GENERATING TAG FILES):
Build hasktags (standard cabal build)

Ctag format:
```bash
hasktags --ctags .
```

Etag format (used by emacs):
```bash
hasktags --etags .
```

Both formats:
```bash
hasktags --both .
```

*NB:* Generating both tags generates a file called `TAGS` for Emacs, and one called `ctags` for Vim.

## HOWTO (USING TAG FILES):
### ViM
```viml
let tags+=tagfile " tags,TAGS is the default setting so probably you don't have to do anything
```
`:tjump foo<tab>` or such. See `:h` tags

You can use a configuration like [this one](../assets/hasktags.vim)
with [Tagbar](https://github.com/majutsushi/tagbar) to produce a
tagbar like this:

![Tagbar1](../assets/tagbar1.png?raw=true) ![Tagbar2](../assets/tagbar2.png?raw=true)

Enormous thanks to Alexey Radkov for the hierarchical design necessary for this usage.

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
- Jack Henahan (maintainer)
- Marc Weber
- Marco Túlio Pimenta Gontijo
- Nikolay Yakimov
- Alois Cochard
- Liyang HU
- Ben Gamari
- Chris Done
- Chris Stryczynski
- Max Nordlund gmail
- Kwang Yul Seo
- Pedro Rodriguez
- Thomas Miedema
- Vincent B
- dnhgff
- Alexey Radkov
- Michael Baikov
- Magnus Therning
- Felix Gruber

## TODO
Add all people having contributed before Oct 2012
This includes people contributing to the darcs repository as well as people
having contributed when this repository has been part of ghc


And probably much more

[lhs]: http://www.haskell.org/haskellwiki/Literate_programming
