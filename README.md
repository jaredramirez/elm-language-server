# elm-language-server

This project implements the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/) for Elm.

## Platforms

Currently this project only supports MacOS. I'm fairly certain that it works on most linux distros, but I have not tested on it. This is mostly beacuse I'm not an expert on how cross-compiling works with GHC and haven't tackled the problem yet. I'm not sure how it will work on windows at this point.

## Installation

First, download the latest binary from the releases page.

### Neovim

* Make sure the elm-language-server binary is either a) available on your path, or b) have an absolute path to wherever the binary is stored
* Install and setup [neovim](https://neovim.io/)
* Install [neovim language client](https://github.com/autozimu/LanguageClient-neovim)
  <!-- - Install [neovim completeion manager 2](https://github.com/ncm2/ncm2) -->
* Append your config with the following:

  ```
  set hidden

  let g:LanguageClient_serverCommands = {
  \ 'elm': ['elm-language-server-exe'],
  " OR
  \ 'elm': ['/path/to/elm-language-server-exe'],
  \ }
  ```

## Features

This project is still in pretty early development, so the feature set is currently limited.

* As-you-type diagnostics (compiler errors, etc)
* Find and use either global or local (node_modules) elm installations to get diagnostics

## Future/Contributing

This is actively being developed, so there are many things in the pipeline. The first, and most important thing, is making (or borrowing) an error tolerant parser to get an AST. Once we have that working and integrated, the interesting work of this project can begin (auto-completion, varabile rename, goto defintion, etc). I'm heavily in the research aspect of this stage, looking at the 0.19 parser, the elm-format parser, and some other readings on the topic. If you have any expertise in this area, please reach out to me so I can learn eveything I can to make this as good as possible.

If other's are interested in working on this project as well, that would be great! Please reach out to me (email, twitter) before starting anything so we can avoid working on the same thing and properly plan.

Lastly, this a part-time project for me so if you open an issue or reach out it make take me a few days to get back to you.
