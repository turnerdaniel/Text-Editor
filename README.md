# Text Editor
A simple console-based text editor created in Haskell that was designed to be free from side-effects.

## Getting Started
### Prerequisites

The GHC (Glasgow Haskell Compiler) is needed to run the Text Editor. 

The minimal installer is all that is required. It can be downloaded from the Haskell website [here](https://www.haskell.org/downloads/).

### Setup

1. Open the terminal of your choice.
2. Navigate to the directory containing `editor.hs`
3. Launch the interactive compiler by entering:
   ```bash
   ghci editor
   ```
    or 
    ```bash
    ghci
    :load editor
    ```
When finished, enter `:quit` to exit.

### Usage

Haskell requires that operations are always performed on a variable. Therefore, each function must be provided with a parameter and assigned an output location. Example:

```bash
b = initialise a "The spectacle before us was indeed sublime"
```

The current status of the text can be displayed by entering the variable name. Example:
```bash
b
> Line "The spectacle before us was indeed sublime" "" "" ""
```

Where the output is provided by 4 lists representing:

```bash
"{text before cursor}" "{cursor}" "{text after cursor}" "{clipboard}"
```

Therefore, the text editor could be used like below:

```bash
a = create
> Line "" "" "" ""

b = initialise a "The spectacle before us was indeed sublime."
> Line "The spectacle before us was indeed sublime." "" "" ""

c = selectWordLeft b
> Line "The spectacle before us was indeed" " sublime." "" ""

d = backspace c
> Line "The spectacle before us was indeed" "" "" ""

e = write d " amazing!"
> Line "The spectacle before us was indeed amazing!" "" "" ""

f = home e
> Line "" "" "The spectacle before us was indeed amazing!" ""

g = selectWordRight f
> Line "" "The " "spectacle before us was indeed amazing!" ""

h = cut g
> Line "" "" "spectacle before us was indeed amazing!" "The "

i = write h "This "
> Line "This " "" "spectacle before us was indeed amazing!" "The "

save i "demo.txt"
\demo.txt> "This spectacle before us was indeed amazing!"

j = destroy i
> []
```

Entering `help` will display information on all available commands that can be performed. 

## License

Distributed under the GNU General Public License. Further information available at [LICENSE](LICENSE).

## Author

Daniel Turner - [turnerdaniel](https://github.com/turnerdaniel/)