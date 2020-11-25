# Learn Haskell 

I am playing around with Haskell on holiday break, out of curiosity about working with a functional language.

I'm working off of a Windows 10 machine, using [Haskell 2010](https://www.haskell.org/onlinereport/haskell2010/), and [GHC](https://downloads.haskell.org/~ghc/8.2.2/docs/html/users_guide/intro.html) 8.10.2. 

This README documents my learning experiences for future reference.

## Haskell set up

There are several ways to get started, but the least complicated is installing the [Haskell platform](https://www.haskell.org/downloads/#platform) from the Haskell.org website. You'll need to install the [Chocolatey](https://chocolatey.org/install) package manager first, though.

## Writing your first programs

### Hello world

This was my first attempt at a program. I opened up the [Haskell wiki](https://wiki.haskell.org) and looked up how to print a string.

1. Create a new file with an *.hs* extension.
2. In the new file, type and save:

    ```Haskell
    main :: IO ()

    main = do
        putStrLn "Hello, world!"
        putStrLn "I just wasted an hour looking up how to change a file encoding on Windows."
        putStrLn "A bunch of Linux dunces on StackOverflow didn't know about the `Set-Content -Encoding` command and sent me on a wild goose chase."
   ```

3. Back in your CLI, type `ghc <filename.hs>`. This command generates a Haskell executable.

   > **Note**
   >
   > Older docs may recommend running `ghc -o <filename.hs>` instead. This is not necessary, unless you want the executable to have a different name than the source file. See the [official flag reference](https://ghc.readthedocs.io/en/8.0.1/flags.html).

4. Run the executable you just created. It should have the same name as your new file, but with an *.exe* extension instead of *.hs*. You will see the following text print to screen.

    ```PlainText
    Hello, world!
    I just wasted an hour looking up how to change a file encoding on Windows.
    A bunch of Linux dunces on StackOverflow didn't know about the `Set-Content -Encoding` command and sent me on a wild goose chase.
    ```

#### What's going on?

The first line in this program asserts that `main` is an *action* of type `IO`, or in-out. That means that it is a value that can accept input and spit out an output.

The next line states that `main` will be `do`ing several things when it is called. If we didn't add the `do` keyword, main could only perform one thing.

Each of the things main will be doing are print statements. `PutStrLn` is a function prints the string and adds a line break character at the end. There is a similar function, `putStr`, that would just print the string without adding any breaks. In Haskell, function arguments are placed next to the function's name, without needing to be enclosed in parentheses.

Also note that the print statements are indented under the `do` -- like Python, whitespace has syntacical meaning in Haskell.

#### Adventures in file encoding

I thought I would be clever and write a new file from the CLI, using `echo`. However, this brought some unexpected results: files created in this way are generated as UTF-16 under the default Windows 10 configuration. And a fresh install of GHC  can't compile UTF-16 files.

In addition, there isn't really a native command to let you inspect the encoding of a file from PowerShell. You can, alledgedly, use `file` (the Linux file info command) on recent versions of PowerShell -- however, my copy of PowerShell didn't recognize the command.

In Windows you can just open the file in a graphical text editor to see the encoding, or look at the first three bytes of the file, but neither way was really satisfactory.

Thankfully, I also have Git Bash and the Windows Subsystem for Linux installed, so I was able to check the encoding with `file` and confirm it was UTF-16.

So how do you fix an incorrect encoding?

##### Fix one file

If you want to change the encoding of a single file to UTF-8 via PowerShell, you can run `Set-Content -Encoding utf8 <filename>`. Note that this will delete the current contents of the file.

##### Use UTF-8 for new files going forward

If you want to use UTF-8 across the board, and are running Windows 10 1809 or higher, you need to go into **Language settings** > **Administrative language settings** and check off **Use Unicode UTF-8 for worldwide language support**. See [Change default code page of Windows console to UTF-8](https://superuser.com/a/1435645). Earlier versions of Windows require you to mess around inside of the registry, which is also described at the link, in previous answers to the question.

#### *Hello world* variants

I was working off of an [older tutorial about IO in Haskell](https://wiki.haskell.org/Introduction_to_IO) and didn't realize two things:

1. I really should have put a header on my file.
2. I didn't strictly need the `main :: IO ()` line to get it to work.

Haskell files should have a header on the first line stating where the entry point for the program is, though in my case it was implied. My header should have looked like this:

```Haskell
module Main where
```

Meanwhile, I intended the `main :: IO ()` bit to ensure that the action was executed, but I could have just written something like the following and it would have run just fine:

```Haskell
main = putStrLn "I'm a minimalist Haskell `Hello world` program!"
```

Renegade Coder has [an article](https://therenegadecoder.com/code/hello-world-in-haskell/) describing more details.
 