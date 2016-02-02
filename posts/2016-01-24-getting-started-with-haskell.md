---
title: Getting Started with Haskell
date: 2016-01-23 13:00
math: false
draft: false
tags: haskell
---

This guide aims to help those attempting to learn Haskell. It explains:

1. How to [install the Haskell compiler and development tools](#setting-up-haskell). The tools include `cabal` build & dependency-management app; the `ghc-mod` IDE support tool; the `stylish-haskell` code-formatter; and the `hi` ("Haskell init") project-scaffolding tool
2. How to install and configure an [IDE](#installing-an-ide) and [REPL](#the-ihaskell-repl)
3. How to use the `hi` (Haskell Init) project-scaffolding tool to [initialise a new project](#creating-your-first-project).
4. Which are the [best libraries](#recommended-libraries) for regular-expressions, database access, validation etc.
5. What are the [best Haskell learning-resources](#actually-learning-haskell)

There are some tips for Mac users in particular.

While ostensibly straightforward, it takes a surprising amount of effort to install all of these: particularly to install the particular versions of each that work well together.

In recent times, the [Stackage](https://www.stackage.org) project has been launched by the people at [FP Complete](http://www.fpcomplete.com) to address this. Similar to a Linux distribution, Stackage maintains a collection of particular Haskell libraries ("packages") and applications, at specific versions, which are all tested to not only be stable themselves, but stable with respect to each other. However as there is no IDE support for Stack at the moment, I won't discuss stack here.

## Setting Up Haskell

The minimum version to target for GHC is 7.10, and for Cabal, it's 1.22.

Users of Mac OS X will first need to ensure they have the command-line development-tools installed. To do this install XCode from the app-store, then on the command-line run the following.

```
sudo xcode-select --install
sudo xcodebuild -license
```

Next, install the Haskell [Haskell Platform](https://www.haskell.org/platform/).

The [Haskell Platform](https://www.haskell.org/platform/) is also the best option for Windows users.

For Linux users, the first option is obviously their native package manager, but if that doesn't have GHC 7.10, then they too should use the Haskell platform.

Once done, you should have `ghc`, `ghci`, `runghc`, and `cabal` all on your path. GHC is the compiler obviously, `ghci` is a simple REPL, and `runghc` compiles and runs the given script.

Cabal is a build & dependency-management tool similar to Maven, or

The next step is just to update the Cabal package database to the latest version, using

```
cabal update
cabal install cabal cabal-install
```

For those who are confused, [cabal](https://hackage.haskell.org/package/Cabal) is the library and [cabal-install](https://hackage.haskell.org/package/cabal-install) is the application called -- somewhat confusingly -- `cabal`.

Cabal can also be used to install Haskell applications: when you execute the `cabal install ...` command above it will tell you where such applications will be installed: on Macs using the Haskell platform it's `$HOME/Library/Haskell/bin`. Make sure to update your path accordingly.

### A Mac(Ports) Conundrum

Haskell and MacPorts don't always play well together. Some third-party Haskell libraries (or "packages") when being built will opportunistically link to libraries in the MacPorts installation directory, `/opt/local`. This directory includes libraries duplicating those on the system, notably `libiconv`. This can then lead to linker errors when the wrong iconv library is picked up by the linker. You can read [an in-depth description of the MacPorts library problem here](http://blog.omega-prime.co.uk/?p=96), but the short answer is that if you have linker trouble when building an application, particularly if it involves iconv and HSBase, you may want to try amending the project configuration and rebuilding by typig

```
cabal configure --extra-lib-dir=/usr/lib
cabal build
```
This will force a search of `/usr/lib` is before the MacPorts' directory. For whatever reason, the [relocatable GHC package](https://ghcformacosx.github.io/) is less affected by this, but as of February 2016, it does not work on Mac OS X "El Capitan".


## Installing an IDE

As of February 2016, I have found no process which is guaranteed to always provide a new user with an IDE that works reliably, at least not on a Mac.

The [IDE Haskell plugin](https://atom.io/packages/ide-haskell) which works with the [Atom editor](http://atom.io) is the most promising at the moment, however while I've got it to work with GHC 7.10, it was after several false starts. Ghc-mod, on which it relies, will often crash out silently due to malformed Haskell or Cabal files, in which case you need to relaunch the editor. At other times, ghc-mod will launch some expensive, long-running process that confuses Atom.

The [IntelliJ IDEA](https://www.jetbrains.com/idea/) plugins, both IntelliJ's own and [HaskForce](https://carymrobbins.github.io/intellij-haskforce/) similarly struggle with GHC 7.10 and ghc-mod.

The [EclipseFP plugin](http://eclipsefp.github.io/) used to work very well indeed, but it relies on a tool called `buildwrapper` which ironically will not build with GHC 7.10 & Cabal 1.22

The [Leksah](http://leksah.org) application is a Haskell IDE written in Haskell which will definitely work. Visually it looks very poor on both Mac OS X and Windows, and it has an unorthodox and noisy layout that takes time to get used to.

Therefore the next best approach after Atom/IDE-Haskell, is to use the web-based IDE provided by [FP Complete](http://www.fpcomplete.com). While guaranteed to work, this obviously this requires a constant internet connection, and a github hosted project.

Since Atom, when it works, works very well indeed, it's worthwhile trying to install and configure it however, which is what we describe next.


### Atom IDE-Haskell Installation

Install [Atom from its website](http://atom.io) and then use the command-line and cabal to install the Haskell IDE support tools:

```
cabal install happy
cabal install ghc-mod hlint hoogle stylish-haskell
```

Happy needs to be installed first and separately as it is an undeclared dependency of `haskell-src-exts`, which is in turn required by `ghc-mod`.

Then launch Atom and install the following packages:

 * haskell-ghc-mod
 * autocomplete-haskell
 * language-haskell
 * ide-haskell

You will likely need to configure each of these individually to enter the full paths to cabal, ghc, ghc-mod etc. When editing the settings for haskell-ghc-mod you enter both the path to the cabal installation directory (e.g. `/Users/myusername/Library/Haskell/bin`) _and_ the path to ghc (e.g. `/usr/local/bin`) to the "Additional Path Directories" field, the pair separated by a comma.

Then quit Atom, find a Haskell project (or create one as shown below) and restart Atom.

Add the project directory using the "Add Project Folder" item of the "File" menu, then open its Cabal file to trigger the launch of the Haskell IDE support, complete with its eponymous "Haskell IDE" menu. If you're lucky, autocompletion, error-detection, linting and building tools will all become available.

> Before opening a project with Atom, you may want to consider "test-driving" ghc-mod by launching it from the command-line with a sample file to check, using `ghc-mod check /path/to/file.hs`. GHC-Mod does occasionally do some very time-consuming work such as building its own cabal, and this tends to confuse IDEs mightily. Moreover, before you launch either Atom or `ghc-mod` for the first time on a new project, you should ensure that running `cabal build` works first.



## The IHaskell REPL
A good REPL is invaluable when learning a new language, a new library, or just playing around with ideas. IHaskell is the best Haskel REPL I've come across.

It's based on [IPython Notebook](http://ipython.org/notebook.html), a web-frontend that lets you interleave markdown and live code in a virtual notebook. In 2015 this was split into two projects, a ["Jupyter"](http://jupyter.org/) core and a Python plugin. This was to explicitly accommodate other languages, such as Haskell, using the [IHaskell](https://github.com/gibiansky/IHaskell) plugin.

Linux users can install Jupyter using their package manager, though depending on their distribution it may still be called ipython, and then install ihaskell using

```
cabal install ihaskell
ihaskell install
```

Once installed, launch the notebook server by typing `ipython notebook`. Despite the name, you will see you will have the option of creating either Python or Haskell notebooks.

For OS X users you can use [Homebrew to install Python, Pip to install ipython](https://joernhees.de/blog/2014/02/25/scientific-python-on-mac-os-x-10-9-with-homebrew/), and then cabal to install IHaskell as shown above. Should you try to install Python and IPython using MacPorts, you immediately encounter the usual MacPorts / system library linking issues. The easiest, though spectacularly wasteful, solution to this is to install [Kronos Haskell](http://www.kronosnotebook.com/haskell) which is a 2GB app bundle with its own copies of Python, Jupyter, GHC, Cabal and the usual Haskell libraries.

For windows users, the Kronos option is by far and away the simplest approach.

## Creating your first project

We'll use the [Haskell Init tool](https://github.com/fujimura/hi). If you haven't already installed it, do so by opening a console and typing

```
cabal install hi
```

To create a project called `my-project-name` in a directory called `my-project-dir` type

```
hi --repository git://github.com/tfausak/haskeleton.git \
   --directory-name my-project-dir \
   --package-name my-project-name \
   --module-name MyProjectName \
   --author "Bryan Feeney" \
   --email bryan@amixtureofmusings.com
```

The repository flag is not the new project's repository; rather it gives a path to a template project, in this case the Haskeleton template. This  features unit-tests, documentation tests and benchmarks, with all of these explained in the [Haskeleton guide](http://taylor.fausak.me/2014/03/04/haskeleton-a-haskell-project-skeleton/). Other project templates, such as web-apps, can be found on the [HI templates page](https://github.com/fujimura/hi/wiki#available-templates))

Module names, which are camel-cased, are used in Haskell code while package names, which are dashed, are used on the [Hackage package repository](http://hackage.haskell.org).

Before you start coding, you'll want to download the skeleton project's dependencies. Rather than mix these dependent packages in with your global package repository, create a local package sandbox for your project:

```
cd my-project-dir
cabal sandbox init
cabal update

cabal install --only-dependencies
cabal install --enable-test  --only-dependencies
cabal install --enable-bench --only-dependencies
```

Cabal can automatically detect a sandbox, so once it's created you can just use the usual commands. Typing `cabal install --only-dependencies` installs the project's dependencies as given in the Cabal file. By default this is for the library and/or executable only. You need additional calls to download and install any dependencies required for the unit-tests and benchmarks.

As a quick check, make sure everything builds:

```
cabal build
```

And then you're safe to open the project in Atom. Obviously you can also run tests using `cabal test`, and execute the benchmarks using `cabal bench`, though if you want benchmarks presented as graphs rather than ASCII text, type:

```
mkdir -p dist/bench
cabal bench --benchmark-options="--output dist/bench/index.html"
```

Note benchmarks are only available with the Haskeleton project template.


## Actually Learning Haskell

There are two free online resources that are popular:

* [Learn You a Haskell (LYAH)](http://learnyouahaskell.com) is very slightly out of date[[1]](#fn1), is extremely readable, but ends without ever demonstrating how to code, let alone design, a full application
* [Real World Haskell](http://book.realworldhaskell.org) tends to work through doing things the hard way before showing how they can be done simply - an approach which makes learning Haskell unnecessarily tedious. At one point it was very useful, as it tackled typical programming problems. Unfortunately it is now _horribly_ out of date, and while commenters have provided corrections, there's not much they can do when the book uses entirely the wrong (by contemporary standards) libraries for things such as regular expressions.

This is a case where the best approach is to actually buy a book, in this case [Beginning Haskell: A Project-Based Approach](http://www.amazon.com/Beginning-Haskell-A-Project-Based-Approach/dp/1430262508). While obviously more expensive than free, it pays its own way in terms of time saved.

Once you've learnt the core language, you will likely find  [What I wish I knew when learning Haskell](http://dev.stephendiehl.com/hask/) to be a useful read. It is a sort of Haskell-by-example, describing all the practical aspects of application development you need to be aware of. Particularly if you've learnt Haskell from [Learn You a Haskell (LYAH)](http://learnyouahaskell.com), this is the recommended next step.

Also the [24 Days of ...](https://ocharles.org.uk/blog/) guides by Oliver Charles are a nice way of dipping in and out of various Haskell packages and extensions.


## Recommended Libraries

All Haskell libraries, or "packages", can be found on [Hackage](http://hackage.haskell.org). There are no limits on who can upload packages: indeed there is a culture of uploading packages early to see if they get traction. Consequently there are a lot of incomplete, amateur packages; there are multiple packages for every use-case; and the quality of packages varies considerably.

For common cases my recommendations are:

* For **text**, you'll need to wean yourself off the built-in `String` type and move onto `Data.Text`, from the [text](http://hackage.haskell.org/package/text) package. This offers much better time and memory performance. You'll also find yourself using the [OverloadedStrings](https://ocharles.org.uk/blog/posts/2014-12-17-overloaded-strings.html) extension

* For **regular expressions** the [PRCE-Heavy](https://hackage.haskell.org/package/pcre-heavy) library is the one to use. It employs Haskell's quasi-quoting mechanism -- Haskell's equivalent of a preprocessor and macros -- to perform compile-time checking of your regular expressions. Make sure you look at [PCRE's unicode documentation](http://www.regular-expressions.info/unicode.html) as well, PCRE-Heavy is unicode-aware by default

* **Database access** is a sore point as most of Haskell's database access libraries are pretty slow. The fastest two currently only work with the Postgres database; they are the complex [hasql](https://hackage.haskell.org/package/hasql) and the less complex [postgreql-typed](http://hackage.haskell.org/package/postgresql-typed). At a higher level, the [persistent](https://hackage.haskell.org/package/persistent) package abstracts over all databases, but does not support the fast Postgres libraries I've mentioned. There is also [SqlLite-Simple](https://hackage.haskell.org/package/sqlite-simple) for simple applications

* For **websites** [Snap](http://snapframework.com/) is a popular choice these days, but there is also [Scotty](https://hackage.haskell.org/package/scotty) for small, simple sites, and  [Servant](https://hackage.haskell.org/package/servant) for REST APIs. If you're interested in small sites, you can read [this tutorial on creating a blog with Scotty](http://adit.io/posts/2013-04-15-making-a-website-with-haskell.html)

* For JSON processing use [Aeson](https://hackage.haskell.org/package/aeson)


* For **validation** [validate-input](https://hackage.haskell.org/package/validate-input) is easy to get up and running, but [validation](https://hackage.haskell.org/package/validation) is much more fully featured.

* For **Extract-Transform-Load (ETL)** jobs use the [conduit](https://hackage.haskell.org/package/conduit) package

* For **error handling** most beginners use `Either`, but you should use the more abstract [errors](https://hackage.haskell.org/package/errors) approach instead. There is a [tutorial on the errors package here](http://www.haskellforall.com/2012/07/errors-10-simplified-error-handling.html), and a roundup of all the other approaches people have previously tried [here](http://blog.ezyang.com/2011/08/8-ways-to-report-errors-in-haskell-revisited/)

* For **concurrency** there are many options, so the best advice is to read [this section on concurrency](http://dev.stephendiehl.com/hask/#concurrency) from [What I wish I knew when learning Haskell](http://dev.stephendiehl.com/hask/). In general usage, you'll probably find the [async library](https://hackage.haskell.org/package/async) the easiest to use. There's an [example here](http://dev.stephendiehl.com/hask/#async) and a short [tutorial here](https://ocharles.org.uk/blog/posts/2013-12-13-24-days-of-hackage-async.html).



---

 1. <a name="fn1" /> Specifically the eponymous constructors for the `State`, `Writer` and `Reader` monads have all been hidden, and instead replaced with lower-case functions `state`, `writer` and `reader`, affecting the code examples in chapter 13.
