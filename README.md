[![Build Status](https://travis-ci.org/dkhalansky/paradise-ng.svg?branch=master)](https://travis-ci.org/dkhalansky/paradise-ng)
ParadiseNg
==========

This project is intended as a successor to and replacement for the deprecated
[Scalameta Paradise](https://github.com/scalameta/paradise) scala compiler
plugin.

The plugin provides the functionality of assigning a limited form of macros to
annotations.

Getting started
---------------

### First example

Let's define a macro annotation by extending the `ParadiseNgAnnotation` trait
defined in the library part of the project.

```scala
import scala.meta._
import com.github.dkhalansky.paradiseng.lib._
class replaceWithFoo(val barValue: Int) extends ParadiseNgAnnotation {
    override def apply(stat: Stat) = q"object Foo { val bar = $barValue }": Stat
}
```

If we have the plugin enabled, now in a separate subproject we can do

```scala
@replaceWithFoo(42) class arbitraryAndWillBeRemovedAnyway
assert(Foo.bar == 42)
```

You can test this by firing up the REPL with `sbt console` from the root of this
project and copy-pasting these lines there. Make sure not to enable the `:paste`
mode though, or it won't work!

### Basic concepts

This project is divided into two functional parts: the library part and the
plugin part. The library part provides (hopefully) everything you need to define
your own macro annotations, while the plugin is used to expand these
annotations.

You don't need the plugin when defining macro annotations: they are just normal
classes that inherit from normal traits.

### Using it in a project

See `docs/example/` for a complete project using the plugin. Basically, you need
to compile your annotations, for which your macro annotations subproject needs

```
libraryDependencies += "com.github.dkhalansky" % "paradisenglib" % paradiseVer cross CrossVersion.full
```

where `paradiseVer` is the version of this project that you want, and then, to
use the compiled annotations, the code that mentions them needs to be compiled
with the compiler plugin:

```
addCompilerPlugin("com.github.dkhalansky" % "paradisengplugin" % paradiseVer cross CrossVersion.full)
```

Use the following rules of thumb for guidance:

  * The plugin can't apply functions that haven't been compiled yet! Therefore,
    you must ensure that macro annotations are already compiled when they are
    used. The way to do this in SBT is to make a separate project that contains
    the annotations and make your normal code depend on it.
  * Macro annotations are just annotations that inherit `TreeTransformation`,
    even if transitively through `ParadiseNgAnnotation`.

### Advanced usage

If you want to do something non-trivial, it's useful to read
`docs/how-it-works.md`.

Project overview
----------------

### Goals

  * Feature parity with the subset of the `paradise` compiler plugin that
    provides new-style (scalameta-based) macro annotations.
  * Consistency: the thought model behind how macro annotations are expanded
    should be simple. Due to how volatile macros often are, this requirement is
    a must for making understanding complex macro expansion scenarios at least
    possible.
  * Ease of defining new macro annotations and testing them. Ability to test
    macro annotations without resorting to ingenuous tricks such as calling the
    compiler from the test suite and checking its output in order to see if the
    annotations throws the proper exception on wrong input.
  * Helpful error messages.
  * Minimization of the amount of magic required to define new annotations and
    use them. This goal is almost a direct consequence of the previous one but
    has a merit on its own.
  * Easily understandable user-space part of the project: the code in the
    library and the accompanying comments should be enough on their own to
    provide understanding of how to make everything work.
  * Stability: starting from 1.0, the user-space part must be either
    backward-compatible or versioned.
  * Supporting as many versions of `scalac` as is possible without severely
    interfering with the aforementioned goals.

If a part of the project isn't supported by any of these goals, it's redundant.
If a part of the project, when modified, could serve the goals better, the fact
that it hasn't been modified is a bug.

### Non-goals

  * Providing a drop-in replacement for `paradise` or even just for its part
    that deals with new-style annotations. If you want to replace `paradise`
    with this project, you may want to read `docs/migrating.md` for a
    discussion of the differences between the two projects.
  * Making macro annotations Turing-complete. No, we won't accept your pull
    request that wraps all the macro expansion logic in a loop just for that,
    you have to provide another reason.

Troubleshooting
---------------

### Annotations are not expanded.

Make sure you use the compiler plugin in the subproject that uses the
annotations.

### I managed to get everything working, but why can't I do X?

First of all, we suggest trying to find the answer in `docs/how-it-works.md`.
Then, if you still believe that the way plugin works doesn't prohibit X, try
reading `lib`. It isn't actually hard at all, there's very little code. Maybe
the problem is with the inflexible predefined `apply` method and you can get X
by inheriting from `ParadiseNgAnnotation` and redefining what bothers you? If
you still have no idea what prevents X from working, feel free to ask in the
Issues section or, if you're an adventurous type, simply read the `plugin` part.
Good luck to you if you do!
