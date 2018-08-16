Migrating from Paradise
=======================

There are a few important ways in which this project differs from
scalameta/paradise.

Omissions
---------

First, let's list the features that `paradise` has and this project doesn't.

### Old-style macros

`paradise` supported two types of macro annotations: the "old-style", based on
the scalac's macro expansion facilities, and "new-style" or "inline", using the
`scalameta` library to define tree transformations. We don't support old-style
macros at all.

### Annotations on parameters

In `paradise`, one could do
```scala
class A(@myAnnotation val x: String)
```
This way of annotating wasn't really intuitive because it modified `class A`
and not, as one would expect, just `val x`.

### Using `this` to access the annotation's tree

In `paradise`, one could do perform this magic:

```scala
class myAnnotation(commonFields: scala.Symbol*)
extends scala.annotation.StaticAnnotation {
    inline def apply(defn: Any): Any = meta {
        val commonFields = this match {
            case q"new $_(..$xs)" => xs.map {
                case ctor"$_(${Lit(x: String)})" => x }.toList
            case _ => Nil
        }
        ...
    }
}
```

The relevant part here is how `this` behaves. Specifically, it's completely
magical and refers not to the annotation itself but to the `scalameta` tree
corresponding to the annotation.

#### Workaround

Just use the annotation parameters.

Differences in syntax
---------------------

These are the ways the plugins differ.

### General annotation specification syntax

In `paradise`, macro annotations were defined like this:

```scala
class myAnnotation extends scala.annotation.StaticAnnotation {
    inline def apply(defn: Any): Any = meta {
        // body here
    }
}
```

They relied on new syntax (the new `inline` keyword), used meta-blocks and
weren't strict at all in their types.

We define annotations in a drastically different way, not relying on
special syntax at all to distinguish our annotations and instead checking
whether a class is both an annotation and extends a specific trait.

So, no special syntax required. Just extend `ParadiseNgAnnotation`, and
you're good to go!

```scala
class myAnnotation extends ParadiseNgAnnotation {
    def apply(defn: Stat): Stat = {
        // body here
    }
}
```

### Aborting

`paradise` supplied through `scalameta` special functions called `abort`.
Since we don't do magic, now you are supposed to simply throw an exception.

### Speciyfing type parameters on annotations

It was possible in `paradise` to write annotations like this:

```scala
class myAnnotation[T] extends scala.annotation.StaticAnnotation {
    inline def apply(defn: Any): Any = meta {
        q"class A[$T] { }"
    }
}
```

That is, type parameters behaved like normal parameters and contained
`scalameta` trees corresponding to the type parameters.

We instead chose not to make annotations magical in this regard, and
for the same behavior you can do

```scala
import scala.meta._
class myAnnotation(T: Type) extends ParadiseNgAnnotation {
    def apply(defn: Stat): Stat = {
        q"class A[$T] { }"
    }
}
```

Instead of annotations being magical, their invocation is, for the sake of
passing type parameters in an elegant way. Any of the type parameters that are
passed to the annotations are transformed to normal parameters and prepended to
the normal parameter list. So, this:

```scala
@myAnnotation[Int](3, 4, "s") class A
```

effectively behaves like this:

```scala
@myAnnotation(t"Int", 3, 4, "s") class A
```
