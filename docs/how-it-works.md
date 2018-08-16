How Paradise-NG works
=====================

This document specifies what exactly happens when this plugin does its thing. It
may help internalize the limitations that arise naturally from this design.

Major steps
-----------

First of all, it should be noted that this plugin does everything before the
typing phase happens. So, what we have initially is an abstract syntax tree
without any types.

  1. First, we try to add types to a copy of the initial tree where possible by
     running a typechecker on it.
  2. Next, we find the trees annotated with trees with types that are children
     of `ParadiseNgAnnotation`.
  3. For each tree that represents a class definition, trait definition, or type
     definition, we try to find a companion object.
  4. Now we have a list of the trees that we will need to expand annotations on.
     We order it in such a way that the children of a tree are encountered
     earlier than the tree itself, and a companion object of a definition is
     encountered earlier than the definition. This is the order in which macro
     annotations will be expanded.
  5. For each macro annotation (given to us in the form of a tree), try to
     acquire the corresponding class instance.
  6. Now, we parse the whole source code once again, but this time with
     `scalameta`.
  7. For each tree in the to-be-expanded list, we do the following:
        * First, we locate the corresponding subtree in the `scalameta` tree.
          We use the assigned positions to orient ourselves. If the tree has a
          companion object, we locate it too.
        * Next, we use `AnnotationCombination` to combine the annotations.
        * We apply the `pluginInterop` of the resulting instance to the located
          subtree alongside and the subtree of its companion object, if present.
        * Finally, we collect all the resulting trees and replace the given
          subtree in the `scalameta` tree with them.
  8. We convert the result to the string representation, parse it with the
     parser on the compiler's side, and replace the initial tree with the
     result.

Limitations
-----------

### Expansions can't add macro annotations

One may be tempted to create an annotation that adds other macro annotations to
trees. This won't work because at the stage when macro annotations are being
applied we already know all the expansions that we'll perform.

### Annotations don't know about companions created by other expansions

Let's say there's an annotated class `A` and an annotated class `B` and the
annotation for `B` creates an `object A`. This new object won't be considered
when expanding `A`! Technically, since we're doing everything before the typing
phase, the newly-created object will be considered the companion of `A`, but
our macro system won't know this.
