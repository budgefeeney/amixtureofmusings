---
title: Associated Types and Haskell
date: 2016-05-19 18:00
draft: false
tags: Haskell, Programming
---

*Or how to write a type-class containing a function returning a value of another related type-class.*

Lets assume you're using type-classes extensively to make your business logic independent of your data-representation. Some may argue whether this is a good idea, but that's not relevant to what follows. This article explains why you'll encounter a bug, and how to use the `TypeFamilies` extension to work around it.

Assume you have a type-class that defines a few functions:

```haskell
type Position = (Int, Int)

class Player p where
  playerPosition :: p -> Position
  playerMoveTo :: Position -> p -> p
  -- etc. and lots more
```

Now lets say this is all part of your game state, which you also define via a type-class

```haskell
class GameState g where
  getPlayer :: g -> Player
  getMonsterPositions :: g -> [Position]
```

So far, so good. All of this compiles. We can even create implementations:

```haskell
data PlayerData = PlayerData { _pos :: Position, ... }
instance Player PlayerData where
  playerPosition = _pos
  playerMoveTo pos player = player { _pos = pos }
  -- etc..

data GameStateData = GameStateData PlayerData [Position]
instance GameState GameStateData where
  getPlayer           (GameStateData p _) = p
  getMonsterPositions (GameStateData _ mPoses) = mPoses
```

This still compiles. The problem occurs when you try to write a function using these classes:

```haskell
checkForCollisions :: GameState -> [Position] -> Bool
checkForCollisions s ps =
  let
    p    = getPlayer s
    pPos = playerPosition player
  in
  foldl False (\a oPos -> a || pPos == oPos) ps
```

This function will not compile. The error will be something along the lines of `Could not deduce (p ~ Player)`

## Polymorphism vs Monomorphism

To someone coming from the imperative object-orientated world, this seems mysterious, as one could trivially achieve the same effect using interfaces.

The issue is the difference between polymorphism and monomorphism.

Consider the following Java code

```java
interace MyInterface {
  public int someFunction();
}

public static <I extends MyInterface> I genericFunc(int a) { ... }

public static void int callGenericFunc() {
  MyInterface mi = genericFunc(42);
  return mi.someFunction();
}
```

What we are saying is that

 * There is a family of types that implement `MyInterface`
 * `genericFunc` will return a value of a single, specific type in that family[[1]](#fn1)
 * `callGenericFunc` must be able to handle any value in that family, regardless of its underlying representation (i.e. it must be polymorphic)
 * It is `genericFunc` that chooses the particular type.

The following Haskell code looks very similar:

```haskell
class MyTypeClass t where
  someFunction :: t -> Int

genericFunc :: (MyTypeClass t) :: Int -> t
genericFunc = ...

callGenericFunc :: Int
callGenericFunc =
  let mt = genericFunc 42 in
  someFunction mt
```

In the Haskell version, we are saying that

 * There is a family of types that implement `MyTypeClass`
 * `genericFunc` can return a value of _any_ type in that family. <br>Whereas the Java version returned a value of a single specific type.
 * `callGenericFunc` will only work on _one specific type_ in that family.<br>Whereas the Java version worked on _any_ value in the family
 * It is `callGenericFunc` which decides which type `genericFunc` should return.<br>Whereas the Java version had `genericFunc` make the decision

So while the Java compiler renders this generic code _polymorphic_, by adapting `callGenericFunc` to work with any value in the `MyInterface` family, Haskell makes the code _monomorphic_ by choosing a single specific type in the `MyTypeClass` family and generating variants of `genericFunc` and `callGenericFunc` which work on that type.

There are a few advantages to this. On a machine level, forcing everything to a single concrete type allows for static dispatch and therefore function-inlining which is a performance optimisation[[2]](#fn2). This is is why you see monomorphism appearing in recent ML-derivatives like Swift and Rust.

The second is that it means the typeclass code you write is incredibly generic, and can work in whichever way the caller requires.

However in order for monomorphism to work, the compiler needs to be able to identify the particular type that `callGenericFunc` will use.

## Associated Types to the Rescue

If we look at our example again, we can see the problem

```haskell
checkForCollisions :: GameState -> [Position] -> Bool
checkForCollisions s ps =
  let
    p    = getPlayer s
    pPos = playerPosition player
  in
  foldl False (\a oPos -> a || pPos == oPos) ps
```

`GameState` is a generic typeclass, so the compiler will inspect the code that calls `checkForCollisions` to choose the specific implementation.

The typechecker now looks at `checkForCollision` and sees `getPlayer` returns a value of another generic typeclass `Player`.

Remember it's _not_ the implementation of `GameState` for `GameStateData` that must determine the type, it's `checkForCollisions`, so that's where the type-checker looks.

Unfortunately, all the code in `checkForCollisions` is completely generic, so it can't choose a single concrete type: hence `Could not deduce (p ~ Player)`.

The solution to this to allow the implementation of `GameState` for `GameStateData` to identify the particular type to use, by associating a type with the type-class.

To this this we use the `TypeFamilies` extension.

First we alter our type-class to add a type placeholder called `PlayerType`

```haskell
{-# LANGUAGE TypeFamilies #-}

class Player (PlayerType s) => GameState s where
  type PlayerType s :: *
  getPlayer :: s -> PlayerType s
```

Essentially PlayerType is a variable that contains a type rather than a value. Consequently it's annotated with a _kind_ (recall, that the "type of a type" is called a "kind"). In this case the single asterisk means that this should be a concrete type.

Associated types must be tied (i.e. associated with) the type defined in the type class, which is why it's `PlayerType s` and not just `PlayerType`.

However we can still constrain the associated type to be in our `Player` type-class as you can see.

You can have as many associated types as you want by the way, I've just used one in this example for simplicity.

The next step is to assign a particular concrete type in our implementation:

```haskell
{-# LANGUAGE TypeFamilies #-}

instance GameState GameStateData where
  type PlayerType GameStateData = PlayerData
  getPlayer           (GameStateData p _) = p
  getMonsterPositions (GameStateData _ mPoses) = mPoses
```

This then solves our problem. The code that called `checkForCollisions`  has already chosen the particular type in the `GameState` family, and lets say it's `GameStateData`.

The compiler next looks at `checkForCollisions`, but now it knows that sees that for the `GameStateData` implementaton of the `GameState` class, the associated type used for `getPlayer` is `PlayerData`, and which is in the `Player` typeclass. Hence the code type-checks, and the compiler has the information it needs to monomorphise it.

## Final Thoughts

This only really rears it's head once you start making extensive use of type-classes. Since defining and altering types is so easy in Haskell, you can argue that there's no need for typeclasses. In fact, since abstraction bulks out code, and so can make it harder to read, there's an argument to be made _against_ the use of typeclasses.

However there are many Haskellers that use typeclasses as a way to write code in an "effectful" style (emulating the effects features in other pure functional languages like PureScript) and it's here that they can run into issues, as I did.

In my case, I had a function like

```haskell
goto :: (HasLevel r, MonadReader r m, HasPlayer p, MonadState p s)
     => Direction -> m MoveResult
```

And in this case I'd defined `HasLevel` to return a value in a `Level` type-class so the game engine could work equally well with different sorts of `Level` implementations. As it turned out, in the end, I only had the one implementation, so this was an unnecessary and premature abstraction.

In short, I wouldn't encourage developers to use this on a regular basis. It is a useful trick to know, however, particularly since it's begun to appear in other, more mainstream ML-derivatives like Swift and Rust.

----
<small>
1. <a name="fn1" /> This is not strictly true, you can have an if-statement in Java code which will return one type in one branch, and another type in another branch. The point being the Java code can only return values from a small subset of types in the `MyInterface` family whereas the Haskell code can return a value of any of the types in the `MyTypeClass` family.
</small><br>
<small>
2. <a name="fn2" /> Polymorphic code looks up a table for every function call, then calls the function. Static dispatch calls the function directly, without a run-time lookup and so is faster. Inlining skips the function call overhead entirely, copying the function body into the calling code, and so is faster still. However as this copying makes the overall size of your code greater, it can overflow the cache, which will make your code run _much much slower_. As a result, inlining is not an automatic win.
</small>
