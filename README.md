# React-Haskell [![Hackage](https://img.shields.io/hackage/v/react-haskell.svg?style=flat-square)](https://hackage.haskell.org/package/react-haskell)

As crazy as it seems, using [React](http://facebook.github.io/react) and [Haskell](https://www.haskell.org) together just *may* be a good idea.

I was driven to create this thing because I had a large existing Haskell codebase I wanted to put online. However, even without existing code, I think a lot of problems are better modeled in Haskell than JavaScript or  other languages. Or you might want to use some existing Haskell libraries.

## Examples

Let's put a simple paragraph on the page:

```haskell
sample :: React () ()
sample = p_ [ class_ "style" ] $ em_ "Andy Warhol"

main :: IO ()
main = do
    Just elem <- elemById "id"
    render elem sample
```

That creates a DOM node on the page that looks like:

```html
<p class="style">
    <em>Andy Warhol</em>
</p>
```

We can make that a little more complicated with some more child nodes.

```haskell
sample :: React () ()
sample = div_ [ class_ "beautify" ] $ do
    "The Velvet Underground"

    input_

    "Lou Reed"
```

But of course that input doesn't do anything. Let's change that.

```haskell
sample :: JSString -> React AppKey ()
sample str = div_ $ do
    "Favorite artist:"

    input_ [ onChange (Just . value . target) ]

    text str
```

## Getting Started

The first step is a working GHCJS installation. The easiest way is to download a virtual machine with GHCJS pre-installed. I recommend <a href="https://github.com/joelburget/ghcjs-box">ghcjs-box</a>.

Now that GHCJS is installed we can use cabal to create a project.

```bash
$ mkdir project
$ cd project
$ cabal init # generate a .cabal file
```
Now edit the cabal file to include dependencies.

```cabal
build-depends:
  base >= 4.8 && < 5,
  ghcjs-base,
  ghcjs-dom,
  react-haskell >= 1.3
```
Now we can write `Main.hs`.

```haskell
sample :: React () ()
sample = p_ [ class_ "style" ] $ em_ "Andy Warhol"

main :: IO ()
main = do
    Just elem <- elemById "id"
    render elem sample
```

## Next Steps

### Reference

* [haddocks](http://hackage.haskell.org/package/react-haskell)
* [examples](https://github.com/joelburget/react-haskell/tree/master/example)

### Additional Resources

* [Animating Web UI with React and Haskell](http://joelburget.com/react-haskell) (article)
* [Writing a React JS front-end in Haskell](http://begriffs.com/posts/2015-01-12-reactjs-in-haskell.html) (video)

## Is it Right for Me?

React-Haskell is a great tool for building web UI from Haskell. However, you may want to consider the alternatives:

* By writing plain React / JSX you can speed development by avoiding the GHCJS compilation step. This also has the advantage of being a bit more universal - more people use React through JSX than React-Haskell.
* [ghcjs-react](https://github.com/fpco/ghcjs-react) is a very similar project.
* [Reflex](https://github.com/ryantrinkle/try-reflex) is an FRP system built with GHCJS in mind.

## Small Print

[MIT License](http://opensource.org/licenses/MIT)

[![Bitdeli Badge](https://d2weczhvl823v0.cloudfront.net/joelburget/react-haskell/trend.png)](https://bitdeli.com/free "Bitdeli Badge")
