# React-Haskell [![Hackage](https://img.shields.io/hackage/v/react-haskell.svg?style=flat-square)](https://hackage.haskell.org/package/react-haskell)

As crazy as it seems, using React and Haskell together just *may* be a good idea.

I was driven to create this thing because I had a large existing Haskell codebase I wanted to put online. However, even without existing code, I think a lot of problems are better modeled in Haskell than JavaScript or  other languages. Or you might want to use some existing Haskell libraries.

## Examples

Let's put a simple paragraph on the page:

```haskell
sample :: React () ()
sample = p_ [ class_ "style" ] $ em_ "andy warhol"

main :: IO ()
main = do
    Just elem <- elemById "id"
    render elem sample
```

That creates a dom node on the page that looks like:

```html
<p class="style">
    <em>andy warhol</em>
</p>
```

We can make that a little more complicated with some more child nodes.

```haskell
sample :: React () ()
sample = div_ [ class_ "beautify" ] $ do
    "velvet underground"

    input_

    "lou reed"
```

But of course that input doesn't do anything. Let's change that.

```haskell
sample :: JSString -> React AppKey ()
sample str = div_ $ do
    "favorite artist:"

    input_ [ onChange (Just . value . target) ]

    text str
```

## Getting Started

TODO

## Notes

Jordan Walke [sketched out](https://gist.github.com/jordwalke/67819c91df1552009b22) a similar API for OCaml.

We should try to adhere to React's [Virtual DOM Terminology](https://gist.github.com/sebmarkbage/fcb1b6ab493b0c77d589) when possible.

## License

[MIT License](http://opensource.org/licenses/MIT)


[![Bitdeli Badge](https://d2weczhvl823v0.cloudfront.net/joelburget/react-haskell/trend.png)](https://bitdeli.com/free "Bitdeli Badge")

