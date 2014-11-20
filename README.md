# React-Haskell

Bindings for React under Haste.

## Examples

Let's put a simple paragraph on the page:

```haskell
sample :: React
sample = p <! className "style" $ em "andy warhol"

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
sample :: React
sample = div <! className "beautify" $ do
    "velvet underground"

    input

    "lou reed"
```

But of course that input doesn't do anything. Let's change that.

```haskell
sample :: StatefulReact JSString
sample = div $ do
    "favorite artist:"

    input <! onChange (updateState . targetValue)

    text <$> getState
```

## Getting Started

TODO

## Notes

Jordan Walke [sketched out](https://gist.github.com/jordwalke/67819c91df1552009b22) a similar API for OCaml.

We should try to adhere to React's [Virtual DOM Terminology](https://gist.github.com/sebmarkbage/fcb1b6ab493b0c77d589) when possible.

## License

[MIT License](http://opensource.org/licenses/MIT)
