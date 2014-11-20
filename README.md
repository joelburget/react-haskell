# React-Haskell

Bindings for React under Haste.

## Example

```haskell
render elem $ div $ do
    span <! className "im-a-span" $ "some string"

    pre "this thing should be in a pre"

    "some other string"
```

## Getting Started

TODO

## Notes

Jordan Walke [sketched out](https://gist.github.com/jordwalke/67819c91df1552009b22) a similar API for OCaml.

We should try to adhere to React's [Virtual DOM Terminology](https://gist.github.com/sebmarkbage/fcb1b6ab493b0c77d589) when possible.

## License

[MIT License](http://opensource.org/licenses/MIT)
