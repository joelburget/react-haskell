-- I think building ASTs like this could lead to advantages:
-- * performance improvements
--   - the StatefulReactT monad is really a function from state to AST and
--     a new state. to get the AST you need to evaluate the function. react
--     also has planned support for defining the virtual dom in this way
--     rather than by function calls, the idea being that things like
--     react-haskell could interface in that way.
-- * introspection, i guess
--   - you can inspect a piece of DOM being passed around without
--     evaluating the function
--
-- ReactNode already exists, but it's not convenient.

main = do
    Just elem <- elemById "inject"
    render elem $
        Div [("className", Str "foo")] [
            [ Text [] "some string"
            , Div [("className", Str "bar")] []
            , Pre [] [ Text [] "this thing should be in a pre" ]
            , Text [] "some other string"
            ]
        ]
