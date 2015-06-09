{-# LANGUAGE OverloadedStrings, NamedFieldPuns, Rank2Types, TemplateHaskell, LiberalTypeSynonyms, RebindableSyntax, DataKinds  #-}
module Main where
-- TODO:
-- * persistence
-- * routing

import Prelude hiding ((>>), (=<<), return)

import Control.Applicative
import Data.String
import Data.Void

import GHCJS.Foreign
import GHCJS.Types
import GHCJS.DOM (currentDocument)
import GHCJS.DOM.Types (Document)
import GHCJS.DOM.Document (documentGetElementById)
import Lens.Family2
import Lens.Family2.Stock
import Lens.Family2.TH
import React

import System.IO.Unsafe

import Debug.Trace

-- MODEL

data Status = Active | Completed
    deriving Eq

data Todo = Todo
    { _text :: JSString
    , _status :: Status
    }

data PageState = PageState
    { _todos :: [Todo]
    , _typingValue :: JSString
    }

$(makeLenses ''Todo)
$(makeLenses ''PageState)

type TodoMvc = React RtBuiltin Transition

initialPageState :: PageState
initialPageState = PageState
    [Todo "abc" Active, Todo "xyz" Completed,
     Todo "sjdfk" Active, Todo "ksljl" Completed]
    ""

data Key = Enter | Escape

data Transition
    = Typing JSString
    | HeaderKey Key
    | Check Int
    | DoubleClick
    | Destroy Int
    | ToggleAll
    | ClearCompleted

pageTransition :: Transition -> PageState -> PageState
pageTransition (Typing str) = handleTyping str
pageTransition (HeaderKey Enter) = handleEnter
pageTransition (HeaderKey Escape) = handleEsc
pageTransition (Check i) = handleItemCheck i
pageTransition DoubleClick = handleLabelDoubleClick
pageTransition (Destroy i) = handleDestroy i
pageTransition ToggleAll = handleToggleAll
pageTransition ClearCompleted = clearCompleted

-- UTILITY

toggleStatus :: Status -> Status
toggleStatus Active = Completed
toggleStatus Completed = Active

foreign import javascript unsafe "$1.trim()" trim :: JSString -> JSString

-- this traversal is in lens but lens-family has a weird ix which isn't
-- what we want. definition just copied from lens.
-- TODO(joel) just use lens?
ix' :: Int -> Traversal' [a] a
ix' k f xs0 | k < 0     = pure xs0
            | otherwise = go xs0 k where
    go [] _ = pure []
    go (a:as) 0 = (:as) <$> f a
    go (a:as) i = (a:) <$> (go as $! i - 1)

-- remove an item from the list by index
iFilter :: Int -> [a] -> [a]
iFilter 0 (a:as) = as
iFilter n (a:as) = a : iFilter (n-1) as

-- CONTROLLER

handleEnter :: PageState -> PageState
handleEnter oldState@PageState{_todos, _typingValue} =
    let trimmed = trim _typingValue
    in if trimmed == ""
           then oldState
           else PageState (_todos ++ [Todo trimmed Active]) ""

-- TODO exit editing
-- "If escape is pressed during the edit, the edit state should be left and
-- any changes be discarded."
handleEsc :: PageState -> PageState
handleEsc state = state & typingValue .~ ""

emitKeydown :: KeyboardEvent -> Maybe Transition
emitKeydown KeyboardEvent{key="Enter"} = trace "enter" $ Just (HeaderKey Enter)
emitKeydown KeyboardEvent{key="Escape"} = trace "esc" $ Just (HeaderKey Escape)
emitKeydown evt = Nothing

handleTyping :: JSString -> PageState -> PageState
handleTyping _typingValue state = state{_typingValue}

statusOfToggle :: [Todo] -> Status
statusOfToggle _todos =
    let allActive = all (\Todo{_status} -> _status == Active) _todos
    in if allActive then Active else Completed

handleToggleAll :: PageState -> PageState
handleToggleAll state@PageState{_todos} = state{_todos=newTodos} where
    _status = toggleStatus $ statusOfToggle _todos
    newTodos = map (\todo -> todo{_status}) _todos

handleItemCheck :: Int -> PageState -> PageState
handleItemCheck todoNum state =
    state & todos . ix' todoNum . status %~ toggleStatus

-- TODO
handleLabelDoubleClick :: PageState -> PageState
handleLabelDoubleClick = id

handleDestroy :: Int -> PageState -> PageState
handleDestroy todoNum state = state & todos %~ iFilter todoNum

clearCompleted :: PageState -> PageState
clearCompleted state = state & todos %~ todosWithStatus Active

-- VIEW

-- "New todos are entered in the input at the top of the app. The input
-- element should be focused when the page is loaded preferably using the
-- autofocus input attribute. Pressing Enter creates the todo, appends it
-- to the todo list and clears the input. Make sure to .trim() the input
-- and then check that it's not empty before creating a new todo."
header :: PageState -> TodoMvc
header PageState{_typingValue} = header_ [ id_ "header" ] $ do
    h1_ [] $ text_ "todos"
    input_ [ id_ "new-todo"
           , placeholder_ "What needs to be done?"
           , autofocus_ True
           , value_ _typingValue
           , onChange (Just . Typing . value . target)
           , onKeyDown emitKeydown
           ]

todoView :: PageState -> Int -> TodoMvc
todoView PageState{_todos} i =
    let Todo{_text, _status} = _todos !! i
    in li_ [ class_ (if _status == Completed then "completed" else "") ] $ do
           div_ [ class_ "view" ] $ do
               input_ [ class_ "toggle"
                      , type_ "checkbox"
                      , checked_ (_status == Completed)
                      , onClick (const (Just (Check i)))
                      ]
               label_ [ onDoubleClick (const (Just DoubleClick)) ] $ text_ _text
               button_ [ class_ "destroy"
                       , onClick (const (Just (Destroy i)))
                       ] $ text_ ""

           input_ [ class_ "edit", value_ _text ]

todosWithStatus :: Status -> [Todo] -> [Todo]
todosWithStatus stat = filter (\Todo{_status} -> _status == stat)

mainBody :: ReactClass PageState () Transition
mainBody = createClass $ statelessClass
    { name = "MainBody"
    , renderFn = \st@PageState{_todos} _ ->
          section_ [ id_ "main" ] $ do
              input_ [ id_ "toggle-all", type_ "checkbox" ]
              label_ [ for_ "toggle-all" , onClick (const (Just ToggleAll)) ]
                  $ text_ "Mark all as complete"

              let blah = text_ "" >> text_ ""
              ul_ [ id_ "todo-list" ] $ case length _todos of
                  0 -> blah
                  _ -> foldr (>>) blah $ map (todoView st) [0 .. length _todos - 1]
    }

innerFooter :: ReactClass PageState () Transition
innerFooter = createClass $ statelessClass
    { name = "InnerFooter"
    , renderFn = \PageState{_todos} _ -> footer_ [ id_ "footer" ] $ do
          let activeCount = length (todosWithStatus Active _todos)
          let inactiveCount = length (todosWithStatus Completed _todos)

          -- "Displays the number of active todos in a pluralized form. Make sure
          -- the number is wrapped by a <strong> tag. Also make sure to pluralize
          -- the item word correctly: 0 items, 1 item, 2 items. Example: 2 items
          -- left"
          span_ [ id_ "todo-count" ] $ do
              strong_ [] (text_ (toJSString (show activeCount)))

              text_ $ if activeCount == 1 then " item left" else " items left"

          unless (inactiveCount == 0) $
              button_ [ id_ "clear-completed" , onClick (const (Just ClearCompleted)) ] $
                  text_ (toJSString ("Clear completed (" ++ show inactiveCount ++ ")"))
    }

outerFooter :: ReactClass () () Void
outerFooter = React.createClass $ statelessClass
    { name = "OuterFooter"
    , renderFn = \_ _ -> footer_ [ id_ "info" ] $ do
          -- TODO react complains about these things not having keys even though
          -- they're statically defined. figure out how to fix this.
          p_ [] $ text_ "Double-click to edit a todo"
          p_ [] $ do
              text_ "Created by "
              a_ [ href_ "http://joelburget.com" ] $ text_ "Joel Burget"
          p_ [] $ do
              text_ "Part of "
              a_ [ href_ "http://todomvc.com" ] $ text_ "TodoMVC"
    }

-- XXX doesn't sig have to be Void here - IE no signal can escape?
wholePage :: ReactClass () PageState Void
wholePage = createClass $ statefulClass
    { name = "WholePage"
    , transition = pageTransition
    , getInitialState = initialPageState
    , renderFn = \_ s@PageState{_todos} -> div_ [] $ do
          section_ [ id_ "todoapp" ] $ do
              header s

              -- "When there are no todos, #main and #footer should be hidden."
              unless (null _todos) $ do
                  mainBody' s
                  innerFooter' s
          locally (outerFooter' ())
    }

wholePage'   = createFactory wholePage
outerFooter' = createFactory outerFooter
innerFooter' = createFactory innerFooter
mainBody'    = createFactory mainBody

main = do
    Just doc <- currentDocument
    let elemId :: JSString
        elemId = "inject"
    Just elem <- documentGetElementById doc elemId
    render elem (wholePage' ())
