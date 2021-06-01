module Events

open Domain

type Event =
    | AddTodoEvt of TodoItem
