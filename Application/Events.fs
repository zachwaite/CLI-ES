module Events

type AddTodoMsg = { Name: string }

type Event = AddTodoEvt of AddTodoMsg
