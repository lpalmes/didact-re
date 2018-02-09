open DidactElements;

external eventToObj : Dom.event => Js.t({..}) = "%identity";

type todo = {
  id: string,
  title: string,
  completed: bool
};

type showing =
  | All
  | Active
  | Completed;

type state = {
  todos: list(todo),
  newTodo: string,
  editing: string,
  nowShowing: showing
};

type action =
  | AddTodo(todo)
  | InputNewTodo(string);

let createElement = (~children, ()) =>
  Didact.component({
    debugName: "TodoMVC",
    initialState: () => {
      todos: [
        {id: "1", title: "Make it work", completed: true},
        {id: "2", title: "Make it correct", completed: false},
        {id: "3", title: "Make it fast", completed: false}
      ],
      newTodo: "",
      editing: "",
      nowShowing: All
    },
    reducer: (action, state) =>
      switch action {
      | InputNewTodo(newTodo) => Didact.Update({...state, newTodo})
      | AddTodo(todo) => Didact.Update({...state, todos: [todo, ...state.todos]})
      },
    render: self =>
      <div>
        <header className="header">
          <h1> (Didact.stringToElement("todos")) </h1>
          <input
            className="new-todo"
            placeholder="What needs to be done?"
            value=self.state.newTodo
            onKeyDown=Js.log
            onChange=(
              e => {
                let value = eventToObj(e)##target##value;
                self.send(InputNewTodo(value));
              }
            )
          />
        </header>
        <h1> (Didact.stringToElement(self.state.newTodo)) </h1>
        <button
          onClick=(
            e => self.send(AddTodo({id: "blue", title: "new todo added", completed: false}))
          )>
          (Didact.stringToElement("Add todo"))
        </button>
        <section className="main">
          <input className="toggle-all" onChange=Js.log />
          <ul className="todo-list">
            (
              List.map(
                todo => <div> <span> (Didact.stringToElement(todo.title)) </span> </div>,
                self.state.todos
              )
              |> Didact.listToElement
            )
          </ul>
        </section>
      </div>
  });