open DidactElements;

[%%bs.raw {|
  import "todomvc-common/base.css";
|}];

type todo = {
  id: string,
  title: string,
  completed: bool
};

module Input = {
  let createElement = (~children, ()) =>
    Didact.component({
      debugName: "Input",
      initialState: () => 0,
      reducer: (action, state) => Didact.NoUpdate,
      render: _self => <div> <input onChange=Js.log /> </div>
    });
};

module TodoMVC = {
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
        | AddTodo(todo) =>
          Didact.Update({...state, todos: [todo, ...state.todos]})
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
              onChange=(e => Js.log(e))
            />
          </header>
          <button
            onClick=(
              e =>
                self.send(
                  AddTodo({
                    id: "blue",
                    title: "new todo added",
                    completed: false
                  })
                )
            )>
            (Didact.stringToElement("Add todo"))
          </button>
          (
            List.length(self.state.todos) != 0 ?
              <section className="main">
                <input className="toggle-all" onChange=Js.log />
                <ul className="todo-list">
                  (
                    List.map(
                      todo =>
                        <div>
                          <span> (Didact.stringToElement(todo.title)) </span>
                        </div>,
                      self.state.todos
                    )
                    |> Didact.listToElement
                  )
                </ul>
              </section> :
              Didact.nullElement
          )
        </div>
    });
};

module App = {
  type state = int;
  type action =
    | Increment
    | Decrement;
  let createElement = (~children, ()) =>
    Didact.component({
      debugName: "App",
      initialState: () => 0,
      reducer: (action, state) =>
        switch action {
        | Increment => Didact.Update(state + 1)
        | Decrement => Didact.Update(state - 1)
        },
      render: ({state, send}) =>
        <div>
          <button onClick=((_) => send(Increment))>
            (Didact.stringToElement("Increment"))
          </button>
          <h3> (Didact.stringToElement(state |> string_of_int)) </h3>
          <button onClick=((_) => send(Decrement))>
            (Didact.stringToElement("Decrement"))
          </button>
          <Input />
        </div>
    });
};

open Webapi.Dom;

switch (Document.getElementById("container", document)) {
| Some(dom) => DidactDom.render(<TodoMVC />, dom) |> ignore
| None => print_endline("No dom element found :(")
};