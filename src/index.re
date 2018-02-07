open DidactElements;

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
        </div>
    });
};

open Bs_webapi.Dom;

switch (Document.getElementById("container", Bs_webapi.Dom.document)) {
| Some(dom) => DidactDom.render(<App />, dom) |> ignore
| None => print_endline("No dom element found :(")
};