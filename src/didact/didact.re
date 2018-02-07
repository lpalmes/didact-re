type sideEffects = unit => unit;

type stateless = unit;

type actionless = unit;

module Callback = {
  type t('payload) = 'payload => unit;
  let default = _event => ();
  let chain = (handlerOne, handlerTwo, payload) => {
    handlerOne(payload);
    handlerTwo(payload);
  };
};

type props = {
  id: option(string),
  value: option(string),
  className: option(string),
  placeholder: option(string),
  onClick: option(Dom.event => unit),
  onKeyDown: option(Dom.event => unit),
  onChange: option(Dom.event => unit)
};

type reduce('payload, 'action) = ('payload => 'action) => Callback.t('payload);

type update('state, 'action) =
  | NoUpdate
  | Update('state)
and self('state, 'action) = {
  state: 'state,
  reduce: 'payload .reduce('payload, 'action),
  send: 'action => unit
};

type didactElementType =
  | Text(string)
  | Node(string)
  | Component(component('state, 'action)): didactElementType
and didactElement = {
  elementType: didactElementType,
  props,
  children: list(didactElement)
}
and componentSpec('state, 'initialState, 'action) = {
  debugName: string,
  render: self('state, 'action) => didactElement,
  initialState: unit => 'initialState,
  reducer: ('action, 'state) => update('state, 'action)
}
and component('state, 'action) = componentSpec('state, 'state, 'action);

let defaultProps = {
  id: None,
  value: None,
  placeholder: None,
  className: None,
  onClick: None,
  onChange: None,
  onKeyDown: None
};

let createDomElement =
    (
      name,
      ~onClick: option(Dom.event => unit)=?,
      ~onChange: option(Dom.event => unit)=?,
      ~onKeyDown: option(Dom.event => unit)=?,
      ~id: option(string)=?,
      ~value: option(string)=?,
      ~className: option(string)=?,
      ~placeholder: option(string)=?,
      ~children: list(didactElement),
      _: unit
    ) => {
  elementType: Node(name),
  props: {
    className,
    onClick,
    onChange,
    onKeyDown,
    placeholder,
    value,
    id
  },
  children
};

let basicComponent = debugName : componentSpec(_, _, _) => {
  debugName,
  render: _self => assert false,
  initialState: () => (),
  reducer: (_action, _state) => NoUpdate
};

let statelessComponent = debugName => {
  ...basicComponent(debugName),
  initialState: () => ()
};

let statefulComponent = debugName => basicComponent(debugName);

let reducerComponent = debugName => basicComponent(debugName);

let stringToElement = value => {
  elementType: Text(value),
  props: defaultProps,
  children: []
};

let listToElement = elements => {
  elementType: Node("div"),
  props: defaultProps,
  children: elements
};

let nullElement = {
  elementType: Node("div"),
  props: defaultProps,
  children: []
};

let component = component => {
  elementType: Component(component),
  props: defaultProps,
  children: []
};