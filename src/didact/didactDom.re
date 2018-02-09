open Didact;

type nodeInstance = {
  element: didactElement,
  dom: Dom.element,
  childInstances: list(didactInstance)
}
and instance('state, 'action) = {
  component: component('state, 'action),
  iState: 'state,
  element: didactElement,
  childInstance: ref(option(didactInstance)),
  dom: ref(Dom.element),
  pendingStateUpdates: ref(list('state => update('state, 'action)))
}
and componentInstance =
  | Instance(instance('state, 'action)): componentInstance
and didactInstance =
  | NodeInstance(nodeInstance)
  | ComponentInstance(componentInstance);

module Reconciler = {
  let addProps = (domElement: Dom.element, prevProps: option(props), props: props) => {
    open Webapi.Dom;
    /* remove attributes */
    switch props.id {
    | Some(_) => ElementRe.removeAttribute("id", domElement)
    | None => ()
    };
    switch props.value {
    | Some(_) => ElementRe.removeAttribute("value", domElement)
    | None => ()
    };
    switch props.onClick {
    | Some(func) => Element.removeEventListener("click", func, domElement)
    | None => ()
    };
    switch props.onKeyDown {
    | Some(func) => Element.removeEventListener("keyDown", func, domElement)
    | None => ()
    };
    switch props.onChange {
    | Some(func) => Element.removeEventListener("input", func, domElement)
    | None => ()
    };
    switch props.className {
    | Some(_) => ElementRe.removeAttribute("class", domElement)
    | None => ()
    };
    switch props.placeholder {
    | Some(_) => ElementRe.removeAttribute("placeholder", domElement)
    | None => ()
    };
    /* add attributes */
    switch props.id {
    | Some(value) => ElementRe.setAttribute("id", value, domElement)
    | None => ()
    };
    switch props.value {
    | Some(value) => ElementRe.setAttribute("value", value, domElement)
    | None => ()
    };
    switch props.onClick {
    | Some(func) => Element.addEventListener("click", func, domElement)
    | None => ()
    };
    switch props.onKeyDown {
    | Some(func) => Element.addEventListener("keyDown", func, domElement)
    | None => ()
    };
    switch props.onChange {
    | Some(func) => Element.addEventListener("input", func, domElement)
    | None => ()
    };
    switch props.className {
    | Some(value) => ElementRe.setAttribute("class", value, domElement)
    | None => ()
    };
    switch props.placeholder {
    | Some(value) => ElementRe.setAttribute("placeholder", value, domElement)
    | None => ()
    };
  };
  let equalizeLists = (aList, bList) : (list(option('a)), list(option('b))) => {
    let aOptionList = List.map(a => Some(a), aList);
    let bOptionList = List.map(a => Some(a), bList);
    (aOptionList, bOptionList);
  };
  let domFromInstance = newInstance =>
    switch newInstance {
    | NodeInstance({dom}) => dom
    | ComponentInstance(Instance({dom})) => dom^
    };
  let rec instantiate = (element: didactElement) : didactInstance => {
    open Webapi.Dom;
    let instance =
      switch element.elementType {
      | Node(name) =>
        let node = Document.createElement(name, document);
        let childInstances = List.map(instantiate, element.children);
        List.iter(
          e =>
            switch e {
            | NodeInstance({dom}) =>
              let _ = Element.appendChild(dom, node);
              ();
            | ComponentInstance(Instance({dom})) =>
              let _ = Element.appendChild(dom^, node);
              ();
            },
          childInstances
        );
        NodeInstance({dom: node, element, childInstances});
      | Text(value) =>
        let dom = Document.createElement("span", document);
        Element.setInnerText(dom, value);
        NodeInstance({dom, element, childInstances: []});
      | Component(component) =>
        let instance = createInstance(Obj.magic(component), element);
        let self = createSelf(instance);
        let element = component.render(Obj.magic(self));
        let newInstance = instantiate(element);
        let Instance({dom, childInstance}) = instance;
        dom := domFromInstance(newInstance);
        childInstance := Some(newInstance);
        ComponentInstance(instance);
      };
    switch instance {
    | NodeInstance({dom}) => addProps(dom, None, element.props)
    | _ => ()
    };
    instance;
  }
  and reconcile =
      (
        parentDom: Dom.element,
        instance: option(didactInstance),
        didactElement: option(didactElement)
      ) => {
    let rec reconcilerImpl =
            (
              parentDom: Dom.element,
              instance: option(didactInstance),
              didactElement: option(didactElement)
            ) =>
      Webapi.Dom.(
        switch (instance, didactElement) {
        | (None, Some(didactElement)) =>
          let newInstance = instantiate(didactElement);
          Element.appendChild(domFromInstance(newInstance), parentDom);
          Some(newInstance);
        | (Some(instance), None) =>
          Element.removeChild(domFromInstance(instance), parentDom) |> ignore;
          None;
        | (
            Some(NodeInstance({dom, element: {elementType: Text(oldText)}} as instance)) as opInstance,
            Some({elementType: Text(newText)} as element)
          ) =>
          if (oldText == newText) {
            opInstance;
          } else {
            Element.setInnerText(dom, newText);
            Some(NodeInstance({...instance, element}));
          }
        | (Some(ComponentInstance(instance)), Some({elementType: Component(component)})) =>
          Js.log("Same component instance");
          let self = createSelf(Obj.magic(instance));
          let element = component.render(Obj.magic(self));
          let Instance({dom, childInstance}) = instance;
          childInstance := reconcilerImpl(parentDom, childInstance^, Some(element));
          switch childInstance^ {
          | Some(c) => dom := domFromInstance(c)
          | _ => ()
          };
          Some(ComponentInstance(instance));
        | (
            Some(
              NodeInstance(
                {element: {elementType: Node(oldValue), props: oldProps} as element, dom} as instance
              )
            ),
            Some({elementType: Node(newValue), props: newProps})
          )
            when oldValue == newValue =>
          addProps(dom, Some(oldProps), newProps);
          let childInstances = List.rev(reconcileChildren(instance, element));
          Some(NodeInstance({...instance, childInstances}));
        | (Some(instance), Some(didactElement)) =>
          let newInstance = instantiate(didactElement);
          Element.removeChild(domFromInstance(instance), parentDom) |> ignore;
          Element.appendChild(domFromInstance(newInstance), parentDom);
          Some(newInstance);
        | (None, None) => None
        }
      );
    reconcilerImpl(parentDom, instance, didactElement);
  }
  and reconcileChildren =
      (instance: nodeInstance, didactElement: didactElement)
      : list(didactInstance) => {
    let (a, b) = equalizeLists(instance.childInstances, didactElement.children);
    let childOptionalInstances = List.map2(reconcile(instance.dom), a, b);
    let instances =
      List.fold_left(
        (a, b) =>
          switch b {
          | Some(x) => [x, ...a]
          | None => a
          },
        [],
        childOptionalInstances
      );
    instances;
  }
  and createSelf = instance : self(_) => {
    let Instance(instance) = instance;
    Webapi.Dom.{
      state: Obj.magic(instance.iState),
      reduce: (payloadToAction, payload) => {
        let action = payloadToAction(payload);
        let stateUpdate = instance.component.reducer(Obj.magic(action));
        instance.pendingStateUpdates := [stateUpdate, ...instance.pendingStateUpdates^];
      },
      send: action => {
        let stateUpdate =
          switch (instance.component.reducer(Obj.magic(action), instance.iState)) {
          | NoUpdate => instance.iState
          | Update(newState) => newState
          };
        switch (Element.parentNode(instance.dom^)) {
        | Some(dom) =>
          reconcile(
            Obj.magic(dom),
            Some(ComponentInstance(Instance({...instance, iState: stateUpdate}))),
            Some(instance.element)
          )
          |> ignore
        | None => ()
        };
      }
    };
  }
  and createInstance = (component, element) : componentInstance => {
    let iState = component.initialState();
    Instance({
      component,
      dom: ref(Webapi.Dom.Document.createElement("span", Webapi.Dom.document)),
      element,
      childInstance: ref(None),
      iState,
      pendingStateUpdates: ref([])
    });
  };
};

let instance: ref(option(didactInstance)) = ref(None);

let render = (element, parentDom) => {
  let newInstance = Reconciler.reconcile(parentDom, instance^, Some(element));
  instance := newInstance;
  newInstance;
};

let hotUpdate = (element, parentDom, oldInstance) =>
  instance := Reconciler.reconcile(parentDom, oldInstance, Some(element));