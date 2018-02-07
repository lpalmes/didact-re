// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
'use strict';

var Block                   = require("bs-platform/lib/js/block.js");
var Curry                   = require("bs-platform/lib/js/curry.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

function $$default() {
  return /* () */0;
}

function chain(handlerOne, handlerTwo, payload) {
  Curry._1(handlerOne, payload);
  return Curry._1(handlerTwo, payload);
}

var Callback = /* module */[
  /* default */$$default,
  /* chain */chain
];

var defaultProps = /* record */[
  /* id : None */0,
  /* value : None */0,
  /* onClick : None */0,
  /* onChange : None */0
];

function createDomElement(name, onClick, children, _) {
  return /* record */[
          /* elementType : Node */Block.__(1, [name]),
          /* props : record */[
            /* id : None */0,
            /* value : None */0,
            /* onClick */onClick,
            /* onChange : None */0
          ],
          /* children */children
        ];
}

function basicComponent(debugName) {
  return /* record */[
          /* debugName */debugName,
          /* render */(function () {
              throw [
                    Caml_builtin_exceptions.assert_failure,
                    [
                      "didact.re",
                      70,
                      19
                    ]
                  ];
            }),
          /* initialState */(function () {
              return /* () */0;
            }),
          /* reducer */(function (_, _$1) {
              return /* NoUpdate */0;
            })
        ];
}

function statelessComponent(debugName) {
  var init = basicComponent(debugName);
  return /* record */[
          /* debugName */init[/* debugName */0],
          /* render */init[/* render */1],
          /* initialState */(function () {
              return /* () */0;
            }),
          /* reducer */init[/* reducer */3]
        ];
}

var statefulComponent = basicComponent;

var reducerComponent = basicComponent;

function stringToElement(value) {
  return /* record */[
          /* elementType : Text */Block.__(0, [value]),
          /* props */defaultProps,
          /* children : [] */0
        ];
}

function component(component$1) {
  return /* record */[
          /* elementType : Component */Block.__(2, [component$1]),
          /* props */defaultProps,
          /* children : [] */0
        ];
}

exports.Callback           = Callback;
exports.defaultProps       = defaultProps;
exports.createDomElement   = createDomElement;
exports.basicComponent     = basicComponent;
exports.statelessComponent = statelessComponent;
exports.statefulComponent  = statefulComponent;
exports.reducerComponent   = reducerComponent;
exports.stringToElement    = stringToElement;
exports.component          = component;
/* No side effect */