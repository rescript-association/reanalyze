// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

import * as List from "bs-platform/lib/es6/list.js";
import * as React from "react";
import * as $$String from "bs-platform/lib/es6/string.js";
import * as ReasonReact from "reason-react/src/ReasonReact.js";
import * as ImportMyBanner from "./ImportMyBanner.bs.js";

var component = ReasonReact.statelessComponent("ReasonComponent");

function onClick(param) {
  console.log("click");
  
}

function make(messageOpt, person, intListOpt, _children) {
  var message = messageOpt !== undefined ? messageOpt : "default message";
  var intList = intListOpt !== undefined ? intListOpt : /* :: */[
      0,
      /* [] */0
    ];
  return {
          debugName: component.debugName,
          reactClassInternal: component.reactClassInternal,
          handedOffState: component.handedOffState,
          willReceiveProps: component.willReceiveProps,
          didMount: component.didMount,
          didUpdate: component.didUpdate,
          willUnmount: component.willUnmount,
          willUpdate: component.willUpdate,
          shouldUpdate: component.shouldUpdate,
          render: (function (_self) {
              return React.createElement("div", {
                          className: "App",
                          onClick: onClick
                        }, "ReasonReact " + (message + (" and intList: " + ($$String.concat(",", List.map((function (i) {
                                          return String(i);
                                        }), intList)) + (" and person name: " + person.name)))), ReasonReact.element(undefined, undefined, ImportMyBanner.make(true, {
                                  text: "this is from ReasonComponent"
                                }, [])));
            }),
          initialState: component.initialState,
          retainedProps: component.retainedProps,
          reducer: component.reducer,
          jsElementWrapped: component.jsElementWrapped
        };
}

function minus(firstOpt, second) {
  var first = firstOpt !== undefined ? firstOpt : 0;
  return first - second | 0;
}

function useTypeDefinedInAnotherModule(x) {
  return x;
}

function tToString(t) {
  if (typeof t === "number") {
    return "A";
  } else if (t.tag) {
    return "C(" + (t[0] + ")");
  } else {
    return "B(" + (String(t[0]) + ")");
  }
}

function useRecordsCoord(param) {
  return param.x + param.y | 0;
}

export {
  component ,
  onClick ,
  make ,
  minus ,
  useTypeDefinedInAnotherModule ,
  tToString ,
  useRecordsCoord ,
  
}
/* component Not a pure module */
