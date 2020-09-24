// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Caml_exceptions from "bs-platform/lib/es6/caml_exceptions.js";

var Json_error = Caml_exceptions.create("Yojson.Json_error");

function from_string(param) {
  throw {
        RE_EXN_ID: Json_error,
        _1: "Basic.from_string",
        Error: new Error()
      };
}

var Type_error = Caml_exceptions.create("Yojson.Basic.Util.Type_error");

function member(_s, j) {
  throw {
        RE_EXN_ID: Type_error,
        _1: "Basic.Util.member",
        _2: j,
        Error: new Error()
      };
}

function to_int(param) {
  return 34;
}

function to_string(param) {
  return "";
}

var Util = {
  Type_error: Type_error,
  member: member,
  to_int: to_int,
  to_string: to_string
};

var Basic = {
  from_string: from_string,
  Util: Util
};

export {
  Json_error ,
  Basic ,
  
}
/* No side effect */
