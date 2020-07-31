open Common;

type t = {
  loc: Location.t,
  path: Path.t,
};
/* Keep track of the module path while traversing with Tast_mapper */
let current: ref(t) = ref({loc: Location.none, path: []});

let getCurrent = () => current^;

let setCurrent = p => current := p;
