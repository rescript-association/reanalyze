exception Etoplevel;

module Inside = {
  exception Einside;
};

let eToplevel = Etoplevel;

let eInside = Inside.Einside;

Js.log(eInside);