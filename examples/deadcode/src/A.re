module M: {
  type t =
    | A;
} = {
  module T = {
    type t =
      | A;
  };
  include T;

  Js.log(A);
};
