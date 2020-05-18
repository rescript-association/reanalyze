exception Json_error(string);

module Basic = {
  type t;

  [@raises Json_error]
  let from_string: string => t = _ => raise(Json_error("Basic.from_string"));
};