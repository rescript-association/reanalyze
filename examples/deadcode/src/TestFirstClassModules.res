@genType
let convert = (x: FirstClassModules.firstClassModule) => x

@genType
let convertInterface = (x: FirstClassModulesInterface.firstClassModule) => x

@genType
let convertRecord = (x: FirstClassModulesInterface.record) => x

module type MT = {
  type outer
  let out: outer => outer

  module Inner: {
    type inner
    let inn: inner => inner
  }
}

@genType
type firstClassModuleWithTypeEquations<'i, 'o> = module(MT with
  type Inner.inner = 'i
  and type outer = 'o
)

@genType
let convertFirstClassModuleWithTypeEquations = (
  type o i,
  x: module(MT with type Inner.inner = i and type outer = o),
) => x

@genType
let convertFirstClassModuleWithTypeEquationsUsingAlias = (
  type o i,
  x: firstClassModuleWithTypeEquations<i, o>,
) => x