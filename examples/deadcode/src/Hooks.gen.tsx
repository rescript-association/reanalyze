/* TypeScript file generated from Hooks.res by genType. */
/* eslint-disable import/first */


// tslint:disable-next-line:no-var-requires
const Curry = require('bs-platform/lib/es6/curry.js');

// tslint:disable-next-line:no-var-requires
const HooksBS = require('./Hooks.bs');

// tslint:disable-next-line:interface-over-type-literal
export type vehicle = { readonly name: string };

// tslint:disable-next-line:interface-over-type-literal
export type cb = (_1:{ readonly to: vehicle }) => void;

// tslint:disable-next-line:interface-over-type-literal
export type callback<input,output> = (_1:input) => output;

// tslint:disable-next-line:interface-over-type-literal
export type testReactContext = React.Context<number>;

// tslint:disable-next-line:interface-over-type-literal
export type testReactRef = { current: (null | number) };

// tslint:disable-next-line:interface-over-type-literal
export type testDomRef = React.Ref<unknown>;

export const $$default: (_1:unknown) => JSX.Element = HooksBS.default;

export default $$default;

export const anotherComponent: (_1:unknown) => JSX.Element = HooksBS.anotherComponent;

export const Inner_make: (_1:unknown) => JSX.Element = HooksBS.Inner.make;

export const Inner_anotherComponent: (_1:unknown) => JSX.Element = HooksBS.Inner.anotherComponent;

export const Inner_Inner2_make: (_1:unknown) => JSX.Element = HooksBS.Inner.Inner2.make;

export const Inner_Inner2_anotherComponent: (_1:unknown) => JSX.Element = HooksBS.Inner.Inner2.anotherComponent;

export const NoProps_make: (_1:unknown) => JSX.Element = HooksBS.NoProps.make;

export const functionWithRenamedArgs: (_1:{
  readonly to: vehicle; 
  readonly Type: vehicle; 
  readonly cb: cb
}) => string = function (Arg1: any) {
  const result = Curry._3(HooksBS.functionWithRenamedArgs, Arg1.to, Arg1.Type, function (Argto: any) {
      const result1 = Arg1.cb({to:Argto});
      return result1
    });
  return result
};

export const componentWithRenamedArgs: (_1:unknown) => JSX.Element = HooksBS.componentWithRenamedArgs;

export const makeWithRef: (_1:unknown, _2:(null | undefined | any)) => JSX.Element = function (Arg1: any, Arg2: any) {
  const result = Curry._2(HooksBS.makeWithRef, Arg1, Arg2);
  return result
};

export const testForwardRef: (_1:unknown) => JSX.Element = HooksBS.testForwardRef;

export const input: (_1:unknown) => JSX.Element = HooksBS.input;

export const polymorphicComponent: (_1:unknown) => JSX.Element = HooksBS.polymorphicComponent;

export const functionReturningReactElement: (_1:unknown) => JSX.Element = HooksBS.functionReturningReactElement;

export const RenderPropRequiresConversion_make: (_1:unknown) => JSX.Element = HooksBS.RenderPropRequiresConversion.make;

export const aComponentWithChildren: (_1:unknown) => JSX.Element = HooksBS.aComponentWithChildren;
