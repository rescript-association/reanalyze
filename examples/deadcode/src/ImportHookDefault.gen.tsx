/* TypeScript file generated from ImportHookDefault.res by genType. */
/* eslint-disable import/first */


import {default as makeNotChecked} from './hookExample';

import {default as defaultNotChecked} from './hookExample';

// In case of type error, check the type of 'make' in 'ImportHookDefault.re' and './hookExample'.
export const makeTypeChecked: (_1:unknown) => JSX.Element = makeNotChecked;

// Export 'make' early to allow circular import from the '.bs.js' file.
export const make: unknown = makeTypeChecked as (_1:unknown) => JSX.Element;

// In case of type error, check the type of 'default' in 'ImportHookDefault.re' and './hookExample'.
export const defaultTypeChecked: (_1:unknown) => JSX.Element = defaultNotChecked;

// Export '$$default' early to allow circular import from the '.bs.js' file.
export const $$default: unknown = defaultTypeChecked as (_1:unknown) => JSX.Element;

export default $$default;
