/*
We need this script to consistently run BSB on Unix & Windows based systems
*/

const fs = require("fs");
const child_process = require("child_process");
const path = require("path");

const spawn = child_process.spawn;

const isWindows = /^win/i.test(process.platform);

const input = (args = process.argv.slice(2));

const shell = isWindows ? true : false;

try {
  fs.unlinkSync("../lazyLoad.exe");
} catch (err) {}

fs.symlink(
  "../_esy/default/build/install/default/bin/lazyLoad.exe",
  "../lazyLoad.exe",
  function (err) {
    console.log(err || "Done.");
  }
);

try {
  fs.unlinkSync("../DeadCodePPX.exe");
} catch (err) {}

fs.symlink(
  "../_esy/default/build/install/default/bin/DeadCodePPX.exe",
  "../DeadCodePPX.exe",
  function (err) {
    console.log(err || "Done.");
  }
);

try {
  fs.unlinkSync("../reanalyze.exe");
} catch (err) {}

fs.symlink(
  "../_esy/default/build/install/default/bin/reanalyze.exe",
  "../reanalyze.exe",
  function (err) {
    console.log(err || "Done.");
  }
);

spawn("bsb", input, { stdio: ["inherit", "inherit"], shell }).on(
  "exit",
  (code) => process.exit(code)
);
