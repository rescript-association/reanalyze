const fs = require("fs");
const child_process = require("child_process");
const path = require("path");

const spawn = child_process.spawn;

const isWindows = /^win/i.test(process.platform);

const reanalyzeNativePath = path.join(
  __dirname,
  "../../_esy/default/build/install/default/bin/reanalyze.exe"
);

const shell = isWindows ? true : false;

child_process.spawnSync(
  reanalyzeNativePath,
  [
    "-dce",
    "-debug",
    "-blacklist",
    "src/exception",
    "-live-names",
    "globallyLive1",
    "-live-names",
    "globallyLive2,globallyLive3",
    "-live-paths",
    path.join(__dirname, "src/EverythingLiveHere.re"),
    "-live-paths",
    path.join(__dirname, "src/Hooks.re"),
  ],
  {
    stdio: ["inherit", "inherit"],
    shell,
  }
);
