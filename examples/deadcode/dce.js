const fs = require("fs");
const child_process = require("child_process");
const path = require("path");

const spawn = child_process.spawn;

const isWindows = /^win/i.test(process.platform);

const reanalyzeNativePath = path.join(
  __dirname,
  "../../_build/default/src/Reanalyze.exe"
);

const shell = isWindows ? true : false;

child_process.spawnSync(
  reanalyzeNativePath,
  [
    "-config",
    "-debug",
    "-ci",
    "-exclude-paths",
    "src/noalloc,src/exception",
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
