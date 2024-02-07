const path = require("path");
const fs = require("fs");

const platformAndArch =
  process.arch === "x64" ? process.platform : process.platform + process.arch;

function fail(msg) {
  console.warn(msg);
  process.exit(1);
}

function movePlatformBinary() {
  const binDirName = "vendor-" + platformAndArch;
  const sourcePath = path.join(__dirname, binDirName, "reanalyze.exe");

  if (!fs.existsSync(sourcePath)) {
    return fail("error: executable not found: " + sourcePath);
  }

  // We always use the .exe extension, no matter if unix / win32
  const targetPath = path.join(__dirname, "reanalyze.exe");

  fs.renameSync(sourcePath, targetPath);
  fs.chmodSync(targetPath, 0777);
}

switch (platformAndArch) {
  case "win32":
  case "linux":
  case "darwin":
  case "darwinarm64":
    movePlatformBinary();
    break;
  default:
    fail("error: no release built for the " + platform + " platform");
}
