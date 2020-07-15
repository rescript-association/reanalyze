let fs = require("fs");
let cp = require("child_process");
let path = require("path");

let usage = `Missing command, usage: node syntaxUpgrade.js <command>
Options are:
 -migrate Update your project to ReScript
 -rollback Restore a backup

Additional options through env vars:
  RESCRIPT_EXE Location of rescript binary, defaults to rescript in node_modules
`;

let command = "";

if (process.argv.length < 3) {
  console.error(usage);
  process.exit(1);
}

// poor man's arg parser
if (process.argv[2].indexOf("rollback") > -1) {
  command = "rollback";
} else if (process.argv[2].indexOf("migrate") > -1) {
  command = "migrate";
} else {
  console.error(usage);
  process.exit(1);
}

let bsconfig = "bsconfig.json";
let sourcedirs = ".sourcedirs.json";
let projectRoot = "";

if (!projectRoot) {
  let dir = process.cwd();

  while (true) {
    if (fs.existsSync(path.join(dir, bsconfig))) {
      projectRoot = dir;
      break;
    } else {
      let parent = path.dirname(dir);
      if (parent === dir) {
        console.error(
          "Error: cannot find your project root containing bsconfig.json"
        );
        process.exit(1);
      } else {
        dir = parent;
      }
    }
  }
}

let dirs = [];

if (fs.existsSync(path.join(projectRoot, "lib", "bs", sourcedirs))) {
  let sourceDirsJson = JSON.parse(
    fs.readFileSync(path.join(projectRoot, "lib", "bs", sourcedirs)).toString()
  );

  dirs = sourceDirsJson["dirs"];
} else {
  console.error(
    "Error: cannot find your project's source directories, would it be possible to run bsb -make-world first?"
  );
  process.exit(1);
}

let rescriptExe = "";

if (process.env.RESCRIPT_EXE) {
  if (fs.existsSync(process.env.RESCRIPT_EXE)) {
    rescriptExe = process.env.RESCRIPT_EXE;
  } else {
    console.error(
      `Error: RESCRIPT_EXE(${process.env.RESCRIPT_EXE}) does not seem to exist. Provide a valid location.`
    );
    process.exit(1);
  }
} else if (
  fs.existsSync(path.join(projectRoot, "node_modules", ".bin", "rescript.exe"))
) {
  rescriptExe = path.join(projectRoot, "node_modules", ".bin", "rescript.exe");
} else {
  console.error(
    "Error: could not find rescript.exe in node_modules/.bin. Did you install it?\nAlternatively your can provide the location with the env var RESCRIPT_EXE"
  );
  process.exit(1);
}

let refmtExe = "";

if (
  fs.existsSync(path.join(projectRoot, "node_modules", ".bin", "bsrefmt"))
) {
  refmtExe = path.join(projectRoot, "node_modules", ".bin", "bsrefmt");
} else {
  console.error(
    "Error: could not find refmt.exe in node_modules/.bin. Did you npm install bs-platform?\n"
  );
  process.exit(1);
}

function isInterface(filename) {
  if (filename.length === 0) {
    return false;
  } else {
    return filename[filename.length - 1] === "i";
  }
}

function parseBinaryReason(filename) {
  let intf = isInterface(filename);
  let args = ["--print", "binary"];

  if (intf) {
    args.push("--interface");
    args.push("true");
  }

  args.push(filename);

  return cp.spawnSync(refmtExe, args).stdout;
}

let processFileCount = 0;

dirs.forEach((dir) => {
  let files = fs.readdirSync(path.join(projectRoot, dir));
  files
    .filter((filename) => {
      let ext = path.extname(filename);
      if (command === "migrate") {
        return ext === ".re" || ext === ".rei";
      } else if (command === "rollback") {
        return ext === ".backup";
      } else {
        return false;
      }
    })
    .forEach((filename) => {
      let filePath = path.join(projectRoot, dir, filename);

      if (command === "rollback") {
        // Foo.re.backup -> Foo.re
        let newFilename = filename.replace(".backup", "");
        let isInterface = path.extname(newFilename) === ".rei";
        // remove .backup extension, /project/src/Foo.re.backup -> /project/src/Foo.re
        fs.renameSync(filePath, path.join(projectRoot, dir, newFilename));
        // Foo.re -> Foo
        let base = path.basename(newFilename, isInterface ? ".rei" : ".re");
        // Foo -> Foo.res or Foo -> Foo.resi
        let rescriptFilename = base + (isInterface ? ".resi" : ".res");
        // project/src/Foo.res
        let rescriptFilePath = path.join(projectRoot, dir, rescriptFilename);
        // remove project/src/Foo.res when exists
        if (fs.existsSync(rescriptFilePath)) {
          fs.unlinkSync(rescriptFilePath);
        }
      } else {
        let reasonBinary = parseBinaryReason(filePath);

        let args = ["-parse", "reasonBinary", "-print", "ns"];

        let isInterface = path.extname(filename) === ".rei";

        if (isInterface) {
          args.push("-interface");
        }

        let rescriptContent = cp
          .spawnSync(rescriptExe, args, { input: reasonBinary })
          .stdout.toString();
        fs.writeFileSync(
          isInterface
            ? filePath.replace(".rei", ".resi")
            : filePath.replace(".re", ".res"),
          rescriptContent
        );
        fs.renameSync(filePath, filePath + ".backup");
      }
      processFileCount++;
    });
});

if (command === "migrate") {
  console.log(`All done! Migrated ${processFileCount.toString()} files.
If there's any change to your build or artifacts, please check and file us an issue.
Apologies in advance; it's a big refactor.
Thank you!
`);
} else if (command === "rollback") {
  console.log(`All done! Restored ${processFileCount.toString()} backups.
If you encountered any issues, please check and file us an issue.
Apologies in advance; it's a big refactor.
Thank you!
`);
}
