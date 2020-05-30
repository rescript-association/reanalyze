# Development

## Build reanalyze

```
# Installs all dependencies (if needed) and builds reanalyze
npm install
dune build
```

## Test reanalyze

Make sure to always run the tests before submitting any changes (CI usually takes
longer to give you feedback).

```
npm test
```

## Automated Releases (for Maintainers)

The project is compatible with the [`npm
version`](https://docs.npmjs.com/cli/version) workflow. After using the `npm
version [major|minor|patch|...]` command, npm will automatically tag the
current commit, bump all the necessary version numbers (also the number in
`src/Version.re`) and push it to the current remote branch.

**Here are the concrete commands to run:**

```
# Make sure to commit & push all current changes, the working branch should be clean
# and synced up with your remote branch

# Also make sure that your current branch is explicitly set to the relevant remote
# (`git push` instead of `git push origin master`)
git status

# For patches (0.0.X+1)
npm version patch

# For minor (0.X+1.0)
npm version minor

# For major (X+1.0.0)
npm version major
```

## Releasing to npm (Maintainers only)

The releasing mechanism downloads the platform dependent artifacts stored on
the Github releases tab, so make sure to first do an automated release as
stated above (`npm version ...`).

After the CIs are done releasing the built binaries, do following command on a
unix-like system (no Windows supported):

```
node scripts/download_dist.js
```

This will download the prepared npm package from the reanalyze Github releases
tab to the root of your project with the name `reanalyze-$version.tgz`. This is
a `tgz` file ready to be released to npm!

```
# Dry run for testing
npm publish reanalyze-*.tgz --dry-run

# Publish package as @latest
npm publish reanalyze-*.tgz

# Publish package with @beta tag
npm publish reanalyze-*.tgz --tag beta
```

Consult the [npm publish](https://docs.npmjs.com/cli/publish) documentation for more options.
In case you get an `ENEEDAUTH` error, use `npm adduser` and authenticate with your npm account first.
