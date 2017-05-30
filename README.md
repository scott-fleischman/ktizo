# κτίζω (ktizo)
Update GitHub dependencies in Haskell `stack.yaml` files.

## Installation
* Clone repository.
* `stack install` to install

## Usage
* Set `GITHUB_TOKEN` environment variable to your token to access the GitHub API.
* `ktizo PATH` where `PATH` is a directory with `stack.yaml` or is a path to a stack file.
* Use `ktizo --branch=BRANCH PATH` to first try `BRANCH` then fallback to `master` when querying for commits. Without the `--branch` option it only queries for the `master` branch commit.
