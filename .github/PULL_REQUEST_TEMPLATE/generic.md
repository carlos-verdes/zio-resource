# Your checklist for this pull request
🚨Please review the [guidelines for contributing](../../CONTRIBUTING.md) to this repository.

- [ ] Make sure you are requesting to merge of a branch created from main (do not start your branch of develop);
- [ ] Make sure you are requesting to **pull a non develop or main** branch (right side). Don't request your develop or main!
- [ ] Make sure you are making a pull request against the **develop** branch (left side);
- [ ] Check the commit message is  **PR title** is [Conventional Commit Compliant](https://github.com/OlegEfrem/delivery-pipeline#how); 
- [ ] Check your code additions will fail neither code linting checks nor unit test.

# PR title
## Format
[type]: add some message - closes #[github issue number] [pull request number]
## Type
Must be one of the following:
* build: Changes that affect the build system or external dependencies;
* ci: Changes to our CI configuration files and scripts (examples: GitHub Actions);
* docs: Documentation only changes;
* feat: A new feature;
* fix: A bug fix;
* perf: A code change that improves performance;
* refactor: A code change that neither fixes a bug nor adds a feature;
* test: Adding missing tests or correcting existing tests;

## Examples: 
* For bug fix:
  * fix: correct reference to external resource - closes #10 #2
* For feature: 
  * feat: add new awesome feature - closes #11 #3
* For breaking change: 
  * break: introduce a horrible breaking change - closes #12 #4
