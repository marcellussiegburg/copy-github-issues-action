# Copy github issues action

This action copies all latest issues from one repository to another.

## Inputs

### `source_token`

The token of the source repository

### `source_user`

The user as which to authenticate in order to use the `source_token`

defaults to `source_owner`

### `source_owner`

The owner of the source repository

defaults to the owner of `GITHUB_REPOSITORY`

### `source_repository`

The source repositories name

defaults to the repository name of `GITHUB_REPOSITORY`

### `target_token`

The token of the target repository

defaults to `source_token`

### `target_user`

The user as which to authenticate in order to use the target_token

defaults to `source_user`

### `target_owner`

The owner of the target repository

defaults to `source_owner`

### `target_repository`

The target repositories name

deaults to `source_repository`

### `force_issue_number`

Whether to force synchronising issue numbers when copying

default: `true`

### `copy_all_missing_issues`

Whether to copy all missing intermediate issues

default: `false`

### `close_issue`

Whether to close the issue copy

default: `false`

### `reference_old_issue`

Whether to reference the old issue within the new issue

default: `false`

### `issue_title`

Use this issue title for copied issues

default: `'$(issueTitle)'`

`$(issueTitle)` interpolates the title of the original issue.

### `issue_body`

Use this issue body for copied issues

default: `'$(issueBody)'`

`$(issueBody)` interpolates the body text of the original issue.

### `deleted_issue_title`

Use this issue title for issues for which there does not exist a number any more

default: `'GONE'`

### `deleted_issue_body`

Use this issue body for copied issues for which there does not exist a number any more

defaults to an empty body

## Example usage

Be sure to set appropiate access tokens for repositories that are not the current repository.
Otherwise you may use `${{ GITHUB_TOKEN }}`.

Copy issues everytime a new issue is created (attention: this can potentially lead to a cycle):

```YAML
name: Copy Issues
on:
  issues:
    types:
      - opened
jobs:
  copy-issue:
    runs-on: ubuntu-latest
    steps:
      - uses: fmidue/copy-github-issues-action@master
        with:
          source_token: ${{ secrets.SOURCE_PASSWORD }}
          source_user: ${{ secrets.SOURCE_USERNAME }}
          target_repository: 'somewhere'
          close_issue: true
          reference_old_issue: true
          force_issue_number: false
```

```YAML
name: Copy Issues
on:
  issues:
    types:
      - opened
jobs:
  copy-issue:
    runs-on: ubuntu-latest
    steps:
      - uses: fmidue/copy-github-issues-action@master
        with:
          source_token: ${{ secrets.PRIVATE_PASSWORD }}
          source_user: ${{ secrets.PRIVATE_USERNAME }}
          target_repository: 'internal'
          close_issue: false
          reference_old_issue: true
          force_issue_number: true
          copy_all_missing_issues: true
          issue_title: 'PUBLIC: $(issueTitle)'
```
