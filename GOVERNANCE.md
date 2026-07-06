# Governance

This document describes how the Cardano Node Emulator project is governed and
how decisions are made.

## Overview

The Cardano Node Emulator is an open-source project maintained under the
[IntersectMBO](https://github.com/IntersectMBO) organisation. The project
follows the practices set out in the
[Cardano Engineering Handbook](https://input-output-hk.github.io/cardano-engineering-handbook).

## Roles

### Contributors

Anyone who submits an issue, pull request, or otherwise participates in the
project is a contributor. Contributions are welcome from everyone; please read
[CONTRIBUTING.md](CONTRIBUTING.md) before opening a pull request.

### Maintainers

Maintainers are responsible for reviewing and merging contributions, triaging
issues, guiding the technical direction, and cutting releases. The current
maintainers are listed in [CODEOWNERS](CODEOWNERS).

Maintainers are expected to:

- Review and respond to issues and pull requests.
- Uphold the [Code of Conduct](CODE_OF_CONDUCT.md).
- Make and communicate decisions about the project's direction.
- Approve and merge changes in accordance with the process below.

## Decision-Making

Decisions are made by consensus among the maintainers wherever possible.

- **Everyday changes** (bug fixes, documentation, small improvements) are
  handled through the normal pull request review process. A change may be merged
  once it has approval from at least one maintainer who is a code owner, as
  enforced by [CODEOWNERS](CODEOWNERS), and all required checks pass.
- **Significant changes** (public API changes, new components, architectural
  shifts) should first be discussed in a GitHub issue so that maintainers and
  the community can weigh in before implementation begins.
- **Disagreements** that cannot be resolved by consensus are decided by the
  maintainers. In the event of a deadlock, the code owner has the final say.

## Contribution Process

1. Open an issue to discuss substantial changes before starting work.
2. Fork the repository and create a branch for your change.
3. Follow the development and coding guidelines in [CONTRIBUTING.md](CONTRIBUTING.md).
4. Open a pull request using the provided
   [pull request template](.github/PULL_REQUEST_TEMPLATE.md).
5. Address review feedback until the change is approved by a code owner.

## Releases

Releases are cut by the maintainers. Note that this is currently pre-release
software; there is no fixed release cadence, and interfaces may change.

## Code of Conduct

All participants are expected to abide by the project's
[Code of Conduct](CODE_OF_CONDUCT.md).

## Security

Please report security vulnerabilities as described in our
[Security Policy](SECURITY.md) rather than through public issues.

## Changes to This Document

Changes to this governance document follow the same pull request and review
process as any other change to the repository and require maintainer approval.
