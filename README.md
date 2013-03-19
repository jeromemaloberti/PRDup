Github Pull Request Duplicator
==============================

Xen-Org's workflow requires us to to first commit new features and bug fixes
into master, and then backport them into product branches later. This tool
duplicates pull requests for us, so we don't have to manually recreate pull
requests for product branches. It doesn't do anything "smart" when it
cherry-picks commits, and will fail if a cherry-pick doesn't apply cleanly.
