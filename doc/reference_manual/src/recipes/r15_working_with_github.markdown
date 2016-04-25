# Recipe #15: working with GitHub

We use GitHub (https://github.com) for hosting an alternate copy of the `mossco/code` repository.  Development on GitHub differs from SourceForge's approach

1. users "fork" existing projects into their own user space on GitHub
2. users pull/push from and to their own GitHub repository.
3. users issue "pull requests", such that their changes can be pulled into the project they previously forked.

## Obtain a read-only copy of the repository

If you do not have a mossco/code repository, then clone from one of the existing GitHub repositories:

    git clone https://github.com/platipodium/mossco-code.git $MOSSCO_DIR

Alternatively, add one of the existing mossco GitHub repositories to your remotes:

    cd $MOSSCO_DIR
    git remote add github https://github.com/platipodium/mossco-code.git
    git pull github master

## Contribute to and develop MOSSCO

1. Sign up on GitHub https://github.com/join
2. Log into GitHub and navigate to an existing repository, like https://github.com/platipodium/mossco-code
3. Fork your own copy (upper right corner)
4. Clone/remote add your copy on GitHub on your local machine.
5. Pull from/push to your local copy
6. Issue a pull request
