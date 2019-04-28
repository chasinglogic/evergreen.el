
# Table of Contents

1.  [evergreen.el &#x2013; Interact with Evergreen via Emacs](#orge62bd65)
    1.  [Features](#org65d94fa)
    2.  [Installation](#org85c50f3)
    3.  [Usage](#orgd6c3c49)
        1.  [Managing Spawn Hosts](#orgae837f5)
        2.  [Submitting Patch Builds](#orga9bfd73)
        3.  [Configuration](#org5dea9b4)
        4.  [Example Configuration](#orgaa21493)
    4.  [License](#org4f97a8c)


<a id="orge62bd65"></a>

# evergreen.el &#x2013; Interact with Evergreen via Emacs

The Evergreen CLI has a lot of cool features that I rarely remember
to use and trying remember the exact incantation / build
variants that I want is always requires cross-referencing the
evergreen.yml at minimum 5 times.

So I spent some time to write up this Emacs interaction package. It
uses the Evergreen CLI to expose a better UI for submitting patches
and other Evergreen stuff too.


<a id="org65d94fa"></a>

## Features

-   Fuzzy completion of projects, tasks, variants, and aliases when creating a patch build.
-   Exposes most useful boolean flags as yes or no prompts.
-   Can generate automatic patch descriptions
-   Very configurable via customize, many things can be defaulted to avoid prompts


<a id="org85c50f3"></a>

## Installation

The easiest installation method is [quelpa](https://framagit.org/steckerhalter/quelpa) to install direct from
the git repository. Follow their installation instructions then you
can add the following to your emacs init file to install
evergreen.el:

    (quelpa '(evergreen :repo "chasinglogic/evergreen.el" :fetcher github))

Alternatively you can install directly using git. First clone this
repository to your local machine (update the path as necessary):

    $ git clone https://github.com/chasinglogic/evergreen.el ~/.evergreen.el

Now add the following line to your Emacs init file:

    (load-file "~/.evergreen.el/evergreen.el")


<a id="orgd6c3c49"></a>

## Usage


<a id="orgae837f5"></a>

### Managing Spawn Hosts

evergreen.el provides an integrated experience with Evergreen
Spawn hosts.

To list your current spawn hosts you can do so via the command
`M-x evergreen-list-spawn-hosts <RET>`. This will open up the
Evergreen Spawn Host menu buffer.

![img](https://raw.githubusercontent.com/evergreen-ci/evergreen.el/master/screenshots/empty_spawn_host_list.png)

You can see that there is a "Spawn Host" button, if you click on
or press `<RET>` while the cursor is over this button you will be
interactively prompted to create a spawn host. You will receive a
fuzzy searchable list of available distros:

![img](https://raw.githubusercontent.com/evergreen-ci/evergreen.el/master/screenshots/spawning_host.png)

Once the host has started spawning you can call `M-x revert-buffer
    <RET>` inside the Spawn Host menu buffer to update it like any
other buffer in Emacs:

![img](https://raw.githubusercontent.com/evergreen-ci/evergreen.el/master/screenshots/host_provisioning.png)

Note that the Spawn Host id is highlighted as a button. If you
press `<RET>` with the cursor on the ID or simply click it Emacs
will remote into the spawn host using [TRAMP mode](https://www.emacswiki.org/emacs/TrampMode):

![img](https://raw.githubusercontent.com/evergreen-ci/evergreen.el/master/screenshots/host_opened_in_emacs.png)

Important! Tramp can hang weirdly if you are connecting to a host
for the first time (due to host key checking prompts it doesn't
always forward correctly). For a smooth experience with AWS
instances, which spawn hosts are, I recommend adding the following
to your `~/.ssh/config` file:

    Host ec2-*
        StrictHostKeyChecking no

You can use the Terminate button in this buffer just like the
Spawn Host button to, you guessed it, terminate the host.


<a id="orga9bfd73"></a>

### Submitting Patch Builds

evergreen.el allows you to submit patch builds via the command `M-x
   evergreen-patch <RET>` when visiting any file in the repository you
wanna patch against.

You will then be prompted for all required information to run a
patch build. All information that requires knowledge of the
`evergreen.yml` or information Evergreen provides via the
`evergreen list` command will be presented as a fuzzy searchable
completion list. If you have [helm](https://github.com/emacs-helm/helm), [ivy](https://github.com/abo-abo/swiper), or `ido` completion turned
on it will use those packages.

See [1.3.3](#org5dea9b4) below for information about available options and
avoiding unnecessary prompts.


<a id="org5dea9b4"></a>

### Configuration

This is best effort customization documentation. The best way to
discover available options for your installed version is always
via `M-x customize-group <RET> evergreen <RET>`.

1.  evergreen-spawn-key-name

    The key pair name to use when spawning a host. Will be prompted
    for when this is not set.

2.  evergreen-projects

    List of evergreen projects used for completion in evergreen.el
    commands.  Defaults to the result of `evergreen list --projects`.

3.  evergreen-default-project

    When prompting for project completion select the item matching
    this first. If it is nil and projectile is installed then the
    default selection will be the result of
    `(projectile-project-name)`. This works for most projects besides
    `mongodb/mongo`. If you want automatic defaulting to the correct
    `mongodb/mongo` branch then add the following to your emacs init
    file.
    
        (defun my-find-file-hook ()
          "Set evergreen-default-project appropriately for mongodb mongo."
          (when (eq (projectile-project-name "mongo"))
            (setq-local evergreen-default-project (format "mongodb-mongo-%s" (evergreen--branch-name)))))
    
    This will set evergreen-default-project to the right value
    whenever you open a file in the mongo project. It will not update
    when switching branches and visiting a file that was open before
    checking out the new branch.

4.  evergreen-default-alias

    When prompting for alias completion select the item matching
    this first.

5.  evergreen-command-output-buffer

    Buffer to output evergreen stdout to. Defaults to `"*evergreen command output*"`.

6.  evergreen-binary-path evergreen

    Location of evergreen binary, assumes evergreen is in your $PATH.

7.  evergreen-assume-yes

    If not nil then the Evergreen CLI will not do any additional prompting.

8.  evergreen-browse-when-patching

    Whether or not to open a patch in your browser after creation.

9.  evergreen-finalize-when-patching

    If not nil schedule every patch right away.

10. evergreen-never-finalize-when-patching

    If not nil never finalize and do not prompt for finalize when
    patching.  This option is ignored if
    `evergreen-finalize-when-patching` is non-nil.

11. evergreen-browse-when-patching

    If not nil always open new patches in your web browser after submitting.

12. evergreen-never-browse-when-patching

    If not nil never browse and do not prompt for browse when patching.
    This option is ignored if `evergreen-browse-when-patching` is
    non-nil.

13. evergreen-generate-description

    If not nil generates patch descriptions of form `$git_branch_name: $git_head_commit_msg`.


<a id="orgaa21493"></a>

### Example Configuration

This is my configuration as it provides minimum prompts and
maximum automation. Put this in your Emacs init file after loading
evergreen.el:

    (setq 
      evergreen-spawn-key-name "mykeypair"
      evergreen-generate-description t
      evergreen-finalize-when-patching t
      evergreen-browse-when-patching t
      evergreen-assume-yes t)

If you only work on a known set of projects and don't want the
full list of evergreen projects everytime then set the
`evergreen-projects` variable to shorten the list.

    (setq evergreen-projects '("mongodb-mongo-master" "toolchain-builder" "mongodb-mongo-v3.6" "mongodb-mongo-v4.0")
          evergreen-default-project "mongodb-mongo-master")


<a id="org4f97a8c"></a>

## License

`evergreen.el` is licensed under the GPLv3&#x2026; because Emacs.

    evergreen.el -- Interact with Evergreen via Emacs
    Copyright (C) 2019  Mathew Robinson
    
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    
    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

