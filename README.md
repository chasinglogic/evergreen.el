
# Table of Contents

1.  [evergreen.el &#x2013; Interact with Evergreen via Emacs](#org1b4c500)
    1.  [Features](#orgfb54d1c)
    2.  [Installation](#org3d8fa93)
    3.  [Usage](#org63ba17c)
        1.  [Configuration](#orgc13432a)
        2.  [Example Configuration](#org1293971)
    4.  [Future Plans](#orgb468983)
    5.  [License](#orgaebd803)


<a id="org1b4c500"></a>

# evergreen.el &#x2013; Interact with Evergreen via Emacs

The Evergreen CLI has a lot of cool features that I rarely remember
to use and trying remember the exact incantation / build
variants that I want is always requires cross-referencing the
evergreen.yml at minimum 5 times.

So I spent some time to write up this Emacs interaction package. It
uses the Evergreen CLI to expose a better UI for submitting patches
and other Evergreen stuff too.


<a id="orgfb54d1c"></a>

## Features

-   Fuzzy completion of projects, tasks, variants, and aliases when creating a patch build.
-   Exposes most useful boolean flags as yes or no prompts.
-   Can generate automatic patch descriptions
-   Very configurable via customize, many things can be defaulted to avoid prompts


<a id="org3d8fa93"></a>

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


<a id="org63ba17c"></a>

## Usage

For now evergreen.el really only allows you to submit patch builds
to Evergreen. This is done via the command `M-x evergreen-patch <RET>`
when visiting any file in the repository you wanna patch against.

You will then be prompted for all required information to run a
patch build. All information that requires knowledge of the
`evergreen.yml` or information Evergreen has will be presented as a
fuzzy searchable completion list. If you have [helm](https://github.com/emacs-helm/helm), [ivy](https://github.com/abo-abo/swiper), or
`ido` completion turned on it will use those packages.


<a id="orgc13432a"></a>

### Configuration

This is best effort customization documentation. The best way to
discover available options for your installed version is always
via `M-x customize-group <RET> evergreen <RET>`.

1.  evergreen-projects

    List of evergreen projects used for completion in evergreen.el
    commands.  Defaults to the result of `evergreen list --projects`.

2.  evergreen-default-project

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

3.  evergreen-command-output-buffer

    Buffer to output evergreen stdout to. Defaults to `"*evergreen command output*"`.

4.  evergreen-binary-path evergreen

    Location of evergreen binary, assumes evergreen is in your $PATH.

5.  evergreen-assume-yes

    If not nil then the Evergreen CLI will not do any additional prompting.

6.  evergreen-browse-when-patching

    Whether or not to open a patch in your browser after creation.

7.  evergreen-finalize-when-patching

    If not nil schedule every patch right away.

8.  evergreen-never-finalize-when-patching

    If not nil never finalize and do not prompt for finalize when
    patching.  This option is ignored if
    `evergreen-finalize-when-patching` is non-nil.

9.  evergreen-browse-when-patching

    If not nil always open new patches in your web browser after submitting.

10. evergreen-never-browse-when-patching

    If not nil never browse and do not prompt for browse when patching.
    This option is ignored if `evergreen-browse-when-patching` is
    non-nil.

11. evergreen-generate-description

    If not nil generates patch descriptions of form `$git_branch_name: $git_head_commit_msg`.


<a id="org1293971"></a>

### Example Configuration

This is my configuration as it provides minimum prompts and
maximum automation. Put this in your Emacs init file after loading
evergreen.el:

    (setq 
      evergreen-generate-description t
      evergreen-finalize-when-patching t
      evergreen-browse-when-patching t
      evergreen-assume-yes t)

If you only work on a known set of projects and don't want the
full list of evergreen projects everytime then set the
`evergreen-projects` variable to shorten the list.

    (setq evergreen-projects '("mongodb-mongo-master" "toolchain-builder" "mongodb-mongo-v3.6" "mongodb-mongo-v4.0")
          evergreen-default-project "mongodb-mongo-master")


<a id="orgb468983"></a>

## Future Plans

I have a few more features I want to add to this but I'm prevented
by bugs in the Evergreen CLI.

-   Spawn host creation with fuzzy searchable list of available distros. [EVG-6101](https://jira.mongodb.org/browse/EVG-6101)
-   Spawn host integration with [TRAMP mode](https://www.emacswiki.org/emacs/TrampMode). [EVG-6102](https://jira.mongodb.org/browse/EVG-6102)
-   Spawn host management UI inside Emacs, something like `magit-log` or `list-processes`.


<a id="orgaebd803"></a>

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

