Carbs Linux is an in-development Linux® distribution with a suckless mindset.
The base system will consist of only the necessary programs to create a Linux
distribution.

Package Manager
---------------

Carbs Linux uses its own fork of the [kiss package manager] which is an extremely
flexible package manager.


Small base
----------

By default, Carbs Linux comes with busybox for coreutils, and basic utilities
for building new software. The rootfs tarball is not bigger than 40MB.


Multiple Init Support
---------------------

Carbs Linux has support for multiple init systems and service supervisors.
In the main repository are

- `sinit`
- `busybox-init` (SysVinit clone)
- `runit-init`

for init systems, and

- `sysmgr`
- `busybox-runit`
- `runit`

for service supervisors. The [carbs-init] package is the collection of
init scripts that ensure the interoperatability of these init and service
systems, and make it easier for the user to switch to their preferred
combinations of system supervision.

Nothing holds you back, however, from ditching any of these and packaging
some other system supervision technique along with your own init scripts.


[carbs-init]: https://github.com/CarbsLinux/repository/tree/master/core/carbs-init
[kiss package manager]: https://github.com/CarbsLinux/kiss

Links
-----

* IRC    - `#carbslinux` on freenode
* Reddit - [/r/carbslinux](http://reddit.com/r/carbslinux)


News
----

[RSS Feed](/news.xml) | [See all news](/news.html)

### Apr 27 2020

A new rootfs tarball has been released! You can
see it on <https://dl.carbslinux.org/releases>!

**EDIT:** A new bug fix release has been made.

### Apr 10 2020

IRC channel can now be accessed from `#carbslinux` at freenode!

### Apr 06 2020

A new rootfs tarball has been released. See the
[downloads](https://dl.carbslinux.org) page

### Apr 05 2020

Carbs Linux repositories will be hosted only on Github. Commits will be
pushed there, and not the repository. You have until
May 4, 2020 Monday to switch your remotes to <https://github.com/CarbsLinux/repository>.
The git repository will continue to be served until then (without additional
commits).

You can switch your remote by executing the following command on your
repository directory.

    git remote set-url origin https://github.com/CarbsLinux/repository


### Feb 18 2020

A new tarball is released. It can be found on <https://dl.carbslinux.org>.

**edit:** I have removed the tarball because of a bootstrapping issue. 
I will be re-adding it later today.

**edit 2:** I have added a tarball (20200219) to reflect my recent
changes on Busybox.

