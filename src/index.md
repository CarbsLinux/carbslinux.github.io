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
* Reddit - [/r/carbslinux]
* [Mailing Lists]

[/r/carbslinux]: http://reddit.com/r/carbslinux
[Mailing Lists]: /mailing-lists.html

News
----

[RSS Feed](/news.xml) | [See all news](/news.html)

### May 19 2020

A rootfs tarball targeting the i686 architecture has
been released. It can be found on the [downloads page]

[downloads page]: https://dl.carbslinux.org/releases/i686

### May 10 2020

A GCC 10.1.0 change causes a kernel panic for kernels built
with GCC 10.1. This issue can be resolved by applying this
[patch] to your kernel sources.

[patch]: https://git.kernel.org/pub/scm/linux/kernel/git/tip/tip.git/patch/?id=f670269a42bfdd2c83a1118cc3d1b475547eac22

### Apr 27 2020

A new rootfs tarball has been released! You can
see it on <https://dl.carbslinux.org/releases>!

**EDIT:** A new bug fix release has been made.

### Apr 10 2020

IRC channel can now be accessed from `#carbslinux` at freenode!

### Apr 06 2020

A new rootfs tarball has been released. See the
[downloads](https://dl.carbslinux.org) page
