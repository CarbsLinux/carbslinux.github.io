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
