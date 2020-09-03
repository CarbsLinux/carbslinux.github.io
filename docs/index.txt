Carbs Linux is an in-development Linux® distribution with a suckless mindset.
The base system consists of only the necessary programs to create a Linux
distribution.

Package Manager
--------------------------------------------------------------------------------

Carbs Linux uses its own package manager [Carbs Packaging Tools], a POSIX shell
package manager forked from KISS with the tool-based approach of xbps.


Small base
--------------------------------------------------------------------------------

By default, Carbs Linux comes with busybox for coreutils, bearssl for its crypto
library, musl libc, and other basic utilities that are required for building
new software. The majority of the base packages are statically linked.


Multiple Init Support
--------------------------------------------------------------------------------

Carbs Linux has support for multiple init systems and service supervisors. In
the main repository are

- `sinit`
- `busybox-init` (SysVinit clone)
- `runit-init`

for init systems, and

- `sysmgr`
- `busybox-runit`
- `runit`

for service supervisors. The [carbs-init] package is the collection of init
scripts that ensure the interoperability of these init and service systems, and
make it easier for the user to switch to their preferred combinations of system
supervision.

Nothing holds you back, however, from ditching any of these and packaging some
other system supervision technique along with your own init scripts.


[carbs-init]: https://github.com/CarbsLinux/repository/tree/master/core/carbs-init
[Carbs Packaging Tools]: https://github.com/CarbsLinux/cpt

Links
--------------------------------------------------------------------------------

* IRC    - `#carbslinux` on freenode
* Reddit - [/r/carbslinux]
* [Mailing Lists]

[/r/carbslinux]: http://reddit.com/r/carbslinux
[Mailing Lists]: /mailing-lists.html

News
--------------------------------------------------------------------------------

[RSS Feed](/news.xml) | [See all news](/news.html)

Sep 03 2020
--------------------------------------------------------------------------------

The default TLS provider has been reverted to 'libressl'. System update will
replace bearssl dependencies with libressl. You may choose to keep or remove
bearssl after the update

Jul 27 2020
--------------------------------------------------------------------------------

Carbs Linux's fork of the `kiss` package manager has been renamed to
`Carbs Packaging Tools` due to huge amount of changes. Your latest `kiss`
update will handle the bootstrapping. You will need to rename your `KISS-*`
variables to `CPT-*`

Jun 17 2020
--------------------------------------------------------------------------------

A new rootfs tarball including the changes to the core is released and can be
found on the downloads page.

May 26 2020
--------------------------------------------------------------------------------

An initial version for binary package management is released.
It can be found on the [GitHub page].

[GitHub page]: https://github.com/CarbsLinux/kiss-bin

May 19 2020
--------------------------------------------------------------------------------

A rootfs tarball targeting the i686 architecture has
been released. It can be found on the [downloads page]

[downloads page]: https://dl.carbslinux.org/releases/i686
