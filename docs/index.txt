Carbs Linux is an in-development Linux® distribution with a suckless mindset.
The system will consist of only the necessary programs to create a Linux
distribution.

Package Manager
---------------

Carbs Linux is built upon the world's best Linux distribution 
[KISS Linux](https://getkiss.org) and uses [kiss](https://github.com/kisslinux/kiss)
as its package manager, which has proven to be extremely flexible. I aim to move Carbs
to an even more suckless path.


Sane defaults
-------------

By default, Carbs Linux comes with busybox for coreutils, runit as the system 
supervisor, and sinit as the init, though nothing holds you from changing any
of them.


Links
-----

* Reddit - [/r/carbslinux](http://reddit.com/r/carbslinux)
* Matrix - [#carbs-linux:matrix.org](https://matrix.to/#/#carbs-linux:matrix.org)


News
----

[RSS Feed](/news.xml)

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


### Feb 14 2020

**IMPORTANT!** Carbs-init update to be released on 17th of February
will require manual intervention. I am holding this update back so
people can see it before they update their system (even though the
update will show the same message as this). The rationale for the
update is explained below the intervention.

    # There is an explanation for each init
    # You only need to follow the instructions
    # for your own init-system

    ## busybox-init
    # If you did not edit your inittab simply 
    # move inittab.new to inittab
    -> mv /etc/inittab.new /etc/inittab

    # If you did edit your inittab you can use
    # a simple sed command to remove the necessary lines
    -> sed -i '/getty/d;/runsvdir/d' /etc/inittab


    ## runit
    # same as busybox-init, if you did not edit
    # /etc/runit/2 move 2.new to 2
    -> mv /etc/runit/2.new /etc/runit/2

    # else
    -> vim /etc/runit/2
    # open your 2 file and remove the for loop containing
    # getty commands. If you are using runit, it is recommended
    # to comment out /etc/init/runit.boot

    ## sinit/minit
    # If you did not edit your {sinit-,}launch-services.boot
    # you can simply remove it.
    -> rm -f /etc/init/launch-services.boot /etc/init/sinit-launch-services.boot

    # This should leave you without any issues, and you can safely reboot.

Carbs-init update is to make sure init systems do not clash, and do
not have to use different files. This is a sensible update both for
the user user and for the maintainer side. 

To give an example, before this update busybox-init was managing getty
through `inittab`, runit was managing through `/etc/runit/2`, and
minit/sinit were launching from `/etc/init/launch-services.boot`. This
is a configuration nightmare for everyone, and since I am maintaining
and constantly testing those init providers, a huge nightmare for me. 
This is a Quality of Life update.


### Feb 13 2020

Runit is now released on the core repository! You can now replace
busybox-init with runit-init. You can also replace the busybox runit
utilities with the actual runit. This will reduce the dependency on
busybox greatly.

### Jan 28 2020

Web Server is now on a Carbs Linux server. It is not complete yet,
but I will be adding a little more for the intended functionality.
Downtimes can be expected.

UPDATE: ssl is now working.

UPDATE2: downloads are back again.


### Jan 22 2020

Expect downtime on Jan 24 2020 22:00 UTC as I will be migrating
this web server to my Carbs Linux server!


### Jan 15 2020

sinit has been taken to `core`, and is now officially supported


### Jan 13 2020

**Busybox update reqires manual intervention**

The new busybox package conflicts with the pre-update busybox-init. That's why
you should rebuild and install `busybox-init` before doing the update.

    # Do a kiss update first (but don't actually update it yet), or manually pull the git repository
    $ kiss u  # Cancel this operation after it asks for you to confirm the update
    
    # Install the busybox-init package
    $ kiss b busybox-init && kiss i busybox-init
    
    # Continue your update as you normally would.
    $ kiss u

I have moved runit and logging utilities from the busybox-init package to the busybox
package, which is the cause of these conflicts.

### Jan 10 2020

carbs-rootfs-20200110.tar.xz was [released](//dl.carbslinux.org/releases)


### Jan 09 2020

Happy new year, everyone! I am back after some long silence. I was unable to commit for a
while, but I have pulled some new updates from the KISS repository, added some of my own.
This week I will be changing/adding some things.

Important things are,

* Carbs-init was updated to 0.3.
* I will be adding an implementation of suckless.org's [sinit](//core.suckless.org/sinit) as `carbs-sinit`
* Carbs Repository was added to [repology.org](//repology.org/repository/carbs)
* A new rootfs tarball will be coming up tomorrow.


### Dec 18 2019

carbs-rootfs-20191218.tar.xz was [released](//dl.carbslinux.org/releases)


### Dec 17 2019

Installation instruction are now complete. A review would be greatly appreciated!


### Dec 15 2019

Carbs Linux Wiki is now available to be edited and updated by the users go
to the [wiki](//carbslinux.org/wiki) to obtain more information.

**February 2020 EDIT:** Changed link for wiki


### Dec 12 2019

Busybox and Busybox-init has been seperated into two different packages.
This is for adding standalone runit/runit-init to the repositories.


### Dec 10 2019

First alpha release was made. See the [downloads page](//dl.carbslinux.org/releases).

