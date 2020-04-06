Title: Switching to Sinit

Switching to Sinit
==================

Carbs Linux runs `busybox-init` by default. You can switch to
anything else you desire, but only busybox-init and sinit are
officially supported. Switching to sinit is extremely easy, you
can be up and running within minutes. The only "hard" requirement
is an environment you will be chrooting in. So, let's begin!


Preparing your environment
--------------------------

From your other environment, download the kiss-chroot utility 
and make it executable.

    $ wget https://dl.getkiss.org/kiss-chroot
    $ chmod +x kiss-chroot
    
    # From your environment start the chroot
    $ kiss-chroot /mnt

Removing busybox-init
---------------------

Now that we are in chroot environment, let's remove busybox-init.

    $ kiss r busybox-init


Installing and configuring sinit
--------------------------------

Build and install sinit

    $ kiss b sinit
    $ kiss i sinit

Open up your `/etc/init/rc.conf` now and uncomment
the variables regarding SINIT. You are ready now.

If you edited your inittab, make sure you reflect your changes
on the `/etc/init/sinit-launch-services.boot` file.


Finishing up
------------

If all of the above is done, simply exit the chroot and reboot.
Sinit should be functioning without any issues.
