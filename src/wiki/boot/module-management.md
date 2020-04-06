Title: Module Management

Module Management
=================

With **busybox-init**, a module can be loaded at boot by adding such a line to your `inittab`

    ::once:/bin/modprobe module-name


With **sinit**, a module can be loaded from your `/etc/rc.local` file. Add this to your file

    /bin/modprobe module-name


