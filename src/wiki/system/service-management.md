Title: Service Management

Service Management
==================

Carbs Linux uses busybox-runit as the default system supervisor.


Enabling Services
-----------------

Services start immediately when you enable them, and run by default on boot.


    $ ln -s /etc/sv/acpid /var/service


Disabling a service
-------------------

    $ unlink /var/service/acpid


Starting a service
------------------

    $ sv start acpid


Stopping a service
------------------

    $ sv stop acpid


More
----

Runit is extremely flexible and simple. Refer to `sv`, `runsv`, `svc`, `runsvdir`
help outputs for more information.
