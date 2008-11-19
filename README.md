
========
CONFIGGY
========

Configgy is a library for handling config files and logging for a scala
daemon. The idea is that it should be simple and straightforward, allowing
you to plug it in and get started quickly.

.. contents::


Why?
====

You would think config files would be a solved problem by now, but for some
reason they're not. Java is still trying to shoehorn "property files" for
the purpose (keys and values are strings, no blocks). Python is still trying
to use INI files (!), and Ruby invented a new text marshalling format (YAML)
and decided to abuse that for config files too. It's all kind of a sad mess.

Configuration doesn't have to be a hard problem. My experience shows that
only a few features are needed:

    - key/value pairs can be set
    - values can be bools, ints, strings, or lists of strings
    - sets of key/value pairs can be grouped into blocks and nested
      inside each other
    - some blocks can inherit default values from other blocks
    - values can be based (in some way) on previous values
    - files can be "imported" into each other

From the API side, it should be easy to find and manage key/value pairs
and blocks. A library should also be able to "subscribe" to a block and
be notified when values have been changed from somewhere else.

None of property files, INI files, or YAML files can do this. So here we
are.

At Danger, after several false starts, we ended up with a server config
file format very much like what I've implemented here (but it was more
complex and had a few major flaws). Other projects have created similar
formats. So I think this is an idea whose time has come.

Logging is tightly integrated with configuration, since config files are
usually used to setup logging, and some sort of logging is usually required at
the same time as a config file. So this library puts them together,
using the simple config format to setup logging, too -- allowing most of the
flexibility of java logging, but making it easy to do the common case of
creating a single logfile with one debug level.


Quick Start
===========

This is all you need to know to use the library::

    import net.lag.configgy.Configgy
    import net.lag.logging.Logger

    // load our config file and configure logfiles:
    Configgy.configure("/etc/pingd.conf")
    
    // read hostname and port:
    val config = Configgy.config
    val hostname = config.get("hostname", "localhost")
    val port = config.getInt("port", 3000)
    
    // log an error:
    val log = Logger.get
    log.error("Unable to listen on %s:%d!", hostname, port)
    
    // or an exception:
    try {
        ...
    } catch {
        case e: IOException => log.error(e, "IOException while doodling")
    }

The following config file will setup a logfile at debug level, that rolls
every night, and also sets a few simple config values::

    <log>
        filename = "/var/log/pingd.log"
        roll = "daily"
        level = "debug"
    </log>
    
    hostname = "pingd.example.com"
    port = 3000

The rest of this README just describes the config file format, the logging
options, and how to use the library in more detail.


Basic Use
=========

Config Files
------------

The config file format is simple and clean. It borrows from XML, but isn't
XML. It has a few advanced features, but limits itself to representing a
nested structure of key/value pairs. It is always in UTF-8.

Values are set like so::

    server_name = "pingd"

Key names follow the same rules as C (or Java or Python) symbols, except
they may also contain dashes ("-") if you like.

String values are in quotes, and have the usual escape codes, like ``\r``.
``\xHH`` and ``\uHHHH`` also work.

Int values don't need quotes, but are treated like strings and converted
only at the last minute.

Boolean values are turned "on" or "off" (or "true" or "false", with an
optional equal sign -- whatever makes you happy)::

    use_root_privs off
    drink_milk = false

String lists are just strings enclosed in brackets, separated by commas::

    states = ["California", "Tennessee", "Idaho"]

Groups of keys may be enclosed in an XML-like tag::

    <pingd>
        # set a high timeout for now.
        timeout = 30
    </pingd>

Nested values can be accessed from the API and from within the config file
as if they were in C structs, using dotted-name notation. So the above config is the same as::

    pingd.timeout = 30


Logging
-------

Logging is configured in a special ``<log>`` block. The main logging options
are described below.

``filename``
    the file to write log entries into (optional)
    
``level``
    the lowest severity log entry that should be written to the logfile
    (defaults to ``INFO``) (described below)

``console``
    ``true`` (``on``) if logs should be written to the stderr console

``syslog_host``
    hostname (or ``hostname:port``) to send syslog formatted log data to
    (optional)

``syslog_server_name``
    server name to attach to log messages when sending to a syslog (optional)

``roll``
    when the logfile should be rolled (described below)

Logging severities are:

=============  ================
Severity       Description
=============  ================
**FATAL**      the server is about to exit
**CRITICAL**   something happened that is so bad that someone should probably
               be paged
**ERROR**      an error occurred that may be limited in scope, but was
               user-visible
**WARNING**    a coder may want to be notified, but the error was probably not
               user-visible
**INFO**       normal informational logging
**DEBUG**      coder-level debugging information
**TRACE**      intensive debugging information
=============  ================

Logfile rolling policies are:

===========  =================
Name         Description
===========  =================
**never**    always use the same logfile
**hourly**   roll to a new logfile at the top of every hour
**daily**    roll to a new logfile at midnight every night
**sunday**   roll to a new logfile at midnight between saturday and sunday,
             once a week
===========  =================

You can omit a rolling policy, or use policy "never", to avoid rolling the
logfiles. For weekly logfile rolling, you may use any day of the week
("monday", "tuesday", etc), not just "sunday".

When a logfile is rolled, the current logfile is renamed to have the date (and
hour, if rolling hourly) attached, and a new one is started. So, for example,
``test.log`` may become ``test-20080425.log``, and ``test.log`` will be
reopened as a new file.

So, for example::

    <log>
        filename = "test.log"
        level = "warning"
        roll = "tuesday"
    </log>

creates a logfile ``test.log`` that captures log entries only at warning,
error, critical, or fatal levels. It's rolled once a week, at midnight on
Tuesday morning.

None of ``filename``, ``console``, or ``syslog_host`` are mutually exclusive,
so you can define any or all of those targets, to have log messages sent to
any possible combination of places.


Advanced Features
=================

There are a few features you may not use right away, but you'll usually
start wanting after the code matures a bit.


Config Files
------------

Previously-set config values can be included in new ones by using variable
substitution, with shell syntax. This also pulls in values from the
environment::

    server_home = "$(HOME)/servers/pingd"
    pid_file = "$(server_home)/pingd.pid"

Files can be included/imported into each other::

    include "$(server_home)/config/local.conf"

(The context is preserved, so if you are inside a block, the imported file
is injected inside that block too.) The default importer uses the local
filesystem. You can set your own importer to use other sources.

To set a value only if it hasn't been set earlier (possibly by an included
file), use shell syntax::

    server_home ?= "/opt/pingd"

Common config blocks can be inherited by later blocks::

    <daemon-base>
        timeout = 15
        chroot = "/opt/magic"
    </daemon-base>
    
    <pingd inherit="daemon-base">
        timeout = 30
    </ping>

The pingd block will use its own value of "timeout" (30), but will inherit
"chroot" from daemon-base. That is, "pingd.chroot" will be "/opt/magic".


Logging
-------

There are a handful of options to tune logging more directly:

``utc``
    ``on`` to log in UTC (previously known as GMT) time instead of local
    time (default: off)

``truncate``
    number of characters to allow in a single log line before eliding with
    "..." (default: 0 = never truncate)

``truncate_stack_traces``
    number of lines of a stack trace to show before eliding (default: 30)

``syslog_use_iso_date_format``
    set ``off`` to use old-style BSD date format in syslog messages
    (default: on)

``use_full_package_names``
    set ``on`` to use full package names in log lines ("net.lag.configgy")
    instead of the toplevel node ("configgy") (default: off)

``append``
    set ``off`` to create a new logfile each time the app starts (default:
    on, meaning to append to any existing logfile)

The logging options are usually set on the root node of java's "logging tree",
at "". You can set options or logging handlers at other nodes by putting them
in config blocks inside ``<log>``. For example::

    <log>
        filename = "test.log"
        level = "warning"
        utc = true
        
        <squelch_noisy>
            node = "com.example.libnoise"
            level = "critical"
        </squelch_noisy>
    </log>

The "com.example.libnoise" node will log at "critical" level (presumably to
silence a noisy library), while everything else will log at "warning" level.
You can put any of the logging options inside these blocks, including those
for logging to files or syslog nodes, so in this way you can create multiple
logfiles.

The extra options you can use in these inner blocks are:

``node``
    define the log node name (as a string)

``use_parents``
    whether to fall back to parent log-node configuration (java's 
    ``setUseParentHandlers``) (default: on)


Usage from within scala
=======================

You can build the scaladocs with the ant rule::

    $ ant docs

The main interface is ``Configgy``, which can be used to load a config file
and configure logging. The loaded config object is of type ``Config`` which is
just a ``ConfigMap`` with methods added for handling subscriptions.


Basic Config
------------

Usually you will just want to set or get config values.

Getting values can return an Option, or (if you provide a default value), a
direct String (or Int, or Boolean, etc)::

    scala> config.getString("name")
    res1: Option[String] = Some(Bender)
    
    scala> config("name")
    res2: String = Bender

    scala> config("name", "Frank")
    res3: String = Bender

    scala> config.getInt("age", 16)   
    res4: Int = 23

Setting values is similar::

    scala> config("name") = "Bender"
    scala> config("age") = 23

To access a nested attribute, you can either use a dotted key like::

    scala> config.getInt("pingd.port")
    res10: Option[Int] = Some(3000)

Or you can access the intermediate ``ConfigMap`` object::

    scala> config.getConfigMap("pingd")
    res1: Option[net.lag.configgy.ConfigMap] = Some({pingd: port="3000" })

    scala> config.getConfigMap("pingd").get.getInt("port")
    res2: Option[Int] = Some(3000)


Subscription API
----------------

Subscribing to an ``AttributesMap`` causes the subscriber to get called on
every change to that node. For monitored config nodes, changes happen in two
phases: validate and commit.

In validation, you can verify that the new settings for this config node are
reasonable and consistent, and if not, you can throw an exception to reject
the change.

If all subscribers successfully validate a change, the commit method is called
to notify subscribers that the change was successful. If any subscriber
refuses to validate a change, the code that called ``set`` should be prepared
to catch the validation exception.


Logging
-------

To access a logger from within a class or object, you can usually just use::

    import net.lag.logging.Logger
    private val log = Logger.get

This creates a ``Logger`` object that uses the current class or object's
package name as the logging node, so class "com.example.foo.Lamp" will log to
node "com.example.foo" (generally showing "foo" as the name in the logfile).
You can also get a logger explicitly by name::

    private val log = Logger.get("com.example.foo")

Logger objects wrap everything useful from "java.util.logging.Logger", as well
as adding some convenience methods::

    // log a string with sprintf conversion:
    log.info("Starting compaction on level %d...", level)
    
    try {
        ...
    } catch {
        // log an exception backtrace with the message:
        case x: IOException => log.info(x, "I/O exception: %s", x.getMessage)
    }

Each of the log levels (from "fatal" to "trace") has these two convenience
methods. You may also use ``log`` directly::

    log(Logger.DEBUG, "Logging %s at debug level.", name)

An advantage to using sprintf ("%s", etc) conversion, as opposed to::

    log(Logger.DEBUG, "Logging " + name + " at debug level.")

is that java & scala perform string concatenation at runtime, even if nothing
will be logged because the logfile isn't writing debug messages right now.
With sprintf parameters, the arguments are just bundled up and passed directly
to the logging level before formatting. If no log message would be written to
any file or device, then no formatting is done and the arguments are thrown
away. That makes it very inexpensive to include excessive debug logging which
can be turned off without recompiling and re-deploying.

The logging classes are done as an extension to the ``java.util.logging`` API,
and so if you want to use the java interface directly, you can. Each of the
java classes (Logger, Handler, Formatter) is just wrapped by a scala class
with a cleaner interface.


Contact
=======

Configgy was written by Robey Pointer <robeypointer@gmail.com> and is licensed
under the ISC license (included). Please write me with questions or comments,
but the software is provided as-is.
