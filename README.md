## How To
Load Verbose with ASDF or Quicklisp.

    (ql:quickload :verbose)

By default --if threading is available-- a new logging thread is already started and set up with a REPL-Faucet on the INFO level, that simply prints log messages from the INFO level and above to the standard output:

    (v:info :test "Hello world!")
    => LOG: 2014-04-01 13:32:49 [INFO ] <TEST>: Hello world!

Each logging statement expects a category and a datum. The simplest case for a datum is a format string:

    (v:info :test "2+2=~f" (+ 2 2))
    => LOG: 2014-04-01 13:33:20 [INFO ] <TEST>: 2+2=4.0

However, you can also pass in different objects, as well as extend the behaviour by adding methods to `log`:

    (v:info :test (make-condition 'error))
    (v:info :test 'simple-error :format-control "Hey!")
    (v:info :test #'bt:current-thread)

You can change the logging level of the REPL-Faucet easily. The available levels by default are `:FATAL` `:SEVERE` `:ERROR` `:WARN` `:INFO` `:DEBUG` `:TRACE`.

    (setf (v:repl-level) :DEBUG)

Using the category-tree-filter you can limit what kinds of categories are shown:

    (v:add-repl-category :foo.bar.*)
    (v:info :foo "Foo")

    (v:info :foo.bar.stuff "Foo")
    => LOG: 2014-04-01 13:51:03 [INFO ] <FOO.BAR.STUFF>: Foo

Verbose also allows you to pass as many categories as you want. This permits to use the categories as a form of tag, rather than a hierarchy.

    (v:info '(:system :server) "Starting up!")
    => LOG: 2014-04-01 13:51:52 [INFO ] <SYSTEM><SERVER>: Starting up!

Log message passing through the pipeline happens in a separate thread. If you create new pipe segments for your logging pipeline that need to access some form of shared variable, you can use `share`, which is SETFable. One `share` that is most likely of interest to everyone is saved under the symbol `*standard-output*`. Setting this anew is useful if you start a new REPL session and need to redirect logging to it.

    (setf (v:shared-instance '*standard-output*) *standard-output*)

Since this is a very frequent thing to do, especially when hooking into remote lisps, you can achieve the same as above by using `output-here`.

On the other hand, you can also suppress output completely or of certain categories using `with-muffled-logging`.

    (v:with-muffled-logging (:test :something.or.other)
      (v:info :test "A")
      (v:info :something.else "B")
      (v:info :something.or.other "C"))
    => LOG: 2015-03-03 17:36:41 [INFO ] <SOMETHING.ELSE>: B

The `with-muffled-logging` macro rebinds the `*muffled-categories*` by prepending the given categories. You can of course also set `*muffled-categories*` directly to achieve a global muffling. If you don't pass anything to `with-muffled-logging` or push `T` onto `*muffled-categories*`, all messages will be suppressed.

If you want to log to a file, you can either create your own custom file faucet, or use a preset one like the rotating-log-faucet:

    (v:add-pipe (make-instance 'v:rotating-log-faucet :file #p"~/verbose.log" :interval (v:make-cron-interval "* * * * *")))

Which creates a faucet that logs to a file with "verbose.log" suffix in your home directory, rotating every minute as per the CRON interval. In case you want to filter the message to a certain level or category instead, you can precede it by a filter:

    (v:add-pipe (make-instance 'piping:predicate-filter :predicate #'(lambda (message) (message-visible message :WARN)))
                (make-instance 'v:rotating-log-faucet :file #p"~/verbose.log" :interval (v:make-cron-interval "* * * * *")))

Using the piping constructs you can create complex logging systems or even change the pipeline on the fly. When you do, be aware that since the message passing happens in a separate thread, you need to acquire access to the pipeline first before modifying it:

    (v:with-controller-lock ()
    (piping:pipeline v:*global-controller*))

See the documentation of  for more information.

## Also See
* [Piping](http://shinmera.github.io/piping/) Dynamic pipelines
* [Dissect](http://shinmera.github.io/dissect/) Conditions and stack-frame analysis
