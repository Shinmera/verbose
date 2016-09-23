## How To
Load Verbose with ASDF or Quicklisp.

    (ql:quickload :verbose)

Since I expect that logging will be a somewhat frequent need, the Verbose package is aliased to the single letter `v`. Under normal circumstances, Verbose starts itself up automatically after you've loaded it, so you can dive right in and log things.

    (v:log :info :intro "Hello!")
    
You will notice that there are three pieces of information to every log message you send over Verbose. The level at which the message is logged, the category, and finally the message itself. For each of the standard `v:*levels*`, a shorthand function `trace` `debug` `info` `warn` `error` `severe` `fatal` exists.

    (v:info :intro "Hello again.")

While the bulk of your logging will probably be made up by messages, you can log any kind of object. However, some classes receive special handling for convenience. Namely `string`s, `symbol`s, `function`s, and `condition`s. Strings in reality are format strings, and thus take an arbitrary number of extra arguments.

    (v:info :intro "We are ~:[good to go~;not ready yet~]." NIL)

Similarly, symbols are taken as condition or class names and the rest of the arguments are used as class-initargs. This allows you to conveniently construct custom objects for logging.

    (v:info :intro 'simple-error :format-control "Aw huckleberry!")

This will also show the special handling for conditions. When a condition object is logged, Verbose also immediately captures the stack at the current point and formats it as a backtrace when printed. You can control how much of a backtrace is shown, but you'll have to consider [Dissect](http://shinmera.github.io/dissect/)'s documentation for that. If you don't like this much info for conditions, you can however also entirely disable it by setting `*verbose-conditions*` to `NIL`.

Finally, when logging function objects they are simply called with the rest of the arguments.

    (v:info :intro #'1+ 1)

Verbose configures itself to only show messages from the info level and up to you at the REPL. You can change this behaviour by setting the `v:repl-level` to whichever the lowest level you want to see should be.

    (v:debug :intro "Snake, this is a stealth mission.")
    (setf (v:repl-level) :debug)
    (v:debug :intro "Hrnm... stealth...")

Aside from just being useful information, the categories can also be filtered. Normally all categories are allowed through, but you can limit this to only the categories that you want.

    (setf (v:repl-categories) (list :woods))
    (v:info :city "Lots of car noises")
    (v:info :woods "Birds chirping")

But categories can go beyond that still. Categories are actually trees, if you separate each branch by a dot in the name. You can then filter by branch and only allow subbranches through.

    (setf (v:repl-categories) (list :woods.cottage))
    (v:info :woods "Birds still chirping")
    (v:info :woods.cottage "Crashing sounds as a moose runs through")

As you can guess, you can have an arbitrary amount of categories that you want to see at the same time. If you want to see nothing at all, you set the categories to `NIL`. If you want to see everything, you set it to `T`. Juggling the category list can be a bit cumbersome like this though, so for convenience there are `add-repl-category` and `remove-repl-category` that each take a list of categories to add or remove at once.

    (setf (v:repl-categories) T)

Aside from being trees, a message can also carry more than one category at the same time. This allows you to tailor what you want to see even more selectively. You can also stash away the categories you want to use in a variable so that you can easily reference them without having to write them all out, if it happens to become excessive.

    (v:info '(:system :core.toolkit) "Deleting everything on disk...")
    (defvar *cats* '(:noodles :trophy :keyboard))
    (v:info *cats* "Meow.")

Before I get to the last point about categories, it is necessary to talk a bit about how Verbose works internally. In order to be extensible, a very generic backend is used that bases its ideas in pipelines. What we've used so far is one pipe segment that is automatically constructed for use at the REPL. It consists of a `level-filter`, a `category-tree-filter`, and a `repl-faucet`. The first one only accepts messages of the correct level, the second one only those that match the categories, and the last one is responsible for printing the messages out.

Being extensible like this it allows you to construct any form of pipeline you might want that updates, filters, and finally records the messages as you desire. There is one more final piece to Verbose's system to be aware of. When the system is thread-capable, Verbose launches a background thread that is used to process messages. Whenever you call a logging function it puts it onto a queue and notifies the thread to process it. This is necessary in order to make logging from separate threads simultaneously possible without everything ending up as mangled garbage.

If you don't need to use threads in your application or want to log directly from the current thread, bypassing the background thread you can bind or set `*process-locally*` to `T`. Otherwise if you want to use the thread, but sometimes need to ensure that messages have been processed, you can make use of `v:sync` which will block until all queued messages have been processed.

Finally we come to the last feature to do with categories. You can muffle categories locally, preventing messages with matching categories from being processed. This is an important difference to changing the REPL categories in that the message will never reach the pipeline to begin with. In the case of the REPL categories, other pipes may still process the message-- it just isn't printed to the REPL. You should only use muffling if you are absolutely sure that you do not want these messages to be processed at all.

    (v:with-muffled-logging (:cars)
      (v:info :pedestrians "Walking")
      (v:info :cars "Honking")
      (v:info '(:cars :pedestrians) "Exist"))

The `with-muffled-logging` simply binds `*muffled-categories*`, which is a list of muffled categories or `T` for everything, just like for the category filter.

When deploying an application the background thread might again cause problems as it is started by default and dumping an image with threads alive is not possible. Thus, you should call `stop-controller` before dumping and `start-controller` after resuming the image. Alternatively you can push `:verbose-no-init` onto `*features*` before loading Verbose and then run something similar to `(setf v:global-controller (v:make-standard-global-controller))` upon startup.

If you are accessing your Lisp image from multiple SLIME instances and the `*standard-output*` changes, you can use `v:output-here` to make it pick up the current standard output value again. Similarly, if you want to redirect the REPL output to another stream, you can do so by setting the shared value.

    (v:output-here (open #p"~/some-file.log" :direction :output))

However, if you really want to log to a file, you should see the following section for a proper solution.

## Customising Verbose
Especially in production environments you'll likely want to do something more than simply logging to the REPL. This is where the aforementioned pipeline system comes in. Verbose offers a couple of standard pipe segments that should be sufficient for most purposes. If you need more than that, see the next section.

* `stream-faucet`  
  Simply writes messages to a stream by `format-message`.
* `repl-faucet`  
  Inherits from `stream-faucet` and merely defaults the stream to `*standard-output*`.
* `file-faucet`  
  Inherits from `stream-faucet` and just manages opening/closing the stream to the file.
* `rotating-file-faucet`  
  Inherits from `file-faucet`, but rotates the log file to a new one in a specified interval. This is recommended for very long-running instances.
* `category-filter`  
  Only lets through messages of which one or more categories match the internal `categories` list.
* `category-tree-filter`  
  Inherits from `category-filter`, but filters tree-like by interpreting each category as a branch with dots separating the segments.
* `level-filter`  
  Only lets through messages which are on a level equal to or higher than the `filtered-level`.

What you'll most likely want to do is configure the controller to send messages through a new pipe that will end in a faucet.

    (v:define-pipe ()
      (v:rotating-file-faucet :template #p"verbose.log"))

This is the simplest you can get, and sufficient if you just want to dump literally everything to a file. If you want to filter things, you'll need to stick a filter segment before it.

    (v:define-pipe ()
      (v:level-filter :level :error)
      (v:rotating-file-faucet :file #p"err.log"))

You can add as many pipes you want, and stick as many filters before each as you like. For cheap "one-time use" filters that need a bit more work than the level-filter and category-filters offer, you can use piping's `predicate-filter`, which takes a predicate whose return value dictates whether the message should be let through.

    (v:define-pipe ()
      (piping:predicate-filter :predicate (lambda (m) (= 0 (local-time:timestamp-hour (v:message-time m)))))
      (v:rotating-file-faucet :file #p"midnight.log"))

As mentioned before, everything else will need custom segments, which are discussed in the next section.

## Extending Verbose
On the most basic level you might want to customise how messages are printed. Since I don't expect there to be any intermediate libraries basing on Verbose, and instead it being usually used in an end-user application, the simplest way to get that done is to simply override the `format-message` method.

    (defmethod v:format-message ((stream stream) (message v:message))
      (format stream "~&~a~%" (v:content message)))

If you don't like replacing code, you can instead subclass `message` and set it as the default class.

    (defclass my-message (v:message) ())
    
    (defmethod v:format-message ((stream stream) (message my-message))
      ..)
    
    (setf v:*default-message-class* 'my-message)

Doing the latter naturally also allows you to add more fields and other shenanigans. However, in order to actually fill those fields you will have to figure out shenanigans of your own, write log functions of your own that call out to `log-message` with the desired initargs, or replace/extend the standard methods on `log` to do what you need them to do.

Next you might want to create a filter pipe segment of your own that does more complex, or perhaps configurable filtering. In order to do this you will want to subclass `piping:filter` and add a method to `pass`. This method should return a message object --which may or may not be the same as the one it receives-- to be passed along further down the pipeline or `NIL` if it should get blocked.

    (defclass type-filter (piping:filter)
      ((type :initarg :type :initform T :accessor filter-type)))

    (defmethod piping:pass ((filter type-filter) thing)
      (when (typep thing (filter-type filter))
        thing))

After adding the filter and a faucet to the pipeline as illustrated above, you'll likely want to get easy access to it again at a later point in order to be able to change its fields. To do this it pays off to give it a name. You can do this by adding a `:name` field to your `define-pipe` expression.

    (v:define-pipe ()
      (type-filter :name 'type-filter)
      (v:repl-faucet))

After setting the name, you can retrieve your filter with `find-place`.

    (piping:find-place () 'type-filter)

These very basic functions is pretty much all the magic that Verbose uses internally to provide you convenience functions like `repl-level` and `make-standard-global-controller`.

Building custom faucets that output to a database or some other medium that isn't supported by files follows about the same procedure as making a custom filter. You subclass `piping:faucet` and add a method on `pass` that does what you need it to do. Then you instantiate your class and add that to the pipeline.

If the logging levels provided by default are not sufficient for you and you'd like to add some or change them, you merely have to manipulate the `*levels*` variable. It describes the priority as an integer with the associated symbol. Defining new ones is merely a matter of using `add-level`. If you also would like a convenience function along with the standard levels, you can use `define-level` for that.

    (v:define-level -20 :whisper)
    (v:whisper :somewhere "I'm a ghost.")
    (v:add-level 100 'scream)
    (v:log 'scream :somewhere "AAAA!!!")

Finally, if you need some more serious control over things and perhaps mess with the dispatch logic or something that I cannot really fathom right now, you might want to create your own controller type. A controller needs to implement a couple of methods in order to function. You will most likely be interested in changing `pass` and `controller-loop`. The first is responsible for scheduling the message onto the controller or directly executing it if that is not allowed (by `*process-locally*`) or not possible. The second is responsible for doing whatever the background thread should be doing.

In the default implementation, `pass` pushes the message onto a queue and then notifies a condition that should awake the controller thread. The controller thread then processes this queue by calling `pass` on its pipeline (an array) and each object in the queue. There's some more mechanics involved in order to reduce the potential impact for waiting that complicate this, but in essence that's all that happens.

Also relevant might be `start` and `stop` which are responsible for setting up and tearing down any background mechanisms, if necessary. In the standard implementation this takes care of creating and shutting down the thread.

## Also See
* [Piping](http://shinmera.github.io/piping/) Dynamic pipelines
* [Dissect](http://shinmera.github.io/dissect/) Stack-frame analysis
