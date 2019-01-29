---
layout: post
title: Allegro Lisp Debugging [Trace]
categories: [lisp, debugging, programming, allegro, tracing]
tags: [debugging, programming, trace]
---
The topic of today's post is the trace method. While trace is part of the common lisp spec, it dosent go into much detail about the behavior of trace. As a result this post will be specific to the Allegro Common Lisp implementation of trace. 

For those of you who dont already know, trace provides a simple and easy way to get visibility into the inputs and outputs of your functions (and indirectly, how often they are called). As we go through the different examples I will using repl inputs and outputs expecting that the following code has been evaluated already.
* TOC
{:toc}
# Reference Code
Through the rest of this document there will be examples of REPL input and output which expect this code to have already been evaluated
{% highlight Lisp %}
(defparameter *do-trace* nil
  "this will be used in our trace examples")
  
(defparameter *do-break* nil
  "this will be used in our trace examples")

(defparameter *total-guest-count* 0 
  "the count of all people who decided to sign into our guest book")

(defparameter *guest-book* '() 
  "a list of all the names of people who have signed the book")

(defun register-guest (name)
  "sign a single guest into the book and incf the counter"
  (push name *guest-book*)
  (incf *total-guest-count*))

(defun register-group (&rest list-of-names)
  "register a group of people"
  (mapc 'register-guest list-of-names))
{% endhighlight %}

## Trace
One of the more disheartening things I've run into in lisp is that the language specifications mentioned a trace utility but didn't really dive into what it should do. As a result ever lisp dialect seems to have slightly different specifications. Here we are going to focus on the allegro CL Trace utility.

# What Is Trace?
Well if you made adjusted a function to print what arguments it was called with everytime it got called and then to print out again then you would have implemented a rudimentary trace on said function. The lisp trace utility gives us the ability to get information of when our function is called and what its called with WITHOUT having to manually adjust any code. All the visibility with none of the risk.

# Basic Traceing
Inorder to do a basic trace we simply call trace with our function name. 
{% highlight Lisp %}
cl-user(74): (trace register-guest)
(register-guest)
cl-user(75): (register-guest "Alonzo Church")
 0[5]: (register-guest "Alonzo Church")
 0[5]: returned 1
1
cl-user(76): 
{% endhighlight %}
And that's all there is too it. Registering a guest now prints out the input (very useful when the function you want to trace is nested inside of other logic) and prints out the return value! Once we solve our problem we can stop tracing using the #'untrace function. 
{% highlight lisp %}
cl-user(76): (untrace register-guest)
(register-guest)
cl-user(77): (register-guest "joshua")
2
{% endhighlight %}

We can trace multiple functions trivially by putting all the function names into a single call to trace.
{% highlight lisp %}
cl-user(84): (trace register-guest register-group)
(register-group register-guest)
cl-user(85): (register-group "person-one" "person-two" "person-three")
 0[5]: (register-group "person-one" "person-two" "person-three")
   1[5]: (register-guest "person-one")
   1[5]: returned 3
   1[5]: (register-guest "person-two")
   1[5]: returned 4
   1[5]: (register-guest "person-three")
   1[5]: returned 5
 0[5]: returned ("person-one" "person-two" "person-three")
("person-one" "person-two" "person-three")
{% endhighlight %}

Notice how when nested functions are traced the trace printouts come out nested accordly!

Trace called with no arguments will just return a list of all the functions currently being traced while untrace with no arguments will stop all functions from being traced. 

cl-user(86): (trace)
(register-guest register-group multiply)
cl-user(87): (untrace)
nil
cl-user(88): (trace)
nil


# Advanced Tracing
## intro
Allegro Common Lisp also allows for a number of options to be given as you describe what functions you want to trace. These options allow for more trailed printing and a finer control over when the tracing happens.

Inorder to use the advanced tracing options we have to use an extended syntax
{% highlight lisp %}
(trace (function-name &key ...))
{% endhighlight %}

## Condition
### Description
When this trace form evaluates we check if the given expression returns true, if it does the function is traced, if it doesnt the function is not traced!
Using the condition keyword (:condition) we pass in an expression, every time this function is called (where in the simple trace examples there would have been a printout) the expression is evaluated. if it is true then we trace that call, if its not we don't. 
### Example
{% highlight lisp %}
cl-user(89): (trace (register-guest :condition (evenp *total-guest-count*)))
(register-guest)
cl-user(90): (register-guest "person1")
6
cl-user(91): (register-guest "person2")
 0[5]: (register-guest "person2")
 0[5]: returned 7
7
{% endhighlight %}

### Thoughts
Part of how I would infer this to be used would be too tie the trace forms to some global value a \*trace-key-functions-p\* or something along those lines which could potentially give people the ability to easily turn on tracing for certain functions. I can kind of see this as a sort of scaffolding you could use while building up a project but definitiely not as a long term solution. I prefer to build up records to a logfile which can be used to audit the program, preferably by someone other than a trained developer. Beyond that I have a strong personal preference twoards a functional style that takes alot of advantage of lexical scoping so while this is helpful its not the solution i was hoping for. 
## The Break Family (:break-before, :break-after & :break-all) 
### Description
These three take a value which is evaluated every time the function in question is called. If the value evaluates to true then the program will break before the function is called, after the function is called or both (respectivly)
### Example
cl-user(48): (defun multiply (input)
               (* input (random 100)))
multiply
cl-user(49): (multiply 100)
800
cl-user(50): (let ((*do-trace* t))
               (multiply 9))
 0[5]: (multiply 9)
 0[5]: returned 81
81
cl-user(51): 




cl-user(59): (trace (multiply :condition *do-trace* :break-after *do-break*))
(multiply)
cl-user(60): (multiply 10)
70
cl-user(61): (let ((*do-break* t))
               (multiply 20))
1600
cl-user(62): (let ((*do-break* t))
               (multiply 20))
400
cl-user(63): (let ((*do-break* t) (*do-trace* t))
               (multiply 20))
 0[5]: (multiply 20)
Break: exiting multiply
cl-user(64): 

### Thoughts
Once you have a break you are tossed into the debugger with the full power of the repl to inspect values and, if you absolutely have too, change the environment in ways to test/induce/mock solutions for your issue. Once you are done playing around you can drop back into the program logic and have it continue as if nothing had ever happened. Again, the value you give it to check when it should break is not evaulated in any sort of scope that gives you access to function inputs so really its build for checking global variables (something im not terribly fond of)
## :print-before, :print-after, :print-all
### Description 
Printing out data each time that its printed out - before, after, or before and after - the function is called
### Example





(defparameter *counter* 0
"counter for the total number of visitors")

(defparameter *visitor-list* nil
   "a list containing all the names of visitors who gave their name")

(defun log-visitor (&optional visitor-name)
   (when visitor-name (push visitor-name *visitor-list*))
   (incf *counter*))

(defun welcome-group (list-of-visitor-names)
"adds a group of people"
(mapc 'log-visitor list-of-visitor-names))

(trace (log-visitor :print-before *counter* :print-after *visitor-list*))
(trace (log-visitor :inside))

### Thoughts 
## :inside, :not-inside
### Description 
These are my absolute favorite two options for trace, they opened the door for me to use trace in a more targeted and efficient manner. 
### Example
### Thoughts
These two options take in 
## :show-stack
### Description
takes in a number n and then everytime the function is called it prints out that many lines of a stack trace. 
### Example
### Thoughts
If you understand what the stack is (a blog post for another time) then printing out lines from the stack trace can give clear indications of the context associated with a specific trace print out

