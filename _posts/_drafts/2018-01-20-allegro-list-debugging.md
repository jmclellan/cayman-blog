---
layout: post
title: Allegro Lisp Debugging [Trace]
categories: [lisp, debugging, programming, allegro, tracing]
tags: [debugging, programming, trace]
---
# Debugging in Allegro Common Lisp

## the problem
Ive always felt like debugging gets a bad rep. True in theory if you got things right the first time debugging would never happen, and it can be frustrating trying to chase down a bug in code you thought would be bulletproof (or at least expected to run) But pragmatically I've found that debugging is so common that its worth getting good at. So i wanted to share some of the debugging tools that I've come to appreciate in the Allegro Common Lisp enviorment. 

## the most basic debugging
Im going go out on a limb and say that print statements are the most basic form of debugging. Taking the code in question and adding statements that will print out messages at certain points. Maby the messages just show that the logic and reached or passed a certain point, or maby it prints out descriptions of data at certain points inorder to give the developer a practical and hands on view of what is going on when their program gets into trouble. This certainly can get the job done and it works great for beginner programmers. When the function erroring is 5 lines long its really easy to keep in your head. However are you try to apply this approach to larger projects it starts to blatantly break down.



## Trace
One of the more disheartening things I've run into in lisp is that the language specifications mentioned a trace utility but didn't really dive into what it should do. As a result ever lisp dialect seems to have slightly different specifications. Here we are going to focus on the allegro CL Trace utility.

### what is trace?
Well if you made adjusted a function to print what arguments it was called with everytime it got called and then to print out again then you would have implemented a rudimentary trace on said function. The lisp trace utility gives us the ability to get information of when our function is called and what its called with WITHOUT having to manually adjust any code. All the visibility with none of the risk.

#### basic trace examples

{% highlight Lisp  linenos %}
     (trace example-function)
 {% endhighlight %}

And that's all there is too it. calling our mystery function now will result in a REPL printout onlime initiation and on return giving us lots of visibility into the situation. 

tracing multiple functions is trivial with the trace utility and shows another great feature - trace will actually indent its printouts so that you can see how the functions you are calling relate to oneanother

{% highlight lisp linenos %}
     (trace function-one function-two)
 {% endhighlight %}

calling trace on a function updates the list of traced functions in our repl right now we have three functions being traced (example-function, function-one, and function-two) and if you want to clean up or you want to stop tracing one of the functions specifically you can use the untrace function!
{% highlight lisp linenos %}
     (untrace mystery-function) ;mystery function no longer traced
     (untrace) ; no more functions traced
 {% endhighlight %}


##### :condition
trace the function if the expression given evaluates to true (I belive that this is checked once at the beginning?)
###### Description
When this trace form evaluates we check if the given expression returns true, if it does the function is traced, if it doesnt the function is not traced!
###### Example
###### Thoughts
Part of how I would infer this to be used would be too tie the trace forms to some global value a \*trace-key-functions-p\* or something along those lines which could potentially give people the ability to easily turn on tracing for certain functions. I can kind of see this as a sort of scaffolding you could use while building up a project but definitiely not as a long term solution. I prefer to build up records to a logfile which can be used to audit the program, preferably by someone other than a trained developer. 
##### :break-before, :break-after, :break-all 
###### Description
These three take a value which is evaluated every time the function in question is called. If the value evaluates to true then the program will break before the function is called, after the function is called or both (respectivly)
###### Example
###### Thoughts
Once you have a break you are tossed into the debugger with the full power of the repl to inspect values and, if you absolutely have too, change the environment in ways to test/induce/mock solutions for your issue. Once you are done playing around you can drop back into the program logic and have it continue as if nothing had ever happened. Again, the value you give it to check when it should break is not evaulated in any sort of scope that gives you access to function inputs so really its build for checking global variables (something im not terribly fond of)
##### :print-before, :print-after, :print-all
###### Description 
Printing out data each time that its printed out - before, after, or before and after - the function is called
###### Example
###### Thoughts 
##### :inside, :not-inside
###### Description 
These are my absolute favorite two options for trace, they opened the door for me to use trace in a more targeted and efficient manner. 
###### Example
###### Thoughts
These two options take in 
##### :show-stack
###### Description
takes in a number n and then everytime the function is called it prints out that many lines of a stack trace. 
###### Example
###### Thoughts
If you understand what the stack is (a blog post for another time) then printing out lines from the stack trace can give clear indications of the context associated with a specific trace print out

