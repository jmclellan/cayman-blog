---
layout: post
title: Function Names
categories: [lisp, allegro, function, debugging]
tags: [debuging, allegro, lisp]
---
Most of the time identifying the function you are interested in is a simple matter of knowing the symbol associated with that function. However there are a couple cases which there is no explicit symbol for a given function. In these cases we have to use an alternate function specification inorder to identitfy what we want. In terms of building out a code base I have had almost no useage for these. But when debugging I often find myself wanted to understand what the inputs to a setf function, or what the outputs to a particular method is. inorder to do this I have to pair the trace functionality with the ability to clearly articulate what function we want to follow 

# Function Identification
Most of the time identifying your function is trivial as there is a function name associated with it. 

## standard function
(trace my-function)
(trace (my-function :inside my-other-function))

## setf 
i need an example here for how to trace the result of a defun (setf ...)
(trace ((setf new-value ((t class-1)))))
(trace ((setf new-value ((t class-1))) :inside))

## flets + labels
(trace ((flet containing-function flet-name)))
(trace ((flet continaing-function flet-name) :inside outer-most-function))

(trace ((labels contianing-function labels-name)))
(trace ((labels contianing-function labels-name)) :inside outer-most-function)

## methods
### reader methods
### writer methods
### other methods
### can i trace all or a subset of them?
