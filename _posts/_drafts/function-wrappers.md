In our last post we covered trace and some of the features specific to allegro common lisp. In this post we dive into another allegro lisp debugging utility, function wrappers. Function wrappers are a fantastic tool that allow for more targeted debugging efforts. Infact the documentation actually states that the fwrapper is the basis for how they chose to implement the trace functionality! 
# Function Wrappers

## Intro
Function wrappers are effectivly a way to wrap your existing function with new code logic. You can wrap a function can be wrapped in multiple wrappers. 

taken directly from the allegro documentaiont 
''' executes at the point of call to the primary function, but its functionality surrounds the call to the actual primary function call '''
** we should probably make a picture for this section **

If nothing else they cover a weak spot in the allegro trace implementations which is the ability to conditionally report on a function call based on the functions arguments. but instead of giving us the conditional tracing function wrappers give us the ability to run any code we would want!

If you are familiar with other parts of lisp function wrappers are probably most like around methods conceptually. 

